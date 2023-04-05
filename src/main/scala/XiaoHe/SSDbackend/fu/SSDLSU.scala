package XiaoHe.SSDbackend.fu

import XiaoHe.SSDbackend._
import XiaoHe._
import _root_.utils._
import bus.simplebus.{SimpleBusCmd, SimpleBusReqBundle, SimpleBusUC}
import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils
import XiaoHe.SSDbackend.fu.LoadPipe.SSDLoadPipe

object LSUOpType { //TODO: refactor LSU fuop
  def lb   = "b0000000".U
  def lh   = "b0000001".U
  def lw   = "b0000010".U
  def ld   = "b0000011".U
  def lbu  = "b0000100".U
  def lhu  = "b0000101".U
  def lwu  = "b0000110".U
  def sb   = "b0001000".U
  def sh   = "b0001001".U
  def sw   = "b0001010".U
  def sd   = "b0001011".U

  def lr      = "b0100000".U
  def sc      = "b0100001".U
  def amoswap = "b0100010".U
  def amoadd  = "b1100011".U
  def amoxor  = "b0100100".U
  def amoand  = "b0100101".U
  def amoor   = "b0100110".U
  def amomin  = "b0110111".U
  def amomax  = "b0110000".U
  def amominu = "b0110001".U
  def amomaxu = "b0110010".U

  def isAdd(func: UInt) = func(6)
  def isAtom(func: UInt): Bool = func(5)
  def isStore(func: UInt): Bool = func(3)
  def isLoad(func: UInt): Bool = !isStore(func) & !isAtom(func)
  def isLR(func: UInt): Bool = func === lr
  def isSC(func: UInt): Bool = func === sc
  def isAMO(func: UInt): Bool = isAtom(func) && !isLR(func) && !isSC(func)

  def needMemRead(func: UInt): Bool = isLoad(func) || isAMO(func) || isLR(func)
  def needMemWrite(func: UInt): Bool = isStore(func) || isAMO(func) || isSC(func)

  def atomW = "010".U
  def atomD = "011".U
}

class SSDLSUIO extends FunctionUnitIO{
  val offset = Input(UInt(XLEN.W))
  val memStall = Output(Bool())
  val invalid = Input(Vec(3,Bool()))
  //??? for what
  val LoadReady = Output(Bool())
  val StoreReady = Output(Bool())
  
  val dmem = new SimpleBusUC(addrBits = VAddrBits)
  val storeBypassCtrl = Flipped((new LSUPipeBypassCtrl).storeBypassCtrlE2)
  val storeBypassPort = Flipped((new StorePipeBypassPort).storeBypassPortE2)
  val isMMIO = Output(Bool())
}

class StoreHitCtrl extends Bundle{
  val hit = Output(Bool())
  val hitData = Output(UInt(64.W))
  val hitMask = Output(UInt(8.W))
}
class storePipeEntry extends StoreBufferEntry{
  val isStore             = Output(Bool())
  val isCacheStore        = Output(Bool())
  val isMMIOStore         = Output(Bool())
  val isMMIOStoreInvalid  = Output(Bool())
  val isMMIO              = Output(Bool())  //load or store
  val func                = Output(UInt(7.W))
  val pc                  = Output(UInt(VAddrBits.W))
  val offset              = Output(UInt(64.W))
  val rs1                 = Output(UInt(64.W))
  val mergeAddr           = Output(Bool())
}

class SSDLSU extends  NutCoreModule with HasStoreBufferConst{
  val io = IO(new SSDLSUIO)
  val (valid, src1, src2, func, invalid, offset) = (io.in.valid, io.in.bits.src1, io.in.bits.src2, io.in.bits.func, io.invalid, io.offset)
  def access(valid: Bool, src1: UInt, src2: UInt, func: UInt, offset: UInt): UInt = {
    this.valid := valid
    this.src1 := src1
    this.src2 := src2
    this.func := func
    this.offset := offset
    io.out.bits
  }
  def genWmask(addr: UInt, sizeEncode: UInt): UInt = {
    LookupTree(sizeEncode, List(
      "b00".U -> 0x1.U, //0001 << addr(2:0)
      "b01".U -> 0x3.U, //0011
      "b10".U -> 0xf.U, //1111
      "b11".U -> 0xff.U //11111111
    )) << addr(2, 0)
  }
  def genWdata(data: UInt, sizeEncode: UInt): UInt = {
    LookupTree(sizeEncode, List(
      "b00".U -> Fill(8, data(7, 0)),
      "b01".U -> Fill(4, data(15, 0)),
      "b10".U -> Fill(2, data(31, 0)),
      "b11".U -> data
    ))
  }

  val addr = src1 + offset
  val wdata = src2
  val size = func(1,0)
  val isStore = valid && LSUOpType.isStore(func)
  val isLoad = valid && LSUOpType.isLoad(func)

  val reqAddr  = addr
  val reqWdata = genWdata(wdata, size)
  val reqWmask = genWmask(addr, size)

  val loadPipe = Module(new SSDLoadPipe)
  //store pipeline
  /*
  ||----------EX2------------||
  ||--------memStage3--------|| register <-  invalid(0)
  ||----------EX3------------||
  ||--------memStage4--------|| register <-  invalid(1)
  ||----------EX4------------||
  ||-------storeBuffer-------|| register <-  invalid(2)
   */
  val lsuPipeIn = Wire(Vec(2,Flipped(Decoupled(new storePipeEntry))))
  val lsuPipeOut = Wire(Vec(2,Decoupled(new storePipeEntry)))
  val lsuPipeStage3 = Module(new stallPointConnect(new storePipeEntry)).suggestName("memStage3")
  val lsuPipeStage4 = Module(new stallPointConnect(new storePipeEntry)).suggestName("memStage4")
  //cache signal
  // storeCacheIn \ loadCacheIn --> cacheIn
  val cacheIn = Wire(Decoupled(new SimpleBusReqBundle))
  val storeCacheIn = Wire(Decoupled(new SimpleBusReqBundle))
  val loadCacheIn = Wire(Decoupled(new SimpleBusReqBundle))
//  dontTouch(cacheIn)
//  dontTouch(storeCacheIn)
//  dontTouch(loadCacheIn)
  io.dmem.req <> cacheIn
  //store buffer
  val storeBuffer = Module(new StoreBuffer)
  //MMIO & OutBuffer
  val outBuffer = Module(new Queue(new StoreBufferEntry, entries = 1))
  val MMIOStorePkt = Wire(Decoupled(new StoreBufferEntry))
  val isMMIOStore = AddressSpace.isMMIO(addr) && isStore
  val isMMIO = AddressSpace.isMMIO(addr)
  val MMIOStorePending = (lsuPipeStage4.right.valid && lsuPipeStage4.right.bits.isMMIOStore) || outBuffer.io.deq.valid

  outBuffer.io.enq.valid := lsuPipeStage4.right.valid && lsuPipeStage4.right.bits.isMMIOStore && !invalid(2)
  outBuffer.io.enq.bits.data := lsuPipeStage4.right.bits.data
  outBuffer.io.enq.bits.mask := lsuPipeStage4.right.bits.mask
  outBuffer.io.enq.bits.size := lsuPipeStage4.right.bits.size
  outBuffer.io.enq.bits.paddr := lsuPipeStage4.right.bits.paddr
  outBuffer.io.deq.ready := MMIOStorePkt.ready
  MMIOStorePkt.bits := outBuffer.io.deq.bits
  MMIOStorePkt.valid := outBuffer.io.deq.valid
  MMIOStorePkt.ready := false.B
  val outBufferFire = outBuffer.io.deq.fire()
  BoringUtils.addSource(MMIOStorePkt.valid,"MMIOStorePktValid")
  BoringUtils.addSource(MMIOStorePkt.bits,"MMIOStorePktBits")
  BoringUtils.addSink(MMIOStorePkt.ready,"MMIOStorePktReady")
  BoringUtils.addSource(MMIOStorePending,"MMIOStorePending")
  BoringUtils.addSource(outBufferFire,"outBufferFire")

  //stall signal
  val cacheStall = WireInit(false.B)
  BoringUtils.addSink(cacheStall,"cacheStall")
  val  bufferFullStall = (storeBuffer.io.isAlmostFull && lsuPipeOut(1).bits.isStore && lsuPipeOut(1).valid) || storeBuffer.io.isFull  //when almost full, still can store one
  BoringUtils.addSource(bufferFullStall,"bufferFullStall")
  
  val pc = WireInit(0.U(VAddrBits.W))  //for LSU debug
  BoringUtils.addSink(pc,"lsuPC")  
  val loads2valid = WireInit(false.B)
  BoringUtils.addSink(loads2valid, "loads2valid")  
  io.memStall := cacheStall && (isLoad || loads2valid) || bufferFullStall

  lsuPipeIn(0).valid := isStore  
  lsuPipeIn(0).bits.isStore := isStore
  lsuPipeIn(0).bits.paddr := reqAddr(PAddrBits-1,0)
  lsuPipeIn(0).bits.offset := offset
  lsuPipeIn(0).bits.rs1 := src1
  lsuPipeIn(0).bits.mergeAddr := isStore && io.storeBypassCtrl.asUInt.orR
  lsuPipeIn(0).bits.data := reqWdata
  lsuPipeIn(0).bits.size := size
  lsuPipeIn(0).bits.mask := reqWmask
  lsuPipeIn(0).bits.func := func
  lsuPipeIn(0).bits.isCacheStore := cacheIn.fire() && cacheIn.bits.cmd === SimpleBusCmd.write
  lsuPipeIn(0).bits.pc := pc
  lsuPipeIn(0).bits.isMMIOStore := isMMIOStore
  lsuPipeIn(0).bits.isMMIOStoreInvalid := isMMIOStore
  lsuPipeIn(0).bits.isMMIO := isMMIO
  lsuPipeOut(1).ready := !bufferFullStall
  lsuPipeStage3.io.isStall := false.B
  lsuPipeStage4.io.isStall := io.memStall //There is only one stall point in LSU


  loadPipe.io.in.bits.src1 := src1
  loadPipe.io.in.bits.offset := offset
  loadPipe.io.in.bits.func := func
  loadPipe.io.in.valid := io.in.valid && isLoad
  loadPipe.io.storePipeE3.bits <> lsuPipeOut(0).bits
  loadPipe.io.storePipeE4.bits <> lsuPipeOut(1).bits
  loadPipe.io.storePipeE3.valid <> lsuPipeOut(0).valid
  loadPipe.io.storePipeE4.valid <> lsuPipeOut(1).valid
  loadPipe.io.storebuffer <> storeBuffer.io.snapshot
  loadPipe.io.writePtr <> storeBuffer.io.writePtr
  loadPipe.io.readPtr <> storeBuffer.io.readPtr
  loadPipe.io.dmem.req <> loadCacheIn
  loadPipe.io.dmem.resp <> io.dmem.resp
  loadPipe.io.invalid <> invalid(0)
  loadPipe.io.stall := io.memStall
  loadPipe.io.pc := pc


  io.out <> loadPipe.io.out  

  io.LoadReady := cacheIn.ready && !storeBuffer.io.isAlmostFull
  io.StoreReady := lsuPipeIn(0).ready

  for(i <- 1 to 1){
    lsuPipeIn(i).bits := lsuPipeOut(i-1).bits
    lsuPipeIn(i).valid := lsuPipeOut(i-1).valid
    lsuPipeOut(i-1).ready := lsuPipeIn(i).ready
  }

  //store pipeline Rs bypass 
  val storeAddr = reqAddr
  val storeMask = reqWmask
  val bypassEnaE2 = io.storeBypassCtrl.asUInt.orR && lsuPipeIn(0).bits.isStore
  val bypassDataE2 = PriorityMux(io.storeBypassCtrl,io.storeBypassPort)
  val bypassWdata = genWdata(bypassDataE2,lsuPipeIn(0).bits.func(1,0))
  lsuPipeIn(0).bits.data := Mux(bypassEnaE2,bypassWdata,reqWdata)

  val lsuPipeList0 = List(lsuPipeStage3,lsuPipeStage4)
  val pipeIndexList0 = List(0,1)
  (lsuPipeList0 zip pipeIndexList0).foreach{ case(a,b) =>
    a.io.left <> lsuPipeIn(b)
    a.io.right <> lsuPipeOut(b)
    a.io.rightOutFire <> lsuPipeOut(b).fire()
    a.io.inValid <> invalid(b)
  }
  //store buffer
  //load/store issue ctrl (issue to DCache)
  storeCacheIn.bits.apply(
    addr = storeBuffer.io.out.bits.paddr,
    size = storeBuffer.io.out.bits.size,
    wdata = storeBuffer.io.out.bits.data,
    wmask = storeBuffer.io.out.bits.mask,
    cmd = SimpleBusCmd.write
  )

  storeCacheIn.valid := storeBuffer.io.out.valid
  storeBuffer.io.out.ready := storeCacheIn.ready



  val cacheInArbiter0 = Module(new Arbiter((new SimpleBusReqBundle),2)) //store has higher priority,and store ready is driven by arbiter0, load ready is driven by arbiter1
  val cacheInArbiter1 = Module(new Arbiter((new SimpleBusReqBundle),2))
  cacheInArbiter0.io.in(0) <> storeCacheIn
  cacheInArbiter0.io.in(1) <> loadCacheIn
  cacheInArbiter1.io.in(0) <> loadCacheIn
  cacheInArbiter1.io.in(1) <> storeCacheIn

  BoringUtils.addSource(loadCacheIn.valid && storeCacheIn.valid,"LSU_load_store_confilct")

  storeCacheIn.ready := Mux(storeBuffer.io.isAlmostFull || storeBuffer.io.isFull, cacheInArbiter0.io.in(0).ready, cacheInArbiter1.io.in(1).ready)

  cacheIn.bits :=  Mux(storeBuffer.io.isAlmostFull || storeBuffer.io.isFull,cacheInArbiter0.io.out.bits,cacheInArbiter1.io.out.bits)
  cacheIn.valid :=  Mux(storeBuffer.io.isAlmostFull || storeBuffer.io.isFull,cacheInArbiter0.io.out.valid,cacheInArbiter1.io.out.valid)
  cacheInArbiter0.io.out.ready := cacheIn.ready
  cacheInArbiter1.io.out.ready := cacheIn.ready




  io.in.ready := lsuPipeIn(0).ready || loadCacheIn.ready
//  dontTouch(io.out.valid)
  io.isMMIO := lsuPipeStage3.right.bits.isMMIO

  // val outLatch = RegEnable(out, io.out.valid && !dmemFireLatch )
  // val outValidLatch = RegEnable(io.out.valid, io.out.valid && !dmemFireLatch )


  //store buffer snapshit
  storeBuffer.io.in.valid := lsuPipeStage4.io.right.valid && lsuPipeStage4.io.right.bits.isStore && !lsuPipeStage4.io.right.bits.isMMIOStore && !invalid(2)
  storeBuffer.io.in.bits.paddr := lsuPipeStage4.io.right.bits.paddr
  storeBuffer.io.in.bits.data := lsuPipeStage4.io.right.bits.data
  storeBuffer.io.in.bits.mask := lsuPipeStage4.io.right.bits.mask
  storeBuffer.io.in.bits.size := lsuPipeStage4.io.right.bits.size
  //mydebug
  val loadCond = lsuPipeOut(0).fire() && !lsuPipeOut(0).bits.isStore && !lsuPipeOut(0).bits.isCacheStore && io.out.valid
  val storeCond = lsuPipeOut(0).fire() && lsuPipeOut(0).bits.isStore
//  dontTouch(loadCond)
//  dontTouch(storeCond)
  val tag = lsuPipeOut(0).bits.paddr === "h80022b70".U
//  dontTouch(tag)

  val sdtag = (io.dmem.req.valid && (io.dmem.req.bits.addr === "hfc011718".U))
  dontTouch(sdtag)

  if(SSDCoreConfig().EnableLSUDebug){
  myDebug(loadCond, "Load  addr:%x, mask:%b, data:%x, at PC: %x\n",lsuPipeOut(0).bits.paddr,lsuPipeOut(0).bits.mask,io.out.bits,lsuPipeOut(0).bits.pc)
  myDebug(storeCond,"Store addr:%x, mask:%b, data:%x, at PC: %x\n",lsuPipeOut(0).bits.paddr,lsuPipeOut(0).bits.mask,lsuPipeOut(0).bits.data,lsuPipeOut(0).bits.pc)
}
}
