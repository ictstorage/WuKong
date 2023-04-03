/***************************************************************************************
 * Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
 * Copyright (c) 2020-2021 Peng Cheng Laboratory
 *
 * XiangShan is licensed under Mulan PSL v2.
 * You can use this software according to the terms and conditions of the Mulan PSL v2.
 * You may obtain a copy of Mulan PSL v2 at:
 *          http://license.coscl.org.cn/MulanPSL2
 *
 * THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
 * EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
 * MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
 *
 * See the Mulan PSL v2 for more details.
 ***************************************************************************************/

/**************************************************************************************
 * Copyright (c) 2020 Institute of Computing Technology, CAS
 * Copyright (c) 2020 University of Chinese Academy of Sciences
 *
 * NutShell is licensed under Mulan PSL v2.
 * You can use this software according to the terms and conditions of the Mulan PSL v2.
 * You may obtain a copy of Mulan PSL v2 at:
 *             http://license.coscl.org.cn/MulanPSL2
 *
 * THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND, EITHER
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT, MERCHANTABILITY OR
 * FIT FOR A PARTICULAR PURPOSE.
 *
 * See the Mulan PSL v2 for more details.
 ***************************************************************************************/

package utils

import XiaoHe.SSDbackend.{CacheBundle, CacheModule, SSDCacheConfig}
import chisel3._
import chisel3.util._


class BankedSRAMBundleA(val set: Int) extends Bundle {
  val setIdx = Output(UInt(log2Up(set).W))

  def apply(setIdx: UInt) = {
    this.setIdx := setIdx
    this
  }
}

class BankedSRAMBundleAW[T <: Data](private val gen: T, set: Int, val way: Int = 1) extends SRAMBundleA(set) {
  val data = Output(Vec(way, gen))
  val waymask = if (way > 1) Some(Output(UInt(way.W))) else None

  def apply(data: Vec[T], setIdx: UInt, waymask: UInt): BankedSRAMBundleAW[T] = {
    super.apply(setIdx)
    this.data := data
    this.waymask.map(_ := waymask)
    this
  }
  // this could only be used when waymask is onehot or nway is 1
  def apply(data: T, setIdx: UInt, waymask: UInt): BankedSRAMBundleAW[T] = {
    apply(VecInit(Seq.fill(way)(data)), setIdx, waymask)
    this
  }
}

class BankedSRAMBundleR[T <: Data](private val gen: T, val way: Int = 1) extends Bundle {
  val data = Output(Vec(way, gen))
}

class BankedSRAMReadBus[T <: Data](private val gen: T, val set: Int, val way: Int = 1) extends Bundle {
  val req = Decoupled(new SRAMBundleA(set))
  val resp = Flipped(new SRAMBundleR(gen, way))

  def apply(valid: Bool, setIdx: UInt) = {
    this.req.bits.apply(setIdx)
    this.req.valid := valid
    this
  }
}

class BankedSRAMWriteBus[T <: Data](private val gen: T, val set: Int, val way: Int = 1) extends Bundle {
  val req = Decoupled(new BankedSRAMBundleAW(gen, set, way))

  def apply(valid: Bool, data: Vec[T], setIdx: UInt, waymask: UInt): BankedSRAMWriteBus[T] = {
    this.req.bits.apply(data = data, setIdx = setIdx, waymask = waymask)
    this.req.valid := valid
    this
  }
  def apply(valid: Bool, data: T, setIdx: UInt, waymask: UInt): BankedSRAMWriteBus[T] = {
    apply(valid, VecInit(Seq.fill(way)(data)), setIdx, waymask)
    this
  }
}



class L1BankedDataReadReq(implicit val cacheConfig: SSDCacheConfig) extends CacheBundle {
  val way_en = Bits(DCacheWays.W)
  val addr = Bits(PAddrBits.W)
}

class L1BankedDataReadLineReq(implicit val p: SSDCacheConfig) extends L1BankedDataReadReq {
  val rmask = Bits(DCacheBanks.W)
}

// Now, we can write a cache-block in a single cycle
class L1BankedDataWriteReq(implicit val p: SSDCacheConfig) extends L1BankedDataReadReq {
  val wmask = Bits(DCacheBanks.W)
  val data = Vec(DCacheBanks, Bits(LineSize.W))
}

class L1BankedDataReadResult(implicit val cacheConfig: SSDCacheConfig) extends CacheBundle {
  // you can choose which bank to read to save power
  val raw_data = Bits(LineSize.W)
//  val error = Bool() // slow to generate, use it with care
}

//                     Banked DCache Data
// -----------------------------------------------------------------
// | Bank0 | Bank1 | Bank2 | Bank3 | Bank4 | Bank5 | Bank6 | Bank7 |
// -----------------------------------------------------------------
// | Way0  | Way0  | Way0  | Way0  | Way0  | Way0  | Way0  | Way0  |
// | Way1  | Way1  | Way1  | Way1  | Way1  | Way1  | Way1  | Way1  |
// | ....  | ....  | ....  | ....  | ....  | ....  | ....  | ....  |
// -----------------------------------------------------------------
class BankedSRAMTemplate[T <: Data](gen: T, set: Int, way: Int = 1,
                              shouldReset: Boolean = false, holdRead: Boolean = false, singlePort: Boolean = false, bypassWrite: Boolean = false) extends Module {
  val io = IO(new Bundle {
    val r = Flipped(new BankedSRAMReadBus(gen, set, way))
    val w = Flipped(new BankedSRAMWriteBus(gen, set, way))
  })

  val wordType = UInt(gen.getWidth.W)
  val array = SyncReadMem(set, Vec(way, wordType))
  val (resetState, resetSet) = (WireInit(false.B), WireInit(0.U))

  if (shouldReset) {
    val _resetState = RegInit(true.B)
    val (_resetSet, resetFinish) = Counter(_resetState, set)
    when (resetFinish) { _resetState := false.B }

    resetState := _resetState
    resetSet := _resetSet
  }

  val (ren, wen) = (io.r.req.valid, io.w.req.valid || resetState)
  val realRen = (if (singlePort) ren && !wen else ren)

  val setIdx = Mux(resetState, resetSet, io.w.req.bits.setIdx)
  val wdata = VecInit(Mux(resetState, 0.U.asTypeOf(Vec(way, gen)), io.w.req.bits.data).map(_.asTypeOf(wordType)))
  val waymask = Mux(resetState, Fill(way, "b1".U), io.w.req.bits.waymask.getOrElse("b1".U))
  when (wen) { array.write(setIdx, wdata, waymask.asBools) }

  val raw_rdata = array.read(io.r.req.bits.setIdx, realRen)

  // bypass for dual-port SRAMs
  require(!bypassWrite || bypassWrite && !singlePort)
  def need_bypass(wen: Bool, waddr: UInt, wmask: UInt, ren: Bool, raddr: UInt) : UInt = {
    val need_check = RegNext(ren && wen)
    val waddr_reg = RegNext(waddr)
    val raddr_reg = RegNext(raddr)
    require(wmask.getWidth == way)
    val bypass = Fill(way, need_check && waddr_reg === raddr_reg) & RegNext(wmask)
    bypass.asTypeOf(UInt(way.W))
  }
  val bypass_wdata = if (bypassWrite) VecInit(RegNext(io.w.req.bits.data).map(_.asTypeOf(wordType)))
  else VecInit((0 until way).map(_ => LFSR64().asTypeOf(wordType)))
  val bypass_mask = need_bypass(io.w.req.valid, io.w.req.bits.setIdx, io.w.req.bits.waymask.getOrElse("b1".U), io.r.req.valid, io.r.req.bits.setIdx)
  val mem_rdata = {
    if (singlePort) raw_rdata
    else VecInit(bypass_mask.asBools.zip(raw_rdata).zip(bypass_wdata).map {
      case ((m, r), w) => Mux(m, w, r)
    })
  }

  // hold read data for SRAMs
  val rdata = (if (holdRead) HoldUnless(mem_rdata, RegNext(realRen))
  else mem_rdata).map(_.asTypeOf(gen))

  io.r.resp.data := VecInit(rdata)
  io.r.req.ready := !resetState && (if (singlePort) !wen else true.B)
  io.w.req.ready := true.B

}

abstract class AbstractBankedDataArray(implicit val cacheConfig: SSDCacheConfig) extends CacheModule {
  val ReadlinePortErrorIndex = LoadPipelineWidth
  val io = IO(new Bundle {
    // load pipeline read word req
    val read = Vec(LoadPipelineWidth, Flipped(DecoupledIO(new L1BankedDataReadReq)))
    // main pipeline read / write line req
    val readline = Flipped(DecoupledIO(new L1BankedDataReadLineReq))
    val write = Flipped(DecoupledIO(new L1BankedDataWriteReq))
    // data bank read resp (all banks)
    val resp = Output(Vec(DCacheBanks, new L1BankedDataReadResult()))
    // val nacks = Output(Vec(LoadPipelineWidth, Bool()))
    // val errors = Output(Vec(LoadPipelineWidth + 1, new L1CacheErrorInfo)) // read ports + readline port
    val read_error = Output(Vec(LoadPipelineWidth, Bool()))
//    val readline_error = Output(Bool())
    // when bank_conflict, read (1) port should be ignored
    val bank_conflict_slow = Output(Vec(LoadPipelineWidth, Bool()))
    val bank_conflict_fast = Output(Vec(LoadPipelineWidth, Bool()))
    // customized cache op port
    //    val cacheOp = Flipped(new L1CacheInnerOpIO)
    //    override implicit val cacheConfig: SSDCacheConfig = _
  })
  assert(LoadPipelineWidth <= 2) // BankedDataArray is designed for no more than 2 read ports

  def pipeMap[T <: Data](f: Int => T) = VecInit((0 until LoadPipelineWidth).map(f))

  def addr_to_dcache_bank(addr: UInt) = {
    require(addr.getWidth >= DCacheSetOffset)
    addr(DCacheSetOffset - 1, DCacheBankOffset)
  }

  def addr_to_dcache_set(addr: UInt) = {
    require(addr.getWidth >= DCacheAboveIndexOffset)
    addr(DCacheAboveIndexOffset - 1, DCacheSetOffset)
  }

  def get_data_of_bank(bank: Int, data: UInt) = {
    require(data.getWidth >= (bank + 1) * DCacheSRAMRowBits)
    data(DCacheSRAMRowBits * (bank + 1) - 1, DCacheSRAMRowBits * bank)
  }

  def get_mask_of_bank(bank: Int, data: UInt) = {
    require(data.getWidth >= (bank + 1) * DCacheSRAMRowBytes)
    data(DCacheSRAMRowBytes * (bank + 1) - 1, DCacheSRAMRowBytes * bank)
  }
}






class BankedDataArray(implicit val p: SSDCacheConfig) extends AbstractBankedDataArray {


  val ReduceReadlineConflict = false

  io.write.ready := true.B

  // wrap data rows of 8 ways
  class DataSRAMBank(index: Int) extends Module {
    val io = IO(new Bundle() {
      val w = new Bundle() {
        val en = Input(Bool())
        val addr = Input(UInt())
        val way_en = Input(UInt(DCacheWays.W))
        val data = Input(UInt(DCacheSRAMRowBits.W))
      }

      val r = new Bundle() {
        val en = Input(Bool())
        val addr = Input(UInt())
        val way_en = Input(UInt(DCacheWays.W))
        val data = Output(UInt(DCacheSRAMRowBits.W))
      }
    })




    val r_way_en_reg = RegNext(io.r.way_en)

    // multiway data bank
    val data_bank = Array.fill(DCacheWays) {
      Module(new BankedSRAMTemplate(
        Bits(DCacheSRAMRowBits.W),
        set = DCacheSets,
        way = 1,
        shouldReset = false,
        holdRead = false,
        singlePort = true
      ))
    }

    for (w <- 0 until DCacheWays) {
      val wen = io.w.en && io.w.way_en(w)
      data_bank(w).io.w.req.valid := wen
      data_bank(w).io.w.req.bits.apply(
        setIdx = io.w.addr,
        data = io.w.data,
        waymask = 1.U
      )
      data_bank(w).io.r.req.valid := io.r.en
      data_bank(w).io.r.req.bits.apply(setIdx = io.r.addr)
    }

    val half = nWays / 2
    val data_read = data_bank.map(_.io.r.resp.data(0))
    val data_left = Mux1H(r_way_en_reg.tail(half), data_read.take(half))
    val data_right = Mux1H(r_way_en_reg.head(half), data_read.drop(half))

    val sel_low = r_way_en_reg.tail(half).orR()
    val row_data = Mux(sel_low, data_left, data_right)

    io.r.data := row_data

  }


  val data_banks = List.tabulate(DCacheBanks)(i => Module(new DataSRAMBank(i)))



  val way_en = Wire(Vec(LoadPipelineWidth, io.read(0).bits.way_en.cloneType))
  val way_en_reg = RegNext(way_en)
  val set_addrs = Wire(Vec(LoadPipelineWidth, UInt()))
  val bank_addrs = Wire(Vec(LoadPipelineWidth, UInt()))

  // read data_banks and ecc_banks
  // for single port SRAM, do not allow read and write in the same cycle
  val rwhazard = io.write.valid
  val rrhazard = false.B // io.readline.valid
  (0 until LoadPipelineWidth).map(rport_index => {
    set_addrs(rport_index) := addr_to_dcache_set(io.read(rport_index).bits.addr)
    bank_addrs(rport_index) := addr_to_dcache_bank(io.read(rport_index).bits.addr)

    io.read(rport_index).ready := !(rwhazard || rrhazard)

    // use way_en to select a way after data read out
    assert(!(RegNext(io.read(rport_index).fire() && PopCount(io.read(rport_index).bits.way_en) > 1.U)))
    way_en(rport_index) := io.read(rport_index).bits.way_en
  })
  io.readline.ready := !(rwhazard)

  // read each bank, get bank result
  val bank_result = Wire(Vec(DCacheBanks, new L1BankedDataReadResult()))
  dontTouch(bank_result)

  val rr_bank_conflict = bank_addrs(0) === bank_addrs(1) && io.read(0).valid && io.read(1).valid
  val rrl_bank_conflict_0 = Wire(Bool())
  val rrl_bank_conflict_1 = Wire(Bool())
  if (ReduceReadlineConflict) {
    rrl_bank_conflict_0 := io.read(0).valid && io.readline.valid && io.readline.bits.rmask(bank_addrs(0))
    rrl_bank_conflict_1 := io.read(1).valid && io.readline.valid && io.readline.bits.rmask(bank_addrs(1))
  } else {
    rrl_bank_conflict_0 := io.read(0).valid && io.readline.valid
    rrl_bank_conflict_1 := io.read(1).valid && io.readline.valid
  }

  val rw_bank_conflict_0 = io.read(0).valid && rwhazard
  val rw_bank_conflict_1 = io.read(1).valid && rwhazard
  val perf_multi_read = io.read(0).valid && io.read(1).valid
  io.bank_conflict_fast(0) := rw_bank_conflict_0 || rrl_bank_conflict_0
  io.bank_conflict_slow(0) := RegNext(io.bank_conflict_fast(0))
  io.bank_conflict_fast(1) := rw_bank_conflict_1 || rrl_bank_conflict_1 || rr_bank_conflict
  io.bank_conflict_slow(1) := RegNext(io.bank_conflict_fast(1))


  for (bank_index <- 0 until DCacheBanks) {
    //     Set Addr & Read Way Mask
    //
    //      Pipe 0      Pipe 1
    //        +           +
    //        |           |
    // +------+-----------+-------+
    //  X                        X
    //   X                      +------+ Bank Addr Match
    //    +---------+----------+
    //              |
    //     +--------+--------+
    //     |    Data Bank    |
    //     +-----------------+
    val bank_addr_matchs = WireInit(VecInit(List.tabulate(LoadPipelineWidth)(i => {
      bank_addrs(i) === bank_index.U && io.read(i).valid
    })))
    val readline_match = Wire(Bool())
    if (ReduceReadlineConflict) {
      readline_match := io.readline.valid && io.readline.bits.rmask(bank_index)
    } else {
      readline_match := io.readline.valid
    }
    val bank_way_en = Mux(readline_match,
      io.readline.bits.way_en,
      Mux(bank_addr_matchs(0), way_en(0), way_en(1))
    )
    val bank_set_addr = Mux(readline_match,
      addr_to_dcache_set(io.readline.bits.addr),
      Mux(bank_addr_matchs(0), set_addrs(0), set_addrs(1))
    )

    // read raw data
    val data_bank = data_banks(bank_index)
    data_bank.io.r.en := bank_addr_matchs.asUInt.orR || readline_match
    data_bank.io.r.way_en := bank_way_en
    data_bank.io.r.addr := bank_set_addr
    bank_result(bank_index).raw_data := data_bank.io.r.data

  }

  // read result: expose banked read result
  io.resp := bank_result

  // error detection
  // normal read ports
  (0 until LoadPipelineWidth).map(rport_index => {
    io.read_error(rport_index) := RegNext(io.read(rport_index).fire()) &&
      !io.bank_conflict_slow(rport_index)
  })
  // readline port


  // write data_banks & ecc_banks
  val sram_waddr = addr_to_dcache_set(io.write.bits.addr)
  for (bank_index <- 0 until DCacheBanks) {
    // data write
    val data_bank = data_banks(bank_index)
    data_bank.io.w.en := io.write.valid && io.write.bits.wmask(bank_index)
    data_bank.io.w.way_en := io.write.bits.way_en
    data_bank.io.w.addr := sram_waddr
    data_bank.io.w.data := io.write.bits.data(bank_index)

  }

}

