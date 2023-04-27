/**************************************************************************************
 * Copyright (c) 2020 Institute of Computing Technology, CAS
 * Copyright (c) 2020 University of Chinese Academy of Sciences
 *
 * WuKong is licensed under Mulan PSL v2.
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

package WuKong

import chisel3._
import chisel3.util._
import WuKong.Backend._
import WuKong.Frontend._
import WuKong.isa.{SrcType, FuType, FuOpType}
import WuKong.Backend.ALU2PMUIO
import WuKong.CoreBundle
class CtrlSignalIO extends CoreBundle {
  val src1Type = Output(SrcType())
  val src2Type = Output(SrcType())
  val fuType = Output(FuType())
  val fuOpType = Output(FuOpType())
  val rfSrc1 = Output(UInt(5.W))
  val rfSrc2 = Output(UInt(5.W))
  val rfWen = Output(Bool())
  val rfDest = Output(UInt(5.W))
  val isCoreTrap = Output(Bool())
  val isSrc1Forward = Output(Bool())
  val isSrc2Forward = Output(Bool())
  val noSpecExec = Output(Bool())  // This inst can not be speculated
  val isBlocked = Output(Bool())   // This inst requires pipeline to be blocked

  //new signal
  val rs1Valid = Output(Bool())
  val rs2Valid = Output(Bool())
  val rdValid = Output(Bool())
}

class DataSrcIO extends CoreBundle {
  val src1 = Output(UInt(XLEN.W))
  val src2 = Output(UInt(XLEN.W))
  val imm  = Output(UInt(XLEN.W))
}

class RedirectIO extends CoreBundle {
  val target = Output(UInt(VAddrBits.W))
  val rtype = Output(UInt(1.W)) // 1: branch mispredict: only need to flush frontend  0: others: flush the whole pipeline
  val valid = Output(Bool())
  val btbIsBranch = Output(UInt(4.W))
  //for debug
  val pc = Output(UInt(VAddrBits.W))
}

class RedirectIO_nooo extends CoreBundle {
  val target = Output(UInt(VAddrBits.W))
  val rtype = Output(UInt(1.W)) // 1: branch mispredict: only need to flush frontend  0: others: flush the whole pipeline
  val valid = Output(Bool())
}




class BypassIO extends  Bundle{
  val isALU = Output(Bool())
  val isMDU = Output(Bool())
  val isLoad = Output(Bool())
  val isStore = Output(Bool())
}
class CtrlFlowIO extends CoreBundle {
  val instr = Output(UInt(64.W))
  val pc = Output(UInt(VAddrBits.W))
  val pnpc = Output(UInt(VAddrBits.W))
  val redirect = new RedirectIO
  val exceptionVec = Output(Vec(16, Bool()))
  val intrVec = Output(Vec(12, Bool()))
  val brIdx = Output(UInt(4.W))
  val isRVC = Output(Bool())
  val crossPageIPFFix = Output(Bool())
  val runahead_checkpoint_id = Output(UInt(64.W))
  val isBranch = Output(Bool())

  //sfb add
  val sfb = Output(Bool())
}

class DecodeIO extends CoreBundle {
  val cf = new CtrlFlowIO
  val ctrl = new CtrlSignalIO
  val data = new DataSrcIO
}

class WriteBackIO extends CoreBundle {
  val rfWen = Output(Bool())
  val rfDest = Output(UInt(5.W))
  val rfData = Output(UInt(XLEN.W))
}

class CommitIO extends CoreBundle {
  val decode = new DecodeIO
  val isMMIO = Output(Bool())
  val intrNO = Output(UInt(XLEN.W))
  val commits = Output(Vec(FuType.num, UInt(XLEN.W)))
}



class FunctionUnitIO extends CoreBundle {
  val in = Flipped(Decoupled(new Bundle {
    val src1 = Output(UInt(XLEN.W))
    val src2 = Output(UInt(XLEN.W))
    val func = Output(FuOpType())
  }))
  val out = Decoupled(Output(UInt(XLEN.W)))
}

class BankedFunctionUnitIO extends CoreBundle {
  val in = Vec(2, Flipped(Decoupled(new Bundle {
    val src1 = Output(UInt(XLEN.W))
    val src2 = Output(UInt(XLEN.W))
    val offset = Output(UInt(XLEN.W))
    val func = Output(FuOpType())
  })))
  val out = Vec(2, Decoupled(Output(UInt(XLEN.W))))
}

class ForwardIO extends CoreBundle {
  val valid = Output(Bool())
  val wb = new WriteBackIO
  val fuType = Output(FuType())
}

class MMUIO extends CoreBundle {
  // val ptev = Output(Bool())
  // val pteu = Output(Bool())
  // val ptex = Output(Bool())
  // val valid = Output(Bool())
  // val isStore = Output(Bool())

  val priviledgeMode = Input(UInt(2.W))
  val status_sum = Input(Bool())
  val status_mxr = Input(Bool())

  val loadPF = Output(Bool())
  val storePF = Output(Bool())
  val addr = Output(UInt(VAddrBits.W))

  def isPF() = loadPF || storePF
}

class MemMMUIO extends CoreBundle {
  val imem = new MMUIO
  val dmem = new MMUIO
}

class TLBExuIO extends CoreBundle {
  val satp = Output(UInt(XLEN.W))
  val sfence = new Bundle {
    val valid = Output(Bool())
    val asid  = Output(UInt(9.W))
    val vaddr = Output(UInt(XLEN.W))
  }

  def access(valid: Bool, src1: UInt, src2: UInt, func: UInt, satp: UInt) = {//func no use here for just sfence.vma only
    this.sfence.valid := valid
    this.sfence.vaddr := src1
    this.sfence.asid  := src2(8,0)
    this.satp := satp
  }
}

class PredictPkt extends CoreBundle {
  val pc = Output(UInt(VAddrBits.W)) // real PC will be regenerated in IBF
  val pnpc = Output(UInt(VAddrBits.W))
  val brIdx = Output(UInt(4.W))
  val instValid = Output(UInt(4.W))
  //above will be used as user bits in icache
  val icachePF = Output(Bool())
  val btbIsBranch = Output(UInt(4.W))

  //sfb add
  val sfb = Output(UInt(4.W))
}

// Micro OP
class StallFlushIO extends Bundle{
  //stall point : (0,2) -> i0pipestall, i1pipestall, memsstall
  val stall = Output(Vec(3,Bool()))
  // val flush = Output(Vec(10,Bool())) //10 + 2 (2 is for regfile invalid write)
  val invalid = Output(Vec(12,Bool()))
}
class decodePkt extends  CoreBundle{
  val rd = Output(UInt(5.W))
  val rdvalid = Output(Bool())
  val alu = Output(Bool())
  val muldiv = Output(Bool())
  val load = Output(Bool())
  val store = Output(Bool())
  val subalu = Output(Bool())
  val branch = Output(Bool())
  val csr = Output(Bool())
  val skip = Output(Bool())
  val mou  = Output(Bool())
}
trait hasBypassConst{
  def E0BypassPort = 10  // 0->9: alu1,alu0,e21,e20,e31,e30,mem3,mdu3,subalu1,subalu0,e51,e50
  def E2BypassPort = 2   // 0->1: e51,e50
  def E3BypassPort = 5   // 0->8 : e30,e40,e41,e50,e51
  def E1StoreBypassPort = 5
  def E2StoreBypassPort = 7
}
class BypassCtl extends Bundle with hasBypassConst {
  val rs1bypasse0 = Output(Vec(E0BypassPort,Bool()))
  val rs2bypasse0 = Output(Vec(E0BypassPort,Bool()))
  val rs1bypasse2 = Output(Vec(E2BypassPort,Bool()))
  val rs2bypasse2 = Output(Vec(E2BypassPort,Bool()))
  val rs1bypasse3 = Output(Vec(E3BypassPort,Bool()))
  val rs2bypasse3 = Output(Vec(E3BypassPort,Bool()))
}
class LSUPipeBypassCtrl extends Bundle with hasBypassConst{
  val lsBypassCtrlE1 = Output(Vec(E1StoreBypassPort,Bool()))
  val storeBypassCtrlE2 = Output(Vec(E2StoreBypassPort,Bool()))
}
class LSUBypassCtrl extends Bundle with hasBypassConst{
  val lsBypassCtrli0E1 = Output(Vec(E1StoreBypassPort,Bool()))
  val lsBypassCtrli1E1 = Output(Vec(E1StoreBypassPort,Bool()))
  val storeBypassCtrlE2 = Output(Vec(E2StoreBypassPort,Bool()))
}
class StorePipeBypassPort extends  Bundle with hasBypassConst{
  val lsBypassPortE1 = Output(Vec(E1StoreBypassPort,UInt(64.W)))
  val storeBypassPortE2 = Output(Vec(E2StoreBypassPort,UInt(64.W)))
}
class BypassPkt extends Bundle {
  val decodePkt = new decodePkt
  val BypassCtl = new BypassCtl
  val lsuCtrl = new LSUPipeBypassCtrl
}
class rsrdPkt extends Bundle{
  val rs1Valid = Output(Bool())
  val rs1Pc = Output(Bool())
  val rs2Valid = Output(Bool())
  val rs2Imm = Output(Bool())
  val rdValid = Output(Bool())
  val rs1 = Output(UInt(5.W))
  val rs2 = Output(UInt(5.W))
  val rd = Output(UInt(5.W))
}
class FuPkt extends CoreBundle {
  val rs1 = Output(UInt(64.W))
  val rs2 = Output(UInt(64.W))
  val rd = Output(UInt(64.W))
  val fuOpType = Output(UInt(7.W))
  val offset = Output(UInt(64.W))
  val bpuUpdateReq = new BPUUpdateReq
  val alu2pmu = new ALU2PMUIO
  val redirect = new RedirectIO
  //for difftest
  val instr = Output(UInt(32.W))
  //for redirect
  val pc = Output(UInt(VAddrBits.W))
  val pnpc = Output(UInt(VAddrBits.W))
  val brIdx = Output(UInt(4.W))
  val isRVC = Output(Bool())  //not use
  val isBranch = Output(Bool()) // not use
  val debugInfo = new rsrdPkt
  val csrInst = Output(Bool())
  //for SubALU
  val isSubALU = Output(Bool())
  //for MMIO
  val isMMIO = Output(Bool())

  val btbIsBranch = Output(Bool())  //for update 

  val branchTaken = Output(Bool())
  //for difftest
  val CSRregfile = new CSRregfile
  val ArchEvent = new ArchEvent
  //for sfb
  val sfb = Output(Bool())
}
class CSRregfile extends CoreBundle {
  val priviledgeMode      =  Output(UInt(XLEN.W))
  val mstatus      =  Output(UInt(XLEN.W))
  val sstatus      =  Output(UInt(XLEN.W))
  val mepc      =  Output(UInt(XLEN.W))
  val sepc      =  Output(UInt(XLEN.W))
  val mtval      =  Output(UInt(XLEN.W))
  val stval      =  Output(UInt(XLEN.W))
  val mtvec      =  Output(UInt(XLEN.W))
  val stvec      =  Output(UInt(XLEN.W))
  val mcause      =  Output(UInt(XLEN.W))
  val scause      =  Output(UInt(XLEN.W))
  val satp      =  Output(UInt(XLEN.W))
  val mip      =  Output(UInt(XLEN.W))
  val mie      =  Output(UInt(XLEN.W))
  val mscratch      =  Output(UInt(XLEN.W))
  val sscratch      =  Output(UInt(XLEN.W))
  val mideleg      =  Output(UInt(XLEN.W))
  val medeleg      =  Output(UInt(XLEN.W))
}

class ArchEvent extends CoreBundle {
  val intrNO =        Output(UInt(32.W))
  val cause =         Output(UInt(32.W))
  val exceptionPC =   Output(UInt(64.W))
  val exceptionInst = Output(UInt(32.W))
}

