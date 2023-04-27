package top

import chisel3._
import chisel3.stage._
//import Frontend._
import sim.SimTop
import system._
import top.TopMain.args
import top.WuKongConfig



object moduleTop extends App{
  lazy val config = WuKongConfig(FPGAPlatform = false)
//  (new ChiselStage).execute(args, Seq(
//    ChiselGeneratorAnnotation(() => new Core()(config)))
////    ChiselGeneratorAnnotation(() => new testModule))
//  )
  (new chisel3.stage.ChiselStage).execute(args, Seq(
    chisel3.stage.ChiselGeneratorAnnotation(() =>Module(new WuKong()(config))),
    firrtl.stage.RunFirrtlTransformAnnotation(new AddModulePrefix()),
    ModulePrefixAnnotation("ysyx_210062_")
  ))
}