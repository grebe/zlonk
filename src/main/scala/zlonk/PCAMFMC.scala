package zlonk

import chisel3._
import chisel3.experimental.Analog

class Diff extends Bundle {
  val p = Analog(1.W)
  val n = Analog(1.W)
}
object Diff {
  def apply() = new Diff
}

class CAMIO extends Bundle {
  val hsClkP = Diff()
  val hsLaneP = Vec(2, Diff())
  val lpClkP = Diff()
  val lpLaneP = Vec(2, Diff())
  val camBTA = Analog(1.W)
  val camGPIO = Analog(1.W)
}

class PCAMFMCIO extends Bundle {
  val a = new CAMIO()
  val b = new CAMIO()
  val c = new CAMIO()
  val d = new CAMIO()

  val camGPIOOen = Output(Bool())
  val camPWUp = Input(Bool())
  val camGPIODir = Output(Bool())
}

class PCAMFMC extends MultiIOModule {

}
