package zlonk

import chisel3._
import chisel3.experimental.FixedPoint
import chisel3.iotesters.PeekPokeTester
import org.scalatest.{FlatSpec, Matchers}

abstract class SHRTester[T <: Data](c: SHR[T]) extends PeekPokeTester(c) {
  def pokeI(i: Int): Unit
  def expectI(i: Int): Unit

  for (i <- 0 until c.shrParams.delay + 100) {
    pokeI(i)
    if (i > c.shrParams.delay) {
      expectI(i - c.shrParams.delay)
    }
    step(1)
  }
}

class TestBundle extends Bundle {
  val a = UInt(10.W)
  val b = SInt(5.W)
  val c = FixedPoint(5.W, 2.BP)

  class MiniBundle extends Bundle {
    val a = Bool()
    val b = Vec(5, SInt(2.W))
  }
  val d = Vec(5, new MiniBundle)
  val e = UInt(0.W)
}

class UIntSHRTester(c: SHR[UInt]) extends SHRTester(c) {
  override def pokeI(i: Int): Unit = poke(dut.io.in, i)
  override def expectI(i: Int): Unit = expect(dut.io.out, i)
}

class SIntSHRTester(c: SHR[SInt]) extends SHRTester(c) {
  override def pokeI(i: Int): Unit = poke(dut.io.in, i)
  override def expectI(i: Int): Unit = expect(dut.io.out, i)
}

class FixedPointSHRTester(c: SHR[FixedPoint]) extends SHRTester(c) {
  override def pokeI(i: Int): Unit = poke(dut.io.in, i)
  override def expectI(i: Int): Unit = expect(dut.io.out, i)
}

class BundleSHRTester(c: SHR[TestBundle]) extends SHRTester(c) {
  override def pokeI(i: Int): Unit = poke(dut.io.in.a, i)
  override def expectI(i: Int): Unit = expect(dut.io.out.a, i)
}

object SHRTester {
  def apply[T <: Data](shrgen: () => SHR[T], args: Array[String]): (SHR[T] => SHRTester[T]) => Boolean = {
    chisel3.iotesters.Driver.execute(args, shrgen)
  }
}

class SHRSpec extends FlatSpec with Matchers {
  behavior of "SHR"

  it should "work with UInt" in {
    SHRTester(() => new SHR(ShrParams(UInt(10.W), delay = 10)), Array[String]()) { c =>
      new UIntSHRTester(c)
    } should be (true)
  }

  it should "work with SInt" in {
    SHRTester(() => new SHR(ShrParams(SInt(10.W), delay = 10)), Array[String]()) { c =>
      new SIntSHRTester(c)
    } should be (true)
  }

  it should "work with FixedPoint" in {
    SHRTester(() => new SHR(ShrParams(FixedPoint(10.W, 4.BP), delay = 10)), Array[String]("--help", "4")) { c =>
      new FixedPointSHRTester(c)
    } should be (true)
  }

  it should "work with Bundles and Vectors" in {
    SHRTester(() => new SHR(ShrParams(new TestBundle, delay = 10)), Array[String]()) { c =>
      new BundleSHRTester(c)
    } should be (true)
  }

}
