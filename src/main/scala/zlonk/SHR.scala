package zlonk

import chisel3._
import chisel3.experimental.{Analog, ChiselAnnotation, RunFirrtlTransform, annotate, requireIsChiselType, requireIsHardware}
import chisel3.util.{Cat, log2Ceil}
import firrtl._
import firrtl.annotations.{NoTargetAnnotation, SingleTargetAnnotation, Target}
import firrtl.ir._
import firrtl.options.{HasShellOptions, Phase, PhaseManager, Shell, ShellOption, Stage, StageMain}

import scala.collection.immutable.ListMap
import zlonk.implicits._

sealed trait ShrImplType {
  def cost(params: ShrParams[Data]): Int
  def impl[T <: Data]: Class[_ <: ShrImpl[T]]
}

trait FFImpl extends ShrImplType {
  def impl[T <: Data] = classOf[FFSHR[T]]
}
trait SinglePortSyncMemImpl extends ShrImplType {
  def impl[T <: Data] = classOf[SPMemSHR[T]]
}
trait DualPortSyncMemImpl extends ShrImplType {
  def impl[T <: Data] = classOf[DPMemSHR[T]]
}

case class ThresholdFFImpl(threshold: Int) extends FFImpl {
  def cost(params: ShrParams[Data]): Int = if (params.proto.getWidth * params.delay < threshold) {
    -1 // 0
  } else {
    -1 // 1
  }
}
case class ThresholdSinglePortSyncMemImpl(threshold: Int) extends SinglePortSyncMemImpl {
  def cost(params: ShrParams[Data]): Int = if (params.proto.getWidth * params.delay < threshold) {
    1
  } else {
    0
  }
}
case class ThresholdDualPortSyncMemImpl(threshold: Int) extends DualPortSyncMemImpl {
  def cost(params: ShrParams[Data]): Int = if (params.proto.getWidth * params.delay < threshold) {
    1
  } else {
    0
  }
}

trait ShrImplOptions {
  def impls: Seq[ShrImplType]
  def bestImpl(params: ShrParams[_ <: Data]): ShrImplType = impls.minBy(i => i.cost(params))
  def bestShr[T <: Data](params: ShrParams[T]): ShrImpl[T] = {
    val impl = bestImpl(params).impl
    val constructor = impl.getConstructor(classOf[ShrParams[T]])
    constructor.newInstance(params).asInstanceOf[ShrImpl[T]]
  }
}

case class ThresholdShrImplOptions(threshold: Int = 16 * 32, useSP: Boolean = false) extends ShrImplOptions {
  def impls = Seq(ThresholdFFImpl(threshold),
    (if (useSP) ThresholdSinglePortSyncMemImpl(threshold) else ThresholdDualPortSyncMemImpl(threshold)))
}

sealed trait ShrOptionAnnotation

case class ShrMemThresholdAnnotation(threshold: Int) extends NoTargetAnnotation with ShrOptionAnnotation
case class ShrSinglePortMemAnnotation(useSP: Boolean) extends NoTargetAnnotation with ShrOptionAnnotation
case class ReplaceShrAnnotation(target: Target, params: ShrParams[_ <: Data]) extends SingleTargetAnnotation[Target] {
  override def duplicate(n: Target): ReplaceShrAnnotation = this.copy(target = target)
}
case class ReplaceShrChiselAnnotation(target: InstanceId, params: ShrParams[_ <: Data]) extends ChiselAnnotation
with RunFirrtlTransform with HasShellOptions {

  def options = Seq(
    new ShellOption[Int](
      longOption = "shrMemThreshold",
      shortOption = Some("shrmt"),
      toAnnotationSeq = (t: Int) => Seq(ShrMemThresholdAnnotation(t)),
      helpText = "Threshold to replace default FF-based SHR with memory-based SHR",
      helpValueName = Some("<threshold>")
    ),
    new ShellOption[Boolean](
      longOption = "shrMemUseSinglePort",
      shortOption = Some("shrsp"),
      toAnnotationSeq = (b: Boolean) => Seq(ShrSinglePortMemAnnotation(b)),
      helpText = "Use single ported memories for shift registers",
      helpValueName = Some("<useSP>")
    )
  )

  override def transformClass = classOf[ShrImplTransform]
  override def toFirrtl: ReplaceShrAnnotation = ReplaceShrAnnotation(target.toTarget, params)
}

class ShrStage extends Stage {
  val shell: Shell = new Shell("Shift Register Mapping")
  val targets = Seq(
    classOf[ShrPhase]
  )

  def run(annotations: AnnotationSeq): AnnotationSeq = {
    println("Stage ran")
    val annos = try {
      new PhaseManager(targets)
        .transformOrder
        .foldLeft(annotations) { (a, f) => f.transform(a) }
    } catch {
      // todo better
      case e: Exception => throw e
    }
    annos
  }
}

object ShrMain extends StageMain(new ShrStage)

class ShrPhase extends Phase {
  override def transform(a: AnnotationSeq): AnnotationSeq = {
    println("Phase ran")
    a
  }
}

class ShrImplTransform extends Transform { // with RegisteredTransform {
  def inputForm = MidForm // LowForm
  def outputForm = HighForm

  val defaultShrMemThreshold = 64


  private def portToData(tpe: Type): Data = tpe match {
    case ClockType => UInt(0.W) // Clock()
    case AsyncResetType => UInt(0.W) // AsyncReset()
    case AnalogType(IntWidth(w)) => Analog(w.toInt.W)
    case UIntType(IntWidth(w)) => chisel3.UInt(w.toInt.W)
    case SIntType(IntWidth(w)) => chisel3.SInt(w.toInt.W)
    // should be lowered, but easy enough to handle
    case FixedType(IntWidth(w), IntWidth(bp)) => chisel3.experimental.FixedPoint(w.toInt.W, bp.toInt.BP)
    case BundleType(fields) => // make a port out of a field to reuse code
      val ports = fields.map(f => Port(NoInfo, f.name, firrtl.ir.Output, f.tpe))
      makeProto(ports)
    case VectorType(tpe, size) => Vec(size, portToData(tpe))

    // case GroundType(UnknownWidth) =>
    case _ =>
      Utils.error(s"SHR replacement pass has unknown type ${tpe.toString}")
  }
  private def makeProto(ports: Seq[Port]): Record = new Record() {
    override val elements: ListMap[String, Data] = ListMap(ports.map { case p =>
      p.name -> portToData(p.tpe)
    }: _*)

    override def cloneType: this.type = makeProto(ports).asInstanceOf[this.type]
  }

  private def makeShr(name: String, ports: Seq[Port], params: ShrParams[_ <: Data], impl: ShrImplOptions): firrtl.ir.DefModule = {
    val io = makeProto(ports).elements("io")
    val io_in = io.asInstanceOf[Record].elements("in")
    val structuralParams = params.copy(proto = io_in)
    val chiselCircuit = chisel3.Driver.elaborate(() => impl.bestShr(structuralParams))
      // new FFSHR(structuralParams))
    val firrtlCircuit = firrtl.Parser.parse(chisel3.Driver.emit(chiselCircuit))
    val mod = firrtlCircuit.modules.head.asInstanceOf[firrtl.ir.Module]
    mod.copy(name = name)
  }

  override def execute(state: CircuitState): CircuitState = {
    // val threshs = state.annotations.filter(_.getClass == classOf[ShrMemThresholdAnnotation])
    // require(threshs.length <= 1, "Specified SHR threshold twice")
    //val thresh: Int = threshs.headOption.map(_.asInstanceOf[ShrMemThresholdAnnotation].threshold)
    //  .getOrElse(defaultShrMemThreshold)
    val modNames: Map[String, ShrParams[_ <: Data]] = state.annotations.collect {
      case ReplaceShrAnnotation(t, p) => t.moduleOpt.get -> p
    }.toMap
    val implAnnos = state.annotations.collectFirst {
      case a: ShrImplOptions => a
    }
    val impl = if (implAnnos.isEmpty) {
      ThresholdShrImplOptions()
    } else if (implAnnos.size == 1) {
      implAnnos.head
    } else {
        throw new Exception(s"Oh no! There are ${implAnnos.size} annotations giving parameters for shr mapping (should be 1)")
    }

    val mods = state.circuit.modules.map {
      case firrtl.ir.Module(_, name, ports, _) if modNames.contains(name) =>
        makeShr(name, ports, modNames(name), impl)
      case m => m
    }

    println(mods.map(_.serialize).mkString("\n\n"))

    state.copy(circuit = state.circuit.copy(modules = mods))
  }
}

case class ShrParams[+T <: Data]
(
  proto: T,
  delay: Int,
  programmableDepth: Boolean = false,
  resetData: Option[T] = None,
  hasEnable: Boolean = false
) {
  requireIsChiselType(proto)
  require(delay >= 0)
  resetData.map(r => requireIsHardware(r))
}

class ShrIO[+T <: Data](params: ShrParams[T]) extends Bundle {
  val in = chisel3.Input(params.proto.cloneType)
  val out = chisel3.Output(params.proto.cloneType)
  val en = if (params.hasEnable) Some(chisel3.Input(Bool())) else None
  val delay = if (params.programmableDepth) Some(chisel3.Input(UInt(log2Ceil(params.delay).W))) else None

  override def cloneType = new ShrIO(params).asInstanceOf[this.type]
}

trait ShrImpl[+T <: Data] extends RawModule {
  val io: ShrIO[T]
}

class SPMemSHR[T <: Data](params: ShrParams[T]) extends chisel3.Module with ShrImpl[T] {
  require(params.delay > 3, "Single ported memory doesn't make sense for delay < 4")
  val io = IO(new ShrIO(params))

  val memProto = UInt((2 * params.proto.getWidth).W)
  val memDepth = params.delay / 2 // ok  if delay is odd, we'll add a FF

  val inFixed = if (params.delay % 2 == 1) {
    val ffshr = chisel3.Module(new FFSHR(params.copy(delay = 1)))
    ffshr.io.in := io.in
    ffshr.io.en := io.en
    ffshr.io.delay := io.delay
    io.out
  } else {
    io.in
  }
  val delay = if (params.programmableDepth) {
    val d = io.delay.get
    // check for delay to change, update pointers when it does
    when (d =/= RegNext(d, init = params.delay.U)) {
      val diff = writePtr - d
      readPtr := chisel3.Mux(writePtr > d, diff, params.delay.U + d - writePtr) // TODO check
    }
    d
  } else {
    params.delay.U
  }

  val readPtr = RegInit(0.U(log2Ceil(memDepth).W))
  val writePtr = RegInit(0.U(log2Ceil(memDepth).W))
  val reading = RegInit(false.B)
  val oldRead = Reg(UInt(params.proto.getWidth.W))
  val oldWrite = Reg(UInt(params.proto.getWidth.W))

  val mem = SyncReadMem(memDepth, memProto)

  when (reading) {
    oldWrite := io.in.asUInt
    val read = mem.read(readPtr)
    io.out := read(2 * params.proto.getWidth - 1, params.proto.getWidth).asTypeOf(params.proto)
    oldRead := read(params.proto.getWidth - 1, 0)
  } .otherwise {
    io.out := oldRead.asTypeOf(params.proto)
    mem.write(writePtr, Cat(oldWrite))
  }

  if (params.resetData.isDefined) {
    val initialCnt = RegInit(0.U(log2Ceil(params.delay).W))
    initialCnt := initialCnt +% 1.U

  }
}

class DPMemSHR[T <: Data](params: ShrParams[T]) extends chisel3.Module with ShrImpl[T] {
  require(params.delay > 3, "Single ported memory doesn't make sense for delay < 4")
  val io = IO(new ShrIO(params))
}

class FFSHR[T <: Data](params: ShrParams[T]) extends chisel3.Module with ShrImpl[T] {
  require(params.delay > 0)
  val io = IO(new ShrIO(params))

  // Pack shift register into UInt. Makes the generated code a bit more compact
  val delays = params.resetData match {
    case Some(rd) =>
      requireIsHardware(rd)
      RegInit(t = Vec(params.delay, UInt(params.proto.getWidth.W)), init = VecInit(Seq.fill(params.delay)(rd.asUInt)))
    case None =>
      Reg(Vec(params.delay, UInt(params.proto.getWidth.W)))
  }

  delays.foldLeft(io.in.asUInt) { case (in, del) =>
    if (params.hasEnable) {
      when (io.en.get) {
        del := in
      }
    } else {
      del := in
    }
    del
  }

  if (params.programmableDepth) {
    io.out := delays(io.delay.get).asTypeOf(params.proto)
  } else {
    io.out := delays.last.asTypeOf(params.proto)
  }
}

class SHR[T <: Data](val shrParams: ShrParams[T]) extends chisel3.Module {
  override def desiredName: String = {
    val className = shrParams.proto.getClass.getSimpleName
    val progName = if (shrParams.programmableDepth) "prog_" else ""
    val enName = if (shrParams.hasEnable) "en_" else ""
    val resetName = if (shrParams.resetData.isDefined) "reset_" else ""
    s"SHR_${className}_${progName}${enName}${resetName}${shrParams.delay}"
  }

  val io = IO(new ShrIO(shrParams))
  annotate(ReplaceShrChiselAnnotation(this, shrParams.copy(proto = UInt())))
  dontTouch(io)
  io.out := io.in
}

object SHR {
  def apply[T <: Data](in: T, n: Int): T = {
    requireIsHardware(in)
    if (n > 0) {
      val shr = chisel3.Module(new SHR(
        ShrParams(
          chiselTypeOf(in),
          delay = n,
          programmableDepth = false,
          resetData = None,
          hasEnable = false
      )))
      shr.io.in := in
      shr.io.out
    } else {
      in
    }
  }
  def apply[T <: Data](in: T, n: Int, en: Bool): T = {
    requireIsHardware(in)
    requireIsHardware(en)
    if (n > 0) {
      val shr = chisel3.Module(new SHR(
        ShrParams(
          chiselTypeOf(in),
          delay = n,
          programmableDepth = false,
          resetData = None,
          hasEnable = true
        )))
      shr.io.in := in
      shr.io.en.foreach( _ := en)
      shr.io.out
    } else {
      in
    }
  }
  def apply[T <: Data](in: T, n: Int, resetData: T, en: Bool): T = {
    requireIsHardware(in)
    requireIsHardware(en)
    requireIsHardware(resetData)
    if (n > 0) {
      val shr = chisel3.Module(new SHR(
        ShrParams(
          chiselTypeOf(in),
          delay = n,
          programmableDepth = false,
          resetData = Some(resetData),
          hasEnable = true
        )))
      shr.io.in := in
      shr.io.en.foreach( _ := en)
      shr.io.out
    } else {
      in
    }
  }
}
