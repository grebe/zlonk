package zlonk

import firrtl.AnnotationSeq
import firrtl.options.OptionsView

package object stage {
  implicit object ShrImplOptionsView extends OptionsView[ShrImplOptions] {
    def view(options: AnnotationSeq): ShrImplOptions = options
      .collect { case a: ShrOptionAnnotation => a }
      .foldLeft(new ThresholdShrImplOptions()) { (c, x) =>
        x match {
          case ShrMemThresholdAnnotation(thresh) => c.copy(threshold = thresh)
          case ShrSinglePortMemAnnotation(useSP) => c.copy(useSP = useSP)
        }
      }
  }
}