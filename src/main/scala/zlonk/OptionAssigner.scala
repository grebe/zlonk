package zlonk

import chisel3.Data
import scala.language.implicitConversions

class OptionAssigner[T <: Data](lhs: Option[T]) {
  def :=[V <: Data](rhs: V) = { lhs.foreach { _ := rhs} }
  def <>[V <: Data](rhs: V) = { lhs.foreach { _ <> rhs } }

  def :=[V <: Data](rhs: Option[V]) = { lhs.foreach { _ := rhs.get } }
  def :=[V <: Data](rhs: Option[V], default: V) = { lhs.foreach { _ := rhs.getOrElse(default) } }
  def <>[V <: Data](rhs: Option[V]) = { lhs.foreach { _ <> rhs.get } }
  def <>[V <: Data](rhs: Option[V], default: V) = { lhs.foreach { _ <> rhs.getOrElse(default) } }
}

trait OptionAssignerSyntax {
  implicit def optionToAssigner[T <: Data](lhs: Option[T]): OptionAssigner[T] = new OptionAssigner(lhs)
}

object implicits extends OptionAssignerSyntax
