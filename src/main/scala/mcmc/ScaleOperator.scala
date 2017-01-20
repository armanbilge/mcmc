package mcmc

import monocle.Lens
import spire.algebra.{Field, Trig}
import spire.random.Generator
import spire.syntax.field._

class ScaleOperator[T, @specialized(Double) R : Field : Trig](val scaleFactor: R, val lenses: Traversable[Lens[T, R]])(implicit rng: Generator) extends Operator[T, R] {

  override def apply(t: T): T = {
    val scale = scaleFactor + rng.nextDouble * (1 / scaleFactor - scaleFactor)
    lenses.foldRight(t)(_.modify(scale * _)(_))
  }

  override def hastingsRatio(x: T, y: T): R =
    (lenses.size - 2) * Trig[R].log(Field[R].div(lenses.head.get(x), lenses.head.get(x)))

}
