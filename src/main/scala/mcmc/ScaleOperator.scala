package mcmc

import monocle.Lens
import spire.algebra.{Field, Order, Trig}
import spire.random.Generator
import spire.syntax.field._
import spire.syntax.order._

class ScaleOperator[T, @specialized(Double) R : Field : Trig : Order](val scaleFactor: R, val lenses: Traversable[Lens[T, R]])(implicit val rng: Generator) extends Operator[T, R] {

  require(Field[R].zero < scaleFactor && scaleFactor < Field[R].one)

  override def apply(t: T): T = {
    val scale = scaleFactor + rng.nextDouble * (1 / scaleFactor - scaleFactor)
    lenses.foldRight(t)(_.modify(scale * _)(_))
  }

  override def hastingsRatio(x: T, y: T): R =
    (lenses.size - 2) * Trig[R].log(lenses.head.get(y) / lenses.head.get(x))

}

object ScaleOperator {

  implicit def coercer[T, R : Field : Trig : Order]: OperatorCoercer[T, R, ScaleOperator[T, R]] =
    OperatorCoercer[T, R, ScaleOperator[T, R]](op => Trig[R].log(1 / op.scaleFactor - 1))(x => op => new ScaleOperator[T, R](1 / (Trig[R].exp(x) + 1), op.lenses)(Field[R], Trig[R], Order[R], op.rng))

}
