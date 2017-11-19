package mcmc

import monocle.Lens
import spire.algebra.{Field, Order, Trig}
import spire.random.Generator
import spire.syntax.field._
import spire.syntax.order._

class UpDownOperator[T, @specialized(Double) R : Field : Trig : Order](val scaleFactor: R, val up: Traversable[Lens[T, R]], val down: Traversable[Lens[T, R]])(implicit val rng: Generator) extends Operator[T, R] {

  require(Field[R].zero < scaleFactor && scaleFactor < Field[R].one)

  override def apply(t: T): T = {
    val scale = scaleFactor + rng.nextDouble * (1 / scaleFactor - scaleFactor)
    down.foldRight(up.foldRight(t)(_.modify(scale * _)(_)))(_.modify(_ / scale)(_))
  }

  override def hastingsRatio(x: T, y: T): R =
    (up.size - down.size - 2) * Trig[R].log(up.head.get(y) / up.head.get(x))

}

object UpDownOperator {

  implicit def coercer[T, R : Field : Trig : Order]: OperatorCoercer[T, R, UpDownOperator[T, R]] =
    OperatorCoercer[T, R, UpDownOperator[T, R]](op => Trig[R].log(1 / op.scaleFactor - 1))(x => op => new UpDownOperator[T, R](1 / (Trig[R].exp(x) + 1), op.up, op.down)(Field[R], Trig[R], Order[R], op.rng))

}
