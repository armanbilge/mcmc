package mcmc

import mcmc.ScaleOperator.scaler
import monocle.Lens
import shapeless.{::, HList}
import shapeless.ops.hlist.LeftFolder
import shapeless.tag.@@
import spire.algebra.{Field, Trig}
import spire.random.Generator
import spire.syntax.field._

class UpDownOperator[T, @specialized(Double) R : Field : Trig, X, U <: Lens[T, R @@ X] :: HList, D <: HList](val scaleFactor: R, val up: U, val down: D)(implicit rng: Generator, foldLeftUp: LeftFolder.Aux[U, (T, R), scaler.type, (T, R)], foldLeftDown: LeftFolder.Aux[D, (T, R), scaler.type, (T, R)]) extends Operator[T, R] {

  override def apply(t: T): T = {
    val scale = scaleFactor + rng.nextDouble * (1 / scaleFactor - scaleFactor)
    val tp = up.foldLeft((t, scale))(scaler)._1
    down.foldLeft((tp, 1 / scale))(scaler)._1
  }

  override def hastingsRatio(x: T, y: T): R =
    (up.runtimeLength - down.runtimeLength - 2) * Trig[R].log(Field[R].div(up.head.get(x), up.head.get(x)))

}
