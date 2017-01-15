package mcmc

import mcmc.ScaleOperator.scaler
import monocle.Lens
import shapeless.ops.hlist.LeftFolder
import shapeless.{::, HList, Poly2}
import spire.algebra.{Field, Trig}
import spire.random.Generator
import spire.syntax.field._

import scalaz.{@@, Tag}

class ScaleOperator[T, @specialized(Double) R : Field : Trig, X, L <: Lens[T, R @@ X] :: HList](val scaleFactor: R, val lenses: L)(implicit rng: Generator, foldLeft: LeftFolder.Aux[L, (T, R), scaler.type, (T, R)]) extends Operator[T, R] {

  override def apply(t: T): T = {
    val scale = scaleFactor + rng.nextDouble * (1 / scaleFactor - scaleFactor)
    lenses.foldLeft((t, scale))(scaler)._1
  }

  override def hastingsRatio(x: T, y: T): R =
    (lenses.runtimeLength - 2) * Trig[R].log(Tag.unwrap(lenses.head.get(x)) / Tag.unwrap(lenses.head.get(x)))

}

object ScaleOperator {

  object scaler extends Poly2 {
    implicit def f[T, R : Field, X] = at[(T, R), Lens[T, R @@ X]].apply[(T, R)]((ts, lens) => (lens.modify(x => Tag(ts._2 * Tag.unwrap(x)))(ts._1), ts._2))
  }

}
