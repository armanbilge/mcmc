package mcmc

import mcmc.implicits._
import monocle.Lens
import org.scalatest.refspec.RefSpec
import shapeless.tag
import shapeless.tag.@@
import spire.random.rng.MersenneTwister64
import spire.std.double._

class ScaleOperatorSpec extends RefSpec {

  def `simple example`: Unit = {

    trait X
    trait Y
    case class A(x: Double @@ X, y: Double @@ Y)
    val x = Lens[A, Double @@ X](_.x)(x => a => a.copy(x = x))
    val y = Lens[A, Double @@ Y](_.y)(y => a => a.copy(y = y))

    implicit val rng = MersenneTwister64.fromArray(Array(666))

    val scaler = new ScaleOperator[A, Double](0.75, Seq(x, y))
    assert(scaler.apply(A(tag[X](2.0), tag[Y](3.0))) == A(tag[X](1.586437609419412), tag[Y](2.379656414129118)))

  }

}
