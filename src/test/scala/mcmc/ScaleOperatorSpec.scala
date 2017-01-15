package mcmc

import monocle.Lens
import org.scalatest.refspec.RefSpec
import shapeless.{::, HNil}
import spire.random.rng.MersenneTwister64
import spire.std.double._

import scalaz.{@@, Tag}

class ScaleOperatorSpec extends RefSpec {

  def `simple example`: Unit = {

    trait X
    trait Y
    case class A(x: Double @@ X, y: Double @@ Y)
    val x = Lens[A, Double @@ X](_.x)(x => a => a.copy(x = x))
    val y = Lens[A, Double @@ Y](_.y)(y => a => a.copy(y = y))

    implicit val rng = MersenneTwister64.fromTime(666)

    val scaler = new ScaleOperator[A, Double, X, Lens[A, Double @@ X] :: Lens[A, Double @@ Y] :: HNil](0.75, x :: y :: HNil)
    assert(scaler.apply(A(Tag(2.0), Tag(3.0))) == A(Tag(2.373927201651749), Tag(3.5608908024776236)))

  }

}
