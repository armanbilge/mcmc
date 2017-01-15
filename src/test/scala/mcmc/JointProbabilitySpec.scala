package mcmc

import monocle.Lens
import org.scalatest.refspec.RefSpec
import shapeless.tag
import shapeless.tag.@@
import spire.std.double._

class JointProbabilitySpec extends RefSpec {

  def `nested exponentials`: Unit = {

    trait Lambda1
    trait Lambda2
    trait Lambda3

    class Exponential[X <: Double, L <: Double](val x: X, val l: L) extends Probability[Double] {
      val evaluate: Double = math.log(l) + l * x
    }
    implicit def x[X <: Double, L <: Double]: Lens[Exponential[X, L], X] =
      Lens[Exponential[X, L], X](_.x)(x => exp => new Exponential(x, exp.l))
    implicit def l[X <: Double, L <: Double]: Lens[Exponential[X, L], L] =
      Lens[Exponential[X, L], L](_.l)(l => exp => new Exponential(exp.x, l))


    type JP = JointProbability[Double, Exponential[Double, Double @@ Lambda3], Exponential[Double @@ Lambda3, Double @@ Lambda2]]
    type P = JointProbability[Double, JP, Exponential[Double @@ Lambda2, Double @@ Lambda1]]
    val p: P = new P(new JP(new Exponential(1.0, tag[Lambda3](1.0)), new Exponential(tag[Lambda3](1.0), tag[Lambda2](1.0))), new Exponential(tag[Lambda2](1.0), tag[Lambda1](1.0)))

    val lambda1 = implicitly[Lens[P, Double @@ Lambda1]]
    val lambda2 = implicitly[Lens[P, Double @@ Lambda2]]
    val lambda3 = implicitly[Lens[P, Double @@ Lambda3]]

    assert(p.evaluate == 3)

    {
      val pp = lambda3.set(tag[Lambda3](2.0))(p)
      assert(p.p ne pp.p)
      assert(p.q eq pp.q)
    }

    {
      val pp = lambda1.set(tag[Lambda1](2.0))(p)
      assert(p.p.p eq pp.p.p)
    }

  }

}
