package mcmc

import monocle.Lens
import monocle.function.At
import spire.NoImplicit
import spire.algebra.AdditiveMonoid

import scala.collection.generic.CanBuildFrom

class IIDProbability[@specialized(Double) R : AdditiveMonoid, P <: Probability[R], C <: Traversable[P]](val ps: C) extends Probability[R] {

  override lazy val evaluate: R = implicitly[AdditiveMonoid[R]].sum(ps.map(_.evaluate))

}

object IIDProbability {

  def ps[R : AdditiveMonoid, P <: Probability[R], C <: Traversable[P]]: Lens[IIDProbability[R, P, C], C] =
    Lens[IIDProbability[R, P, C], C](_.ps)(ps => _ => new IIDProbability[R, P, C](ps))

  implicit def all[R : AdditiveMonoid, P <: Probability[R], C <: Traversable[P], T](implicit p_t: Lens[P, T], cbf: CanBuildFrom[Traversable[P], P, C]): Lens[IIDProbability[R, P, C], T] =
    Lens[IIDProbability[R, P, C], T](iid => p_t.get(iid.ps.head))(t => ps[R, P, C].modify(_.map[P, C](p_t.set(t))))

  implicit def at[R : AdditiveMonoid, P <: Probability[R], C <: Traversable[P], I, T](implicit cAt: At[C, I, T], pAt: NoImplicit[At[P, I, T]]): At[IIDProbability[R, P, C], I, T] =
    (i: I) => ps[R, P, C] ^|-> cAt.at(i)

  implicit def atAll[R : AdditiveMonoid, P <: Probability[R], C <: Traversable[P], I, T](implicit pAt: At[P, I, T], cbf: CanBuildFrom[Traversable[P], P, C]): At[IIDProbability[R, P, C], I, T] =
    (i: I) => Lens[IIDProbability[R, P, C], T](iid => pAt.at(i).get(iid.ps.head)) { t =>
      val set = pAt.at(i).set(t)
      ps[R, P, C].modify(_.map[P, C](set))
    }

}
