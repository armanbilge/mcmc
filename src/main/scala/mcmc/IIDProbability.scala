package mcmc

import monocle.Lens
import monocle.function.At
import spire.NoImplicit
import spire.algebra.AdditiveMonoid

import scala.collection.TraversableLike
import scala.collection.generic.CanBuildFrom

class IIDProbability[@specialized(Double) R : AdditiveMonoid, P <: Probability[R], C <: TraversableLike[P, C]](val ps: C) extends Probability[R] with Serializable {

  override lazy val evaluate: R = implicitly[AdditiveMonoid[R]].sum(ps.map(_.evaluate))

}

object IIDProbability {

  implicit def ps[R : AdditiveMonoid, P <: Probability[R], C <: TraversableLike[P, C]]: Lens[IIDProbability[R, P, C], C] =
    Lens[IIDProbability[R, P, C], C](_.ps)(ps => _ => new IIDProbability[R, P, C](ps))

  implicit def all[R : AdditiveMonoid, P <: Probability[R], C <: TraversableLike[P, C], T](implicit p_t: Lens[P, T], cbf: CanBuildFrom[C, P, C]): Lens[IIDProbability[R, P, C], T] =
    Lens[IIDProbability[R, P, C], T](iid => p_t.get(iid.ps.head))(t => ps[R, P, C].modify(_.map[P, C](p_t.set(t))))

  implicit def at[R : AdditiveMonoid, P <: Probability[R], C <: TraversableLike[P, C], I, T](implicit p_t: Lens[P, T], cAt: At[C, I, P], pAt: NoImplicit[At[P, I, T]]): At[IIDProbability[R, P, C], I, T] = new At[IIDProbability[R, P, C], I, T] {
    override def at(i: I): Lens[IIDProbability[R, P, C], T] = ps[R, P, C] ^|-> cAt.at(i) ^|-> p_t
  }

  implicit def atAll[R : AdditiveMonoid, P <: Probability[R], C <: TraversableLike[P, C], I, T](implicit pAt: At[P, I, T], cbf: CanBuildFrom[C, P, C]): At[IIDProbability[R, P, C], I, T] = new At[IIDProbability[R, P, C], I, T] {
    override def at(i: I): Lens[IIDProbability[R, P, C], T] = Lens[IIDProbability[R, P, C], T](iid => pAt.at(i).get(iid.ps.head)) { t =>
      val set = pAt.at(i).set(t)
      ps[R, P, C].modify(_.map[P, C](set))
    }
  }

}
