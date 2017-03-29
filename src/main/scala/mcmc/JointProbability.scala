package mcmc

import monocle.Lens
import monocle.function.At
import spire.NoImplicit
import spire.algebra.AdditiveMonoid
import spire.syntax.additiveMonoid._

class JointProbability[@specialized(Double) R : AdditiveMonoid, P <: Probability[R], Q <: Probability[R]](val p: P, val q: Q) extends Probability[R] {

  override lazy val evaluate: R = p.evaluate + q.evaluate

}

object JointProbability {

  implicit def p[R : AdditiveMonoid, P <: Probability[R], Q <: Probability[R]]: Lens[JointProbability[R, P, Q], P] =
    Lens[JointProbability[R, P, Q], P](_.p)(p => jp => new JointProbability(p, jp.q))

  implicit def q[R : AdditiveMonoid, P <: Probability[R], Q <: Probability[R]]: Lens[JointProbability[R, P, Q], Q] =
    Lens[JointProbability[R, P, Q], Q](_.q)(q => jp => new JointProbability(jp.p, q))

  implicit def left[R : AdditiveMonoid, P <: Probability[R], Q <: Probability[R], T](implicit p_t: Lens[P, T], q_t: NoImplicit[Lens[Q, T]]): Lens[JointProbability[R, P, Q], T] =
    p[R, P, Q] ^|-> p_t

  implicit def both[R : AdditiveMonoid, P <: Probability[R], Q <: Probability[R], T](implicit p_t: Lens[P, T], q_t: Lens[Q, T]): Lens[JointProbability[R, P, Q], T] =
    Lens[JointProbability[R, P, Q], T](jp => p_t.get(jp.p))(t => jp => new JointProbability(p_t.set(t)(jp.p), q_t.set(t)(jp.q)))

  implicit def right[R : AdditiveMonoid, P <: Probability[R], Q <: Probability[R], T](implicit p_t: NoImplicit[Lens[P, T]], q_t: Lens[Q, T]): Lens[JointProbability[R, P, Q], T] =
    q[R, P, Q] ^|-> q_t

  implicit def atLeft[R : AdditiveMonoid, P <: Probability[R], Q <: Probability[R], I, T](implicit pAt: At[P, I, T], qAt: NoImplicit[At[Q, I, T]]): At[JointProbability[R, P, Q], I, T] = new At[JointProbability[R, P, Q], I, T] {
    override def at(i: I): Lens[JointProbability[R, P, Q], T] = p[R, P, Q] ^|-> pAt.at(i)
  }

  implicit def atBoth[R : AdditiveMonoid, P <: Probability[R], Q <: Probability[R], I, T](implicit pAt: At[P, I, T], qAt: At[Q, I, T]): At[JointProbability[R, P, Q], I, T] = new At[JointProbability[R, P, Q], I, T] {
    override def at(i: I): Lens[JointProbability[R, P, Q], T] =
      Lens[JointProbability[R, P, Q], T](jp => pAt.at(i).get(jp.p))(t => jp => new JointProbability(pAt.at(i).set(t)(jp.p), qAt.at(i).set(t)(jp.q)))
  }

  implicit def atRight[R : AdditiveMonoid, P <: Probability[R], Q <: Probability[R], I, T](implicit pAt: NoImplicit[At[P, I, T]], qAt: At[Q, I, T]): At[JointProbability[R, P, Q], I, T] = new At[JointProbability[R, P, Q], I, T] {
    override def at(i: I): Lens[JointProbability[R, P, Q], T] = q[R, P, Q] ^|-> qAt.at(i)
  }

}
