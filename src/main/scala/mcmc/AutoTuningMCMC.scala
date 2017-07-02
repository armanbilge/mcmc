package mcmc

import spire.NoImplicit
import spire.algebra.{Field, Order, Trig}
import spire.random.{Generator, Uniform}
import spire.syntax.field._
import spire.syntax.order._

import scala.collection.TraversableOnce
import scala.language.existentials

object AutoTuningMCMC {

  def chain[@specialized(Double) R : Field : Trig : Order : Uniform, P <: Probability[R]](start: P, operators: IndexedSeq[OperatorState[P, R, O] forSome {type O <: Operator[P, R]}])(implicit rng: Generator): TraversableOnce[(P, IndexedSeq[OperatorState[P, R, O] forSome {type O <: Operator[P, R]}])] = {
    val distAlpha = Uniform(Field[R].zero, Field[R].one).map(Trig[R].log)
    Iterator.iterate((start, operators))(Function.tupled { (p, ops) =>
      val i = rng.next(Categorical((ops.indices, ops.view.map(_.weight)).zipped.toMap))
      val op = ops(i)
      val pp = op.op(p)
      val alpha = Field[R].zero min (pp.evaluate - p.evaluate + op.op.hastingsRatio(p, pp))
      (if (rng.next[R](distAlpha) < alpha) pp else p, ops.updated(i, op.operated(alpha)))
    })
  }

  def statify[P, @specialized(Double) R : Field : Trig, O <: Operator[P, R]](op: O, weight: R, target: R = 0.234)(implicit f: (O, R, R) => OperatorState[P, R, O]): OperatorState[P, R, O] =
    f(op, weight, target)

  trait OperatorState[P, @specialized(Double) R, O <: Operator[P, R]] {
    def op: O
    def weight: R
    def operated(alpha: R): OperatorState[P, R, O]
  }

  implicit def nonTunable[P, @specialized(Double) R, O <: Operator[P, R]](implicit noCoercer: NoImplicit[OperatorCoercer[P, R, O]]): (O, R, R) => OperatorState[P, R, O] =
    (op: O, weight: R, target: R) => new NonTunable[P, R, O](op, weight)

  private class NonTunable[P, @specialized(Double) R, O <: Operator[P, R]](val op: O, val weight: R) extends OperatorState[P, R, O] {
    def operated(alpha: R): OperatorState[P, R, O] = this
    override def toString: String = s"NonTunable($op, $weight)"
  }

  implicit def tunable[P, @specialized(Double) R : Field : Trig, O <: Operator[P, R]](implicit coercer: OperatorCoercer[P, R, O]): (O, R, R) => OperatorState[P, R, O] =
    (op: O, weight: R, target: R) => new Tunable(op, weight, target, coercer)

  private class Tunable[P, @specialized(Double) R : Field : Trig, O <: Operator[P, R]](val op: O, val weight: R, val target: R = 0.234, val coercer: OperatorCoercer[P, R, O], val count: Int = 0) extends OperatorState[P, R, O] {
    def operated(alpha: R): OperatorState[P, R, O] = {
      val x = coercer.get(op)
      val c = Trig[R].log(Field[R].fromInt(count)) + 1
      val xp = x + 1 / c * (Trig[R].exp(alpha) - target)
      new Tunable[P, R, O](coercer.set(xp)(op), weight, target, coercer, count + 1)
    }
    override def toString: String = s"Tunable($op, $weight, ${coercer.get(op)})"
  }

}
