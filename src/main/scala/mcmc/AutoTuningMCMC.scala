package mcmc

import mcmc.AutoTuningMCMC.OperatorState
import spire.algebra.{Field, Order, Trig}
import spire.random.{Generator, Uniform}
import spire.syntax.field._
import spire.syntax.order._

import scala.collection.TraversableOnce
import scala.language.existentials

class AutoTuningMCMC[@specialized(Double) R : Field : Trig : Order : Uniform, P <: Probability[R]](val operators: Set[OperatorState[P, R, O] forSome {type O <: Operator[P, R]}])(implicit val rng: Generator) {

  val distAlpha = Uniform(Field[R].zero, Field[R].one).map(Trig[R].log)

  def chain(start: P): TraversableOnce[P] = Iterator.iterate((start, operators))(Function.tupled { (p, ops) =>
    val op = rng.next(Multinomial(ops.map(op => op -> op.weight).toMap[OperatorState[P, R, O] forSome {type O <: Operator[P, R]}, R]))
    val pp = op.op(p)
    val opp = op.operated
    val alpha = Field[R].zero min (pp.evaluate - p.evaluate + op.op.hastingsRatio(p, pp))
    val oppp = op.coercion.map { x =>
      val c = Trig[R].log(Field[R].fromInt(opp.count)) + 1
      val xp = x + 1 / c * (Trig[R].exp(alpha) - opp.target)
      opp.coerced(xp)
    }.getOrElse(opp)
    (if (rng.next[R](distAlpha) < alpha) pp else p, ops - op + oppp)
  }).map(_._1)

}

object AutoTuningMCMC {

  class OperatorState[P, @specialized(Double) R, O <: Operator[P, R]](val op: O, val weight: R, val target: R = 0.234, val coercer: Option[OperatorCoercer[P, R, O]] = None, val count: Int = 0) {

    def coercion: Option[R] = coercer.map(_.get(op))

    def coerced(x: R): OperatorState[P, R, O] = new OperatorState[P, R, O](coercer.map(_.set(x)(op)).getOrElse(op), weight, target, coercer, count)

    def operated: OperatorState[P, R, O] = new OperatorState[P, R, O](op, weight, target, coercer, count + 1)

  }

}
