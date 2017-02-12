package mcmc

import spire.algebra.{Order, Field, Trig}
import spire.random.{Generator, Uniform}
import spire.syntax.additiveGroup._
import spire.syntax.order._

import scala.collection.TraversableOnce

class MCMC[@specialized(Double) R : Field : Trig : Order : Uniform, P <: Probability[R]](val operators: Map[Operator[P, R], R])(implicit val rng: Generator) {

  val distAlpha = Uniform(Field[R].zero, Field[R].one).map(Trig[R].log)
  val distOp = Multinomial(operators)

  def chain(start: P): TraversableOnce[P] = Iterator.iterate(start) { p =>
    val op = rng.next[Operator[P, R]](distOp)
    val pp = op(p)
    val alpha = Field[R].zero min (pp.evaluate - p.evaluate + op.hastingsRatio(p, pp))
    if (rng.next[R](distAlpha) < alpha) pp else p
  }

}
