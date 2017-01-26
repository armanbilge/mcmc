package mcmc

import spire.algebra.{Order, Ring}
import spire.random.{Dist, DistFromGen, Uniform}
import spire.syntax.order._
import spire.syntax.ring._

trait Multinomial[T, @specialized(Double) R] extends Any {

  def apply(probabilities: Map[T, R]): Dist[T]

}

object Multinomial {

  @inline final def apply[T, @specialized(Double) R](implicit m: Multinomial[T, R]): Multinomial[T, R] = m

  def apply[T, @specialized(Double) R](probabilities: Map[T, R])(implicit m: Multinomial[T, R]): Dist[T] = m(probabilities)

  implicit def any[T, @specialized(Double) R : Ring : Uniform : Order]: Multinomial[T, R] =
    (probabilities: Map[T, R]) => {
      val sortedProbabilities = probabilities.toArray.sortWith(Order.by[(T, R), R](_._2).gt).toIndexedSeq
      new DistFromGen[T]({ g =>
        val u = g.next[R](Uniform(Ring[R].zero, Ring[R].sum(probabilities.values)))
        val iter = sortedProbabilities.toIterator
        var current = iter.next()
        var cumsum = current._2
        while (cumsum > u && iter.hasNext) {
          current = iter.next()
          cumsum += current._2
        }
        current._1
      }
    )}

}
