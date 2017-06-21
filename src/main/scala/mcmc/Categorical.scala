package mcmc

import spire.algebra.{Field, Order}
import spire.random.{Dist, DistFromGen, Uniform}
import spire.syntax.field._
import spire.syntax.order._

trait Categorical[T, @specialized(Double) R] extends Any {

  def apply(probabilities: Map[T, R]): Dist[T]

}

object Categorical {

  @inline final def apply[T, @specialized(Double) R](implicit m: Categorical[T, R]): Categorical[T, R] = m

  def apply[T, @specialized(Double) R](probabilities: Map[T, R])(implicit m: Categorical[T, R]): Dist[T] = m(probabilities)

  implicit def any[T, /*@specialized(Double)*/ R : Field : Uniform : Order]: Categorical[T, R] = new Categorical[T, R] {
    override def apply(probabilities: Map[T, R]): Dist[T] = {
      val cumSum = {
        val cs = probabilities.toArray
        scala.util.Sorting.quickSort(cs)(Ordering.fromLessThan(Order.by[(T, R), R](_._2).gt))
        for (i <- cs.indices.tail)
          cs(i) = (cs(i)._1, cs(i - 1)._2 + cs(i)._2)
        cs
      }
      new DistFromGen[T]({ g =>
        val u = g.next[R](Uniform(Field[R].zero, cumSum.last._2))
        var i = 0
        while (i < cumSum.length && u > cumSum(i)._2) i += 1
        cumSum(i)._1
      })
    }
  }

}
