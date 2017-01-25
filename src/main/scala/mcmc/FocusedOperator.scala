package mcmc

import monocle.Lens

final class FocusedOperator[S, T, @specialized(Double) R](val op: Operator[T, R], val lens: Lens[S, T]) extends Operator[S, R] {

  def apply(s: S): S = lens.modify(op)(s)

  def hastingsRatio(x: S, y: S): R = op.hastingsRatio(lens.get(x), lens.get(y))

}
