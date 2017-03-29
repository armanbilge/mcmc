package mcmc

import monocle.Lens

final class FocusedOperator[S, T, @specialized(Double) R, O <: Operator[T, R]](val op: O, val lens: Lens[S, T]) extends Operator[S, R] {

  def apply(s: S): S = lens.modify(op)(s)

  def hastingsRatio(x: S, y: S): R = op.hastingsRatio(lens.get(x), lens.get(y))

}

object FocusedOperator {

  implicit def coercer[S, T, R, O <: Operator[T, R]](implicit coercer: OperatorCoercer[T, R, O]): OperatorCoercer[S, R, FocusedOperator[S, T, R, O]] =
    op ^|-> coercer

  def op[S, T, @specialized(Double) R, O <: Operator[T, R]]: Lens[FocusedOperator[S, T, R, O], O] =
    Lens[FocusedOperator[S, T, R, O], O](_.op)(op => fo => new FocusedOperator(op, fo.lens))

}
