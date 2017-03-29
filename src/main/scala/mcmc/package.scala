import monocle.Lens

package object mcmc {

  type OperatorCoercer[T, @specialized(Double) R, O <: Operator[T, R]] = Lens[O, R]
  def OperatorCoercer[T, @specialized(Double) R, O <: Operator[T, R]](get: O => R)(set: R => O => O): OperatorCoercer[T, R, O] = Lens(get)(set)

}
