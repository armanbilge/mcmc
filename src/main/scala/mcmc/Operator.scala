package mcmc

trait Operator[T, @specialized(Double) R] extends (T => T) {

  def hastingsRatio(x: T, y: T): R

}
