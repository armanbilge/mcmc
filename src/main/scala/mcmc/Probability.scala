package mcmc

trait Probability[@specialized(Double) R] {

  def evaluate: R

}
