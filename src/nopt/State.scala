package nopt

/**
 * trait for all optimization states
 */
trait State {
  def energy(): Double
  def neighbor(): State
}