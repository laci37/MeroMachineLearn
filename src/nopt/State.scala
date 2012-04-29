package nopt

trait State {
  def energy(): Double
  def neighbor(): State
}