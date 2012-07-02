package nopt
/**
 * abstract base for all optimizers
 */
abstract class Optimizer(val init: State) {
  def optimize(cycles: Int):State
  def optimize(stopWhen: (State, Int) => Boolean):State
}