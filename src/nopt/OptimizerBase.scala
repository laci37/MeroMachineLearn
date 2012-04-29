package nopt
import annotation.tailrec
class OptimizerBase(init: State) extends Optimizer(init) {

  def optimize(cycles: Int): State = {
    //inner function to enable tailrec
    @tailrec def inner(c: Int): State = {
      if (c <= 1) return optCycle()
      optCycle()
      inner(c - 1)
    }
    //call inner
    inner(cycles)
  }

  def optimize(stopWhen: (State, Int) => Boolean): State = {
    //inner function to enable tailrec
    @tailrec def inner(c: Int): State = {
      val state = optCycle()
      if (stopWhen(state, c)) return state
      inner(c + 1)
    }
    //call inner
    inner(1)
  }

  //optimization variables
  protected var optimum: State = init
  protected var curState = init
  protected var k = 0d

  /**
   * The body of the optimization cycle
   */
  protected def optCycle(): State = {
    val newState = curState.neighbor()
    collectHeuristicData(newState)
    if (newState.energy - curState.energy < k) {
      curState = newState
      if (newState.energy < optimum.energy) optimum = newState
    } else k += arise
    optimum
  }

  //variables for arise heuristics
  protected var sumdiff = 0d
  protected var cdiff = 0
  protected var div = 2d

  /**
   * collects data for arise heuristic
   */
  protected def collectHeuristicData(newState: State) = {
    sumdiff += math.abs(curState.energy - newState.energy)
    cdiff += 1
  }

  def arise: Double = (sumdiff / cdiff) / div
}