package optimization
import annotation.tailrec

class HeuristicRiseOptimizer(init: State) extends Optimizer(init, 0d) {

  override def acceptRise = (diffsum / diffc) / 2
  private var diffsum = 0d
  private var diffc = 0

  override def optimize(cycles: Int) = {

    @tailrec
    def inner(cycles: Int): State = {
      if (cycles <= 0) return curState;
      val newState = curState.neighbor
      val newCost = newState.cost
      diffsum += math.abs(newCost - curCost)
      diffc += 1
      if (newCost - curCost < accept) {
        curState = newState
        curCost = newCost
        if (newCost < optimalCost) {
          optimum = newState
          optimalCost = newCost
        }
        accept = 0d
      } else accept += acceptRise
      inner(cycles - 1)
    }

    inner(cycles)
  }
}