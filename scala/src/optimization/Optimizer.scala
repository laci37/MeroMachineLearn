package optimization
import annotation.tailrec
class Optimizer(val init: State, _acceptRise: Double) extends Serializable{
  var curState = init
  var curCost: Double = curState.cost
  var accept: Double = 0d
  var optimum = init
  var optimalCost = curCost

  def acceptRise = _acceptRise

  def optimizeWhile(limCycles:Int, stopCondition:(State=>Boolean)): State = {
    @tailrec def inner(limCycles:Int, stopCondition:(State=>Boolean)):State={
      if (limCycles<=0 || stopCondition(optimum)) return optimum;
      optimize(1)
      inner(limCycles-1,stopCondition)
    }
    inner(limCycles,stopCondition)
  }
  
  def optimize(cycles: Int): State = {

    @tailrec
    def inner(cycles: Int): State = {
      if (cycles <= 0) return optimum;
      val newState = curState.neighbor
      val newCost = newState.cost
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