package sa
import annotation.tailrec
class Optimizer(val prob: Problem) {
  var curState = prob.initialState
  var temp = prob.initialTemp
  var optimum = curState
  val rand = util.Random

  def optimize():AnyRef = {
    @tailrec def inner(): State = {
      val neighbor = curState.neighbor
      if (prob.probability(curState, neighbor, temp) > rand.nextDouble()) {
        curState = neighbor
        if (curState.energy < optimum.energy) optimum = curState
      }
      temp = prob.cool(temp)
      if (temp <= 0) return optimum
      inner()
    }
    inner()
  }
}