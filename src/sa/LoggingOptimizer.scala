package sa
import annotation.tailrec
import collection.mutable.ArrayBuffer
class LoggingOptimizer(prob:Problem) extends Optimizer(prob){
  override def optimize()={
    val log=new ArrayBuffer[State]()
     @tailrec def inner(): AnyRef= {
      val neighbor = curState.neighbor
      log+=optimum
      if (prob.probability(curState, neighbor, temp) > rand.nextDouble()) {
        curState = neighbor
        if (curState.energy < optimum.energy) optimum = curState        
      }
      temp = prob.cool(temp)
      if (temp <= 0) return log.toIndexedSeq
      inner()
    }
    inner()
  }
}