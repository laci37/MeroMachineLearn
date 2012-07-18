package knapsack

import actors.Actor
import optimization._
class TestActor(size:Double,items:List[Item]) extends Actor {
  def gotJob():Boolean=job
  private var job=false
  override def act() = {
    loop {
      receive {
        case t: (Int, Double) => {
          job=true
          var opt = new Optimizer(new KnapState(size,items,Nil),0.2*t._2)
          var str = ""
          for (i <- (1 to 50)) {
            opt.optimize(t._1)
            str += opt.optimalCost + " "
            opt = new Optimizer(new KnapState(size,items,Nil),0.2*t._2)
          }
          sender ! (t._1,t._2,str)
          job=false
          
        }
        case _ =>
      }
    }
  }
}