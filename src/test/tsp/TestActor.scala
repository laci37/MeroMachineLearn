package tsp

import actors.Actor
import optimization._
class TestActor(val graph: CityGraph) extends Actor {
  def gotJob(): Boolean = job
  private var job = false
  override def act() = {
    loop {
      receive {
        case t: (Int, Double, Actor) => {
          job = true
          var opt = new Optimizer(new TSPState(graph.cities, graph), 0.2 * t._2)
          var str = ""
          for (i <- (1 to 50)) {
            opt.optimize(t._1)
            str += opt.optimalCost + " "
            opt = new Optimizer(new TSPState(graph.cities, graph), 0.2 * t._2)
          }
          t._3 ! (t._1, t._2, str, this)
          job = false

        }
        case _ =>
      }
    }
  }
}