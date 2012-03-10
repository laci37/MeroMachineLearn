package tsp

import actors.Actor
import java.io._
import collection.mutable.ArrayBuffer
class MainActor(val graph: CityGraph) extends Actor {
  var i = 6
  var j = 1
  val testers = for(i<-(1 to 20)) yield new TestActor(graph)
  val writer = new BufferedWriter(new FileWriter(new File("test3")))
  override def act()={
    testers foreach {a=> a.start(); assignJob(a)}
    while(i<=6) {
      while(j<=1000){
         receive {
          case t: (Int, Double, String, TestActor) => {
            println("Test returned")
            writer.write(t._1 + " " + t._2 + " ")
            writer.write(t._3 + "\n")
            writer.flush()
            assignJob(t._4)
          }
          case _ =>
        }
      }
      i+=1
      j=1
    }
    while(!(testers forall {a => !a.gotJob})){
      receive {
          case t: (Int, Double, String, TestActor) => {
            println("Test returned")
            writer.write(t._1 + " " + t._2 + " ")
            writer.write(t._3 + "\n")
            writer.flush()
          }
          case _ =>
        }
    }
  }
  
  def assignJob(a:TestActor){
    a ! Tuple3((1000d * math.pow(2, i)).toInt, 0.02 * j, this)
    j+=1
  }
}