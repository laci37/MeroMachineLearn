package knapsack
import java.io._
import actors._

class MainActor(size: Double, items: List[Item]) extends Actor {
  var i = 0
  var j = 1
  val testers = for (i <- (1 to 20)) yield new TestActor(size, items)
  val writer = new BufferedWriter(new FileWriter(new File("knap")))
  override def act() = {
    testers foreach { a => a.start(); assignJob(a) }
    while (i <= 5) {
      while (j <= 100) {
        receive {
          case t: (Int, Double, String) => {
            println("Test returned")
            writer.write(t._1 + " " + t._2 + " ")
            writer.write(t._3 + "\n")
            assignJob(sender)
          }
          case _ =>
        }
      }
      i += 1
      j = 1
    }
    while (!(testers forall { a => !a.gotJob })) {
      receive {
        case t: (Int, Double, String) => {
          println("Test returned")
          writer.write(t._1 + " " + t._2 + " ")
          writer.write(t._3 + "\n")
        }
        case _ =>
      }
    }
  }
  
  def assignJob(a: OutputChannel[Any]) {
    a ! Tuple2((math.pow(2, i)).toInt*1000, 0.2 * j)
    j += 1
  }
}