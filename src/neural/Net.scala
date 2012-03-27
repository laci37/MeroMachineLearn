package neural
import scala.collection.parallel.mutable.ParArray
import scala.annotation.tailrec
trait Net extends Serializable{
  var neurons: ParArray[Neuron] //all calculating neurons
  var inputs: ParArray[Input] //inputs 
  var outputs: ParArray[Neuron] //outputs, also referenced in neurons

  val bias: Input = new Neuron(x => x) with Input //bias input
  bias.set(1)

  //makes a timestep, returns true if the state changes
  def timestep(): TimestepReturns = {
    import TimestepReturns._

    var overflow = false
    neurons foreach {
      n =>
        overflow |= n.calc()
    }

    var change = false
    neurons foreach (n => change |= n.timestep())

    if (overflow) Overflow
    else if (change) { Change }
    else NoChange
  }
  /**
   * calculates a stable output, returns true if no overflow happens
   */
  def calc(): Boolean = {
    import TimestepReturns._
    @tailrec def inner(t: TimestepReturns): Boolean = {
      t match {
        case Change => Unit
        case NoChange => return true
        case Overflow => return false
      }
      inner(timestep())
    }
    inner(Change)
  }

}

abstract class TimestepReturns

object TimestepReturns {
  case object NoChange extends TimestepReturns
  case object Change extends TimestepReturns
  case object Overflow extends TimestepReturns
}