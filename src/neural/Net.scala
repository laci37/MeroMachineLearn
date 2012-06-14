package neural
import scala.collection.parallel.mutable.ParArray
import scala.annotation.tailrec
trait Net extends Serializable {
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
  
  /**
   * calculates a stable output, with at most cycles timesteps
   * returns true if no overflow happens
   */
  def calc(cycles:Int):Boolean ={
     import TimestepReturns._
    @tailrec def inner(t: TimestepReturns,c:Int): Boolean = {
      if(c<=0) return true
      t match {
        case Change => Unit
        case NoChange => return true
        case Overflow => return false
      }
      inner(timestep(),c-1)
    }
    inner(Change,cycles)
  }
  
  def reset() = {
    neurons foreach { n => n.reset }
  }
}

abstract class TimestepReturns

object TimestepReturns {
  case object NoChange extends TimestepReturns
  case object Change extends TimestepReturns
  case object Overflow extends TimestepReturns
}