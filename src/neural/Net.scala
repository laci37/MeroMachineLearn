package neural
import scala.collection.parallel.mutable.ParArray
import scala.annotation.tailrec

/**
 * Base trait for all Neural networks
 */
trait Net extends Serializable {
  var neurons: ParArray[Neuron] //all calculating neurons
  var inputs: ParArray[Input] //inputs 
  var outputs: ParArray[Neuron] //outputs, they are also referenced in neurons

  val bias: Input = new Neuron(x => x) with Input //bias input, always set to 1
  bias.set(1)

  /**
   * makes a single timestep
   */
  def timestep(): TimestepReturns = {
    import TimestepReturns._ //import case classes

    var overflow = false //will be set true if any neurons overflows
    neurons foreach {
      n =>
        overflow |= n.calc()
    }

    var change = false //will be set true if any neurons changes its output
    neurons foreach (n => change |= n.timestep())

    if (overflow) Overflow
    else if (change) { Change }
    else NoChange
  }
  
  /**
   * calculates a stable output, returns true if no overflow happens
   */
  def calc(): Boolean = {
    import TimestepReturns._ // import case classes
    
    //inner loop function, must be defined here to use tailrec without final
    @tailrec def inner(t: TimestepReturns): Boolean = {
      t match {
        case Change => Unit //if the state changed continue
        case NoChange => return true //if it hasn't stop and return true
        case Overflow => return false //if there was an overflow return false 
      }
      inner(timestep())
    }
    
    //call the loop
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
  
  /**
   * resets all neurons
   */
  def reset() = {
    neurons foreach { n => n.reset }
  }
  
}

// abstract class and case classes used in Net.calc and Net.timestep
abstract class TimestepReturns

object TimestepReturns {
  case object NoChange extends TimestepReturns
  case object Change extends TimestepReturns
  case object Overflow extends TimestepReturns
}