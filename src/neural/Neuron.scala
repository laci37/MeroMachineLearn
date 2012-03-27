package neural
import scala.collection
import scala.collection.mutable.Map

class Neuron(val actfunc: Double => Double) extends Serializable{
  //the inputs of the neuron, and their weights
  var inputs: Map[Neuron, Double] = Map[Neuron, Double]()
  var poutput = 0d
  var pnextout = 0d
  var debug = ""
  override def toString() = debug
  //the output of the neuron
  def output(): Double = poutput

  //calculates and stores the next output returns true if a NaN is present
  def calc(): Boolean = {
    var sum = 0d
    inputs.foreach(kv => sum += kv._1.output * kv._2)
    pnextout = actfunc(sum)
    pnextout.isNaN || pnextout.isInfinite
  }

  //changes the output to the precalculated value
  //returns true if there's a change
  def timestep(): Boolean = {
    var a = (poutput - pnextout)
    var ret = a != 0
    poutput = pnextout

    ret
  }
}

object Neuron {
  import scala.util.Random

  //initialize with given inputs and weights
  def apply(actfunc: Double => Double, inputs: Map[Neuron, Double]): Neuron = {
    var n = new Neuron(actfunc)
    n.inputs = inputs
    n
  }

  //initialize with given inputs and random weights
  def apply(actfunc: Double => Double, inputs: List[Neuron]): Neuron = {
    var n = new Neuron(actfunc)
    var rand = new Random
    inputs.foreach(in => (n.inputs += ((in, rand.nextDouble()))))
    n
  }
  
  implicit def Neuron2Double(n: Neuron):Double= n.output
}