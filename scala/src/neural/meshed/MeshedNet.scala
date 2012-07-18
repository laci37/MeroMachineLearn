package neural.meshed
import neural._
import collection.parallel.mutable.ParArray
/**
 * describes a fully meshed neural network
 */
class MeshedNet(nInputs: Int, nNeurons: Int) extends Net {
  var inputs = new ParArray[Input](nInputs)
  var neurons = new ParArray[Neuron](nNeurons)

  override def outputs = neurons //all neurons can output
  override def outputs_=(value: ParArray[Neuron]) = neurons = value
}

object MeshedNet {
  /**
   * construct a MeshedNet of given size, with given weights
   */
  def apply(nInputs: Int, nNeurons: Int, weights: Array[Double]): MeshedNet = {
    val ret = new MeshedNet(nInputs, nNeurons)
    //create inputs
    for (i <- (0 to nInputs - 1)) {
      ret.inputs(i) = new Neuron(x => x) with Input
    }
    //create neurons, all of the use tanh(x) act func
    for (i <- (0 to nNeurons - 1)) {
      ret.neurons(i) = new Neuron(math.tanh)
    }
    
    //inner function for reading weights from the array
    var ptr = 0
    def getWeight() = {
      val ret = weights(ptr)
      ptr += 1
      ret
    }
    //create connections for all neurons
    ret.neurons foreach { n =>
      n.inputs += (ret.bias -> getWeight) //bias
      ret.neurons foreach { in =>  //connect with other neurons
        val w = getWeight
        if (w != 0d) //if the weight is zero do not create connection
          n.inputs += (in -> w)
      }
      ret.inputs foreach { in => //connect with inputs
        val w = getWeight
        if (w != 0d)
          n.inputs += (in -> w)
      }
    }

    ret //return the network
  }
}