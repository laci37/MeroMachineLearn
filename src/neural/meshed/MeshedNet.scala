package neural.meshed
import neural._
import collection.parallel.mutable.ParArray
class MeshedNet(nInputs: Int, nNeurons: Int) extends Net {
  var inputs = new ParArray[Input](nInputs)
  var neurons = new ParArray[Neuron](nNeurons)

  override def outputs = neurons
  override def outputs_=(value: ParArray[Neuron]) = neurons = value
}

object MeshedNet {
  def apply(nInputs: Int, nNeurons: Int, weights: Array[Double]): MeshedNet = {
    val ret = new MeshedNet(nInputs, nNeurons)

    for (i <- (0 to nInputs - 1)) {
      ret.inputs(i) = new Neuron(x => x) with Input
    }

    for (i <- (0 to nNeurons - 1)) {
      ret.neurons(i) = new Neuron(x => math.tanh(x))
    }

    var ptr = 0
    def getWeight() = {
      val ret = weights(ptr)
      ptr += 1
      ret
    }

    ret.neurons foreach { n =>
      n.inputs += (ret.bias -> getWeight)
      ret.neurons foreach { in =>
        val w = getWeight
        if (w != 0d)
          n.inputs += (in -> w)
      }
      ret.inputs foreach { in =>
        val w = getWeight
        if (w != 0d)
          n.inputs += (in -> w)
      }
    }

    ret
  }
}