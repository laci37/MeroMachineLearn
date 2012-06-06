package neural
import collection.parallel.mutable.ParArray
class MeshedNet(nInputs: Int, nNeurons: Int) extends Net {

  override var neurons =
    (for (i <- (1 to nNeurons)) yield new Neuron(x => math.tanh(x))).toArray.par
  override val inputs =
    (for (i <- (1 to nNeurons)) yield new Neuron(x => x) with Input).toArray.par
  override def outputs=neurons
  override def outputs_=(value:ParArray[Neuron])= neurons=value
    
  private def construct(nInputs: Int, nNeurons: Int) = {
    neurons foreach { n =>
      neurons foreach { in =>
        n.inputs += in -> 0d
      }
      inputs foreach { in =>
        n.inputs += in -> 0d
      }
    }
  }
}