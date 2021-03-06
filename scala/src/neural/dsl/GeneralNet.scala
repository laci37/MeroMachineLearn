package neural.dsl

import neural._
import collection.parallel.mutable.ParArray
/**
 * The most basic definition of a neural Net
 */
class GeneralNet(val nInputs: Int, val nNeurons: Int, val nOutputs: Int) extends Net {
  override var inputs = new ParArray[Input](nInputs)
  override var neurons = new ParArray[Neuron](nNeurons)
  override var outputs = new ParArray[Neuron](nOutputs)
}