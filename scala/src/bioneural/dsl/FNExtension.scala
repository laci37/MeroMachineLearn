package bioneural.dsl
import bioneural._
class FNExtension(n: FNNeuron) {
  def syn(n2: FNNeuron) =  new Synapse(n, n2)
  def hsyn(n2:FNNeuron)= new HebbSynapse(n,n2)

  def ic(d: Double) = new InputCurrent(n, d)
  
}