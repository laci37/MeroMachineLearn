package bioneural
import math._
trait LimitIC extends FNNeuron{
  override def I={
    val raw=super.I
    def sigma(x:Double)=1/(1+exp(-10*x))
    raw*(1-sigma(raw-1))+1.2*sigma(raw-1)
  }
}

class FNNeuronwLimitIC extends FNNeuron with LimitIC