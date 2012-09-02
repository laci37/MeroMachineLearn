package bioneural.mem
import bioneural._
trait LimitICNeurons extends HopNet {
  protected override def createNeuron(i:Int)={
    neurons(i)=new FNNeuronwLimitIC
  }
}

