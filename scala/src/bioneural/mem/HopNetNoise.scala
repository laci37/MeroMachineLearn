package bioneural.mem
import bioneural._
trait HopNetNoise extends HopNet {
  override def createNeuron(i:Int)={
    neurons(i)= new FNNeuron with ICNoise
  }
}