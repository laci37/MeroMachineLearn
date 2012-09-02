package bioneural.mem
import bioneural._
trait VDSynapses extends HopNet{
  override def createSynapse(i:Int,j:Int)={
    synapses(i)(j)= new VDHebbSynapse(neurons(i),neurons(j))
    sim.objects.add(synapses(i)(j))
  }
  
}