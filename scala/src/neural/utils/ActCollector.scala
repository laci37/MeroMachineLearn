package neural.utils
import neural._
import util.realtime.{ LargeDataCollector ⇒ RTCollector }
trait ActCollector extends Net {
  
  val dc = new RTCollector(getCollectFunc)

  //fill metadata
  for(i<-(0 to inputs.size-1)) dc.metadata(i)="i"+i
  for(i<-(inputs.size to inputs.size+neurons.size-1)) dc.metadata(i)="n"+(i-inputs.size)
  
  protected def getCollectFunc() = {
    (for (i ← inputs) yield () ⇒ i.output).seq union 
    (for (n ← neurons) yield () ⇒ n.output).seq
  }

  override def timestep() = {
    dc.timestep(1)
    super.timestep()
  }
}