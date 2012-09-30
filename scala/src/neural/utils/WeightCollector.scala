package neural.utils
import collection.mutable.ArrayBuffer
import neural.Net
import util.realtime.LargeDataCollector
trait ActWeightCollector extends Net with ActCollector {
  
  //set weight metadata
  System.arraycopy(weightmeta.toArray, 0, dc.metadata, inputs.length + neurons.length, weightmeta.length)
  weightmeta.clear
  var weightmeta: ArrayBuffer[String]=null //will be initialized from getCollectFunc before this constructor
  
  protected override def getCollectFunc() = {
    weightmeta = new ArrayBuffer[String]()
    super.getCollectFunc union
      (for (i ← (0 to neurons.size - 1)) yield (for (in ← neurons(i).inputs) yield {
        if (neurons.indexOf(in._1) != -1) weightmeta += "n"+neurons.indexOf(in._1)+" >> "+"n"+i
        else weightmeta += "i"+inputs.indexOf(in._1)+" >> "+"n"+i
        () ⇒ in._2
      })).flatten
  }

}