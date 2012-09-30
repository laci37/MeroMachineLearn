package bioneural.mem
import bioneural._
trait Detectors extends HopNet {
  val detect = new Array[Detector](size)
  for (i ← (0 to size - 1)) {
    detect(i) = new Detector(neurons(i))
    sim.objects.add(detect(i))
  }

  override def getCollectFunc() = {
    super.getCollectFunc union (for (d ← detect) yield () ⇒ d.f)
  }
  
  override def getCollectMeta() = {
    super.getCollectMeta union (for (i ← (0 to size - 1)) yield "d.f("+i+")")
  }

}

class HopNetwDetect(size: Int) extends HopNet(size) with Detectors