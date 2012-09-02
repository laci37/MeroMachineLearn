package bioneural.mem
import bioneural._
trait Detectors extends HopNet {
  val detect = new Array[Detector](size)
  for(i<-(0 to size-1)){
    detect(i)= new Detector(neurons(i))
    sim.objects.add(detect(i))
  }
}

class HopNetwDetect(size:Int) extends HopNet(size) with Detectors