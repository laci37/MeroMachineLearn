package neural.mem
import neural._
import scala.collection.parallel.mutable.ParArray

class ContextHopNet(size: Int, nContextUnits: Int, initWeight: Double)
  extends HopNet(size, initWeight) {

  def this(size: Int, nContextUnits: Int) = this(size, nContextUnits, 0d)

  neurons = new ParArray[Neuron](size + nContextUnits)
  override def outputs = neurons.slice(0, size)

  init
  override def init() = {
    super.init()
    //create context units with small random positive input weights
    val rand = scala.util.Random
    for (i <- (size to neurons.size - 1)) {
      neurons(i) = new HebbNeuron()
      for (j <- (0 to i - 1)) {
        neurons(i).inputs += (neurons(j) -> rand.nextDouble * (10d/size))
        neurons(j).inputs += (neurons(i) -> initWeight)
      }
    }
  }

}