package neural.ga
import neural.dsl._
class AdaptiveGenome(override val data: Array[Double], l: Layer) extends Genome(data, l) {
  override def mut = 1 / data.size
}

object AdaptiveGenome {
  import collection.mutable.Seq
  /**
   * generate a random member from the given plans
   */
  def random(l: Layer): AdaptiveGenome = {
    var ndata = 0
    var done = Seq.empty[Layer]
    def discover(l: Layer): Unit = {
      if (!done.contains(l)) {
        done = done :+ l
        if (l.inputs.size != 0) ndata += l.size //bias
        l.inputs.foreach(l2 => {
          ndata += l.size * l2.size
          discover(l2)
        })
      }
    }
    discover(l)
    var data = new Array[Double](ndata)
    val rand = util.Random
    for (i <- Range(0, ndata)) { data(i) = rand.nextDouble() - 0.5 }
    new AdaptiveGenome(data, l)
  }

  def zero(l: Layer): AdaptiveGenome = {
    var ndata = 0
    var done = Seq.empty[Layer]
    def discover(l: Layer): Unit = {
      if (!done.contains(l)) {
        done = done :+ l
        if (l.inputs.size != 0) ndata += l.size //bias
        l.inputs.foreach(l2 => {
          ndata += l.size * l2.size
          discover(l2)
        })
      }
    }
    discover(l)
    var data = new Array[Double](ndata)
    for (i <- Range(0, ndata)) { data(i) = 0 }
    new AdaptiveGenome(data, l)
  }
}