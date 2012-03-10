package nga
//base class for Genomes working with an array of doubles as data
abstract class DoubleArrayGenome(val data:Array[Double]) extends Genome{
  /**
   * the rate of crossovers
   */
  def xover = 0.5d
  /**
   *  rate of mutation
   */
  def mut = 0.1d
  /**
   *   magnitude of mutations
   */
  def mut2() = 0.1d

  /**
   * hybridize operator
   */
  override def X(that: this.type):this.type
    val rand = util.Random
    var p = this //the parent currently being copied 
    val newdata = for (i <- Range(0, data.length)) yield {
      if (rand.nextDouble() < xover) {
        if (p == this) p = that
        else p = this
      }
      if (rand.nextDouble() < mut) p.data(i) + (rand.nextGaussian) * mut2
      else p.data(i)
    }
    new this.type(newdata.toArray)
  }
}