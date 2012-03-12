package nga
/**
 * base class for Genomes working with an array of doubles as data
 */
abstract class DoubleArrayGenome(val data: Array[Double]) extends Genome {
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
   * logic behind hybridize operator, calculates new set of Doubles
   */
  protected def calcData(that: Array[Double]): Array[Double] = {
    if (that.size != data.size) throw new IllegalArgumentException("The size of the two arrays must be equal!")
    val rand = util.Random
    var p = this.data //the parent currently being copied 
    val newdata = for (i <- Range(0, data.length)) yield {
      if (rand.nextDouble() < xover) { //change parent with crossover
        if (p == this.data) p = that
        else p = this.data
      }
      if (rand.nextDouble() < mut) p(i) + (rand.nextGaussian) * mut2 //mutation
      else p(i) //normal copy
    }
    newdata.toArray
  }

  /**
   * abstract method for creating an instance from data
   */
  protected def create(data: Array[Double]): this.type

  /**
   *  hybridize, that must be of this.type
   */
  override def X(that: Genome) = {
    that match {
      case that: this.type => create(calcData(that.data))
      case _ => throw new IllegalArgumentException("that must be of type DoubleArrayGenome")
    }
  }
}