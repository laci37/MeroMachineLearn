package nga
/**
 * base class for Genomes working with an array of doubles as data
 */
abstract class DoubleArrayGenome(val data: Array[Double]) extends Hypermutable {
  val rand = scala.util.Random
  private var _xover = 0.5d
  /**
   * the rate of crossovers
   */
  def xover = _xover

  private var _mut = 0.1d
  /**
   *  rate of mutation
   */
  def mut = _mut
  /**
   *   magnitude of mutations
   */
  def mut2() = 0.1d

  /**
   * logic behind hybridize operator, calculates new set of Doubles
   */
  protected def calcData(that: Array[Double]): Array[Double] = {
    if (that.size != data.size) throw new IllegalArgumentException("The size of the two arrays must be equal!")
    var p = this.data //the parent currently being copied 
    val newdata = for (i <- Range(0, data.length)) yield {
      if (rand.nextDouble() < xover) { //change parent with crossover
        if (p == this.data) p = that
        else p = this.data
      }
      mutateDouble(p(i))
    }
    newdata.toArray
  }
  
  protected def mutateDouble(d:Double):Double={
    if (rand.nextDouble() < mut) d + (rand.nextGaussian) * mut2
    else d
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
      case that: DoubleArrayGenome => create(calcData(that.data))
      case _ => throw new IllegalArgumentException("that must be of type DoubleArrayGenome")
    }
  }

  private var _hyper = false
  override def hypermutate() = _hyper
  override def hypermutate_=(value: Boolean) = {
    _hyper = value
    if (_hyper) {
      _mut = 0.5d
    } else {
      _mut = 0.1d
    }
  }
}