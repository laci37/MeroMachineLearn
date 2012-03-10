package neural.ga
import neural.dsl._
import neural._
/**
 * class for storing neural net data
 */
class Genome(val data: Array[Double], val l: Layer) {
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
   * variable for holding the fitness of the chromosome
   */
  var fit: Double = -1

  /**
   * hybridize operator
   */
  def X(other: Genome): Genome = {
    val rand = util.Random
    var p = this //the parent currently being copied 
    val newdata = for (i <- Range(0, data.length)) yield {
      if (rand.nextDouble() < xover) {
        if (p == this) p = other
        else p = this
      }
      if (rand.nextDouble() < mut) p.data(i) + (rand.nextGaussian) * mut2
      else p.data(i)
    }
    new Genome(newdata.toArray, l)
  }

  /**
   *  decode the genome to a network
   */
  def decode(bdr: NetworkBuilder): Net = {
    bdr.build(data)
  }

  /**
   *  decode the genome to a network with default RecursiveBuilder
   */
  def decode(): Net = {
    val bdr = new RecursiveNetworkBuilder(l)
    bdr.build(data)
  }
}

object Genome {
  import collection.mutable.Seq
  /**
   * generate a random member from the given plans
   */
  def random(l: Layer): Genome = {
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
    new Genome(data, l)
  }

  def zero(l: Layer): Genome = {
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
    new Genome(data, l)
  }
}