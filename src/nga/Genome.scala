package nga

/**
 * each Genome object represents one allele
 */
trait Genome {
  /**
   * fitness of the allele, should be nonnegative for compatibility with GenerationBase
   * should be overriden with a lazy val 
   */
  def fitness():Double
  
  //hybridize operator
  def X(that: Genome):Genome
}