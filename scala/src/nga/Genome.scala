package nga

/**
 * The base trait for any "creature" in GA
 */
trait Genome extends Serializable{
  /**
   * fitness of the genome, must be nonnegative for compatibility with GenerationBase
   * should be overriden with a lazy val 
   */
  def fitness():Double
  
  /**
   * hybridize operator
   */
  def X(that: Genome):Genome
}