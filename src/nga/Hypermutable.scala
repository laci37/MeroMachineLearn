package nga
/**
 * trait for Genomes that can change their mutation rates to trigger hypermutation
 */
trait Hypermutable extends Genome{
  def hypermutate(): Boolean
  def hypermutate_=(value:Boolean)
}