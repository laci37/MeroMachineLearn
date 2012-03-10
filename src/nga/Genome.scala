package nga

trait Genome {
  //must be non-negative
  def fitness():Double
  
  //hybridize operator
  def X(that: this.type):this.type
}