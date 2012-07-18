package nga
/**
 * extension for GenerationBase, the best Genome in each generation is copied over to the next
 */
trait Elitism extends GenerationBase with DetermineBest {
  protected var firstmember = true //true before the first member is created
  
  override def step() = {
    firstmember = true
    super.step()
  }
  
  override def createMember()={
    if(firstmember){
      firstmember=false
      best
    } else super.createMember
  }
  
  override def toString()= super.toString + " with Elitism"
}