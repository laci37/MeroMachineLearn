package nga

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
}