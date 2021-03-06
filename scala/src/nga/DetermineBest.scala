package nga

/**
 * extension trait for GenerationBase, determines the best Genome in all generations
 */
trait DetermineBest extends GenerationBase {
  
  /**
   * accessor method for the best Genome in the current generation
   */
  def best(): Genome = {
    test()
    _best.get
  }
  
  override def step()={
    super.step()
    if(resetBest) _best=None
  }
  
  /**
   * sets if the best must be reset on step
   */
  var resetBest=true
  
  protected var _best: Option[Genome] = None
  
  protected override def testMember(g: Genome) = {
    super.testMember(g)
    if (_best.isEmpty) _best = Some(g)
    else if (_best.get.fitness < g.fitness()) _best = Some(g)
  }
  
  override def toString()= super.toString + " with DetermineBest"
}