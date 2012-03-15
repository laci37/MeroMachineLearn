package nga

trait DetermineBest extends GenerationBase {
  
  def best(): Genome = {
    test()
    _best.get
  }
  
  protected var _best: Option[Genome] = None
  
  protected override def testMember(g: Genome) = {
    super.testMember(g)
    if (_best.isEmpty) _best = Some(g)
    else if (_best.get.fitness < g.fitness()) _best = Some(g)
  }

}