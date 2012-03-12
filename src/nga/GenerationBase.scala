package nga

class GenerationBase(override val members: Iterable[Genome]) extends Generation(members) {
  
  /**
   * creates a successor Generation
   */
  override def next() = {
    new GenerationBase(for (i <- (1 to members.size)) yield createMember())
  }

  /**
   * creates a member of the next Generation
   */
  protected def createMember(): Genome = {
    selectParent X selectParent
  }

  /**
   * chooses a parent by some heuristics
   * Base implementation is roulette wheel
   */
  protected def selectParent: Genome = {
    import annotation.tailrec
    test()
    val iter = members.iterator
    @tailrec def inner(g: Genome, p: Double): Genome = {
      var p2 = p
      p2 -= g.fitness
      if (p2 <= 0) { return g }
      inner(iter.next, p2)
    }
    inner(iter.next, fitsum)
  }

  protected var tested = false //true if the test has run
  protected var fitsum = 0d //the sum of fitnesses, used in roulette wheel

  protected def test() = {
    if (!tested) {
      members foreach testMember _
      tested = true
    }
  }

  protected def testMember(g: Genome) = {
    fitsum += g.fitness
  }
}