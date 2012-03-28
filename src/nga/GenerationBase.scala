package nga

/**
 * Basic implementation of a generation
 */
class GenerationBase(initMembers: Iterable[Genome]) extends Generation(initMembers) {

  override def step() = {
    _members = for (i <- (1 to members.size)) yield { createMember }
    tested = false
  }

  /**
   * creates a member of the next generation
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
    val rand = util.Random
    test()
    val iter = members.iterator
    @tailrec def inner(g: Genome, p: Double): Genome = {
      var p2 = p
      p2 -= g.fitness
      if (p2 <= 0) { return g }
      inner(iter.next, p2)
    }
    inner(iter.next, rand.nextDouble * fitsum)
  }

  protected var tested = false //true if the test has run
  protected var fitsum = -1d //the sum of fitnesses, used in roulette wheel

  /**
   * tests the whole generation
   */
  protected def test() = {
    if (!tested) {
      fitsum = 0d
      members foreach testMember _
      tested = true
    }
  }

  /**
   * tests a single member
   */
  protected def testMember(g: Genome) = {
    fitsum += g.fitness
  }
  
  override def toString()="GenerationBase"
}