package nga

abstract class Generation(initMembers: Iterable[Genome]) extends Serializable{
  /**
   * step to the next generation
   */
  def step()

  /**
   * members of the current generation
   */
  def members(): Iterable[Genome] = _members

  protected var _members = initMembers
}