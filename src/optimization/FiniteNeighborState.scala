package optimization
/**
 * State with support for extended heuristics
 */
abstract class FiniteNeighborState extends State{
  /**
   * returns the number of neighboring States
   */
  def nNeighbors: Int
  
  /**
   * returns an Iterator with all neighbors
   */
  def neighborIterator(): Iterator[FiniteNeighborState]
}