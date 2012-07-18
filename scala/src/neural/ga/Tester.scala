package neural.ga

/**
 * base class for testing networks, subclasses should be singletons
 */
abstract class Tester {
  /**
   * tests a Genome, returns the fitness score
   * fitness score must be >=0
   */
  def test(g: Genome): Double
}