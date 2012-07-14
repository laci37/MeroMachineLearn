package neural.meshed
import nga._
import neural._

/**
 * Genome for static sized meshed networks
 */
class StaticMeshGenome(nInputs: Int, nNeurons: Int, weights: Array[Double], test: (Net => Double)) extends DoubleArrayGenome(weights)
  with Decodable[MeshedNet] {

  def decode() = MeshedNet(nInputs, nNeurons, weights)

  def create(data: Array[Double]) = new StaticMeshGenome(nInputs, nNeurons, data, test).asInstanceOf[this.type]

  def fitness() = test(decode)

  override def mutateDouble(d: Double) = {
    import math._
    if (rand.nextDouble < 0.2) d * (rand.nextDouble() + 0.5)
    else if (rand.nextDouble < 0.01) 0d
    else if (rand.nextDouble < (1d / weights.size)) {
      val change = (rand.nextDouble - 0.5) * 0.1
      if (signum(d) == signum(change) && abs(d) < abs(change)) 0d
      else d - change
    } else d
  }

}

/**
 * companion object with initialization functions
 */
object StaticMeshGenome {
  def zero(nInputs: Int, nNeurons: Int, test: (Net => Double)) = {
    new StaticMeshGenome(nInputs, nNeurons, new Array[Double](nNeurons * (nInputs + nNeurons + 1)), test)
  }

  def rand(nInputs: Int, nNeurons: Int, test: (Net => Double)) = {
    val rand = new scala.util.Random
    new StaticMeshGenome(nInputs, nNeurons,
      (for (i <- (1 to (nNeurons * (nInputs + nNeurons + 1)))) yield rand.nextDouble - 0.5).toArray, test)
  }
}