package neural.meshed
import nopt.DoubleArrayState
import neural.Net
/**
 *  State class for the weights of a meshnet with fixed size
 */
class MeshWeightState(weights: Array[Double], nInputs: Int, nNeurons: Int, test: (Net => Double)) extends DoubleArrayState(weights) {

  def energy() = test(MeshedNet(nInputs, nNeurons, weights))

  def neighbor() = new MeshWeightState(calcNeighborData, nInputs, nNeurons, test)

  override def mutateDouble(d: Double) = {
    import math._
    if (rand.nextDouble < 0.2) d * (rand.nextDouble() + 0.5) 
    else if (rand.nextDouble < 0.01) 0d
    else if (rand.nextDouble < (1d/weights.size)) {
      val change = (rand.nextDouble - 0.5) * 0.1
      if (signum(d) == signum(change) && abs(d) < abs(change)) 0d
      else d - change
    } else d
  }

  override def toString() =
    "MeshWeightState n=" + nNeurons + " i=" + nInputs + " e=" + energy + " {" + weights.mkString(" ") + "}"
}