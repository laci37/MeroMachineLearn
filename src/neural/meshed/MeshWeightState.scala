package neural.meshed
import nopt.DoubleArrayState
import neural.Net
class MeshWeightState(weights: Array[Double], nInputs: Int, nNeurons: Int, test: (Net => Double)) extends DoubleArrayState(weights) {

  def energy() = test(MeshedNet(nInputs, nNeurons, weights))

  def neighbor() = new MeshWeightState(calcNeighborData, nInputs, nNeurons, test)

  override def mutateDouble(d: Double) = {
    import math._
    if (rand.nextDouble < 0.001) {
      val change = (rand.nextDouble - 0.5) * 0.1
      if (signum(d) == signum(change) && abs(d) < abs(change)) 0d
      else d - change
    } else if (rand.nextDouble < 0.1) 0d
    else d * ((rand.nextDouble - 0.5) * 0.002 + 1)
  }
}