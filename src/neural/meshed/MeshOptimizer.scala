package neural.meshed
import neural._
import nopt._
class MeshOptimizer(nInputs: Int, minNeurons: Int, test: (Net => Double), limit: Double) {

  def optimize() = {
    var nNeurons = minNeurons
    var best: State = null
    do {
      best = getBest(nNeurons)
      nNeurons += 1
    } while (best.energy > limit)
    best
  }

  var stallLimit = 1000
  var changeLimit=0.001

  def getBest(nNeurons: Int) = {
    //closure of stop function 
    var stall = 0
    var lastEnergy = Double.PositiveInfinity
    def stop(s: State, c: Int): Boolean = {
      if (s.energy <= limit) true
      else {
        if (s.energy+changeLimit < lastEnergy) {
          stall = 0
          lastEnergy = s.energy
          false
        } else if (stall > stallLimit) true
        else{
          stall+=1
          false
        } 
      }
    }
    //closure end

    val opt = new OptimizerBase(new MeshWeightState(
      new Array[Double](nNeurons * (nNeurons + nInputs + 1)),
      nInputs, nNeurons, test))  with StateLogging
    opt.out= new java.io.FileWriter("meshopt_"+nNeurons+".log")
    opt.optimize(stop _)
  }

}