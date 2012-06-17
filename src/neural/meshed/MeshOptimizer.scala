package neural.meshed
import neural._
import nopt._
class MeshOptimizer(nInputs: Int, minNeurons: Int, test: (Net => Double), limit: Double) {

  def optimize() = {
    var nNeurons = minNeurons
    var best: State = null
    do {
      best = getBest(nNeurons)
      println(best)
      nNeurons += 1
    } while (best.energy > limit)
    best
  }

  var stallLimit = 10000
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
    
    //random initialization function
    def initArray(size:Int)={
      val rand=scala.util.Random
      (for(i<-(1 to size)) yield rand.nextDouble-0.5).toArray
    }
    
    val opt = new OptimizerBase(new MeshWeightState(
      initArray(nNeurons * (nNeurons + nInputs + 1)),
      nInputs, nNeurons, test))  with StateLogging
    opt.out= new java.io.FileWriter("meshopt_"+nNeurons+".log")
    opt.optimize(stop _)
  }

}