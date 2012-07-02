package neural.meshed
import neural._
import nopt._
/**
 * experiment for optimization algorithm to find the best MeshedNet for a problem
 * nInputs: the number of inputs for the network
 * minNeurons: the minimal number of neurons for the problem
 * test: energy function defining the problem, better solution should have lower energy
 * limit: stop, if a solution with lower or equal energy than this limit has been reached 
 */
class MeshOptimizer(nInputs: Int, minNeurons: Int, test: (Net => Double), limit: Double) {

  /**
   * main loop
   */
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

  var stallLimit = 10000 //how long stall should trigger an increase in the number of neurons
  var changeLimit=0.001 // changes under this limit won't reset the stall counter

  /**
   * tries to get the best solution for given number of neurons
   */
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
    
    //init an optimizer
    val opt = new OptimizerBase(new MeshWeightState(
      initArray(nNeurons * (nNeurons + nInputs + 1)),
      nInputs, nNeurons, test)) // with StateLogging  //debug logging 
    //opt.out= new java.io.FileWriter("meshopt_"+nNeurons+".log") //debug logging 
    
    //optimize until stop condition is met
    opt.optimize(stop _)
  }

}