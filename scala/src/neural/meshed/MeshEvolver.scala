package neural.meshed
import annotation.tailrec
import nga._
import neural.Net

/**
 * Class for trying to find the best MeshedNet for solving a problem
 * nInputs: the number of inputs to use
 * minNeurons: The minimal number of neurons used to try to solve the problem
 * test: the function defining the problem, the better a network achieves, the greater this score should be
 * stop: the stop condition, if it returns true to a solution the evolution will stop and return
 */
class MeshEvolver(nInputs: Int, minNeurons: Int, test: (Net => Double), stop: (StaticMeshGenome => Boolean)) {
  
  /**
   * Main function of the evolution process 
   */
  def evolve(): StaticMeshGenome = {
    
    @tailrec def inner(): StaticMeshGenome = {
      val ret=loopBody()
      println(ret._2.fitness)
      if(ret._1) return ret._2
      inner
    }
    
    inner
  }
  
  /**
   * the number of neurons for the network
   */
  
  var nNeurons=minNeurons
  /**
   * if evolution stalls for stallLim generations, 
   *  we use the given solution as the best possible for that many neurons
   */
  var stallLim=1000 
  
  def loopBody():(Boolean,StaticMeshGenome)={
    //Creating new GenerationBase for evolving, using Elitism and TriggeredHypermutation
    val gen=new GenerationBase(for(i<-(1 to 20)) yield StaticMeshGenome.zero(nInputs,nNeurons,test))
     with Elitism with TriggeredHypermutation 
    
     //inner loop function, for trying to find the best solution to the given number of neurons
     //must be defined here, to use tailrec and avoid final
    @tailrec def innerLoop():(Boolean,StaticMeshGenome)={
      gen.step //calculate the next generation
      //if stop condition is met return
      if(stop(gen.best.asInstanceOf[StaticMeshGenome])) return (true,gen.best.asInstanceOf[StaticMeshGenome])
      //else if the evolution stalled return
      else if(gen.stall>stallLim) return (false,gen.best.asInstanceOf[StaticMeshGenome])
      innerLoop
    }
    
    //increment nNeurons, it does not matter in this loop anymore
    nNeurons+=1
    //call innerLoop to find the best solution
    innerLoop
  }
}