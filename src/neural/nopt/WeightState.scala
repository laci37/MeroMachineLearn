package neural.nopt
import neural.Net
import neural.dsl._
import nopt._
/**
 * Optimization state representing the weights of a neural network
 * data: weights
 * plan: the output layer of the network's structure plan
 * test: energy function defining the problem, better solutions should have lower energies
 */
class WeightState(data: Array[Double], val plan: Layer, val test: (Net => Double)) extends State {

  def energy(): Double = {
    test(new RecursiveNetworkBuilder(plan).build(data))
  }

  def neighbor(): State = {
    //change weights randomly 
    val rand = scala.util.Random
    val newdata = new Array[Double](data.size)
    data.copyToArray(newdata)
    for (i <- (0 to data.size - 1))
      newdata(i) += (rand.nextDouble - 0.5) * 0.01 + newdata(i) * (rand.nextDouble - 0.5) * 0.2
    new WeightState(newdata, plan, test)
  }

}

object WeightState {
  def zero(plan: Layer, test: (Net => Double)): WeightState = {
    new WeightState(new Array[Double](plan.countWeights),plan,test)
  }
  
}