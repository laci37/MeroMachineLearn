package neural.sa

import sa.State
import neural._
import dsl._

class WeightState(data: Array[Double], val plan: Layer, val test: (Net => Double)) extends State {

  def energy(): Double = test(new RecursiveNetworkBuilder(plan).build(data))

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