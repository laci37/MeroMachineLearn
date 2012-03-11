package neural.opt

import optimization.State
import neural.dsl._
import neural._

/**
 * Optimization state representing the weights of a neural network
 */
class WeightState(data: Array[Double], val plan: Layer, val test: (Net => Double)) extends State {

  def cost(): Double = {
    test(new RecursiveNetworkBuilder(plan).build(data))
  }

  def neighbor(): State = {
    //change one weight between +-10%
    val rand = util.Random
    val newdata = new Array[Double](data.size)
    data.copyToArray(newdata)
    val changeIndex = rand.nextInt(data.size)
    newdata(changeIndex) *= rand.nextDouble() / 5 + 0.9 //multiply by a value between 0.9 and 1.1
    new WeightState(newdata, plan, test)
  }

}