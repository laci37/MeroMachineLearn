package optxor
import neural._
import neural.dsl._
import optimization._
import neural.opt._
object Init {
  def main(args: Array[String]) = {
    val plan=(2 lin) >> (2 tanh) >> (1 tanh)
    val opt= new HeuristicRiseOptimizer(new WeightState(new Array[Double](9),plan,xorTest))
    var c=0
    println("OptXorStart")
    println(System.currentTimeMillis())
    while(opt.optimalCost>0){
      opt.optimize(1)
      println(opt.curCost+";"+opt.optimalCost)
      c+=1
    }
    println(c)
    println(System.currentTimeMillis())
  }
  
  def xorTest(n: Net):Double={
    var score = 0d
    n.inputs(0) set 0d
    n.inputs(1) set 0d
    n.calc()
    score += math.pow(n.outputs(0).output + 1d, 2)
    n.inputs(0) set 0d
    n.inputs(1) set 1d
    n.calc()
    score += math.pow(n.outputs(0).output - 1d, 2)
    n.inputs(0) set 1d
    n.inputs(1) set 0d
    n.calc()
    score += math.pow(n.outputs(0).output - 1d, 2)
    n.inputs(0) set 1d
    n.inputs(1) set 1d
    n.calc()
    score += math.pow(n.outputs(0).output + 1d, 2)
    if(!(score.isNaN||score.isInfinity)) score else Double.PositiveInfinity
  }
}