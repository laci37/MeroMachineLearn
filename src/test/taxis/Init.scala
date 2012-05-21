package taxis
import neural.dsl._
import neural.sb._
import neural.sb._
import optimization._
object Init {

  def main(args: Array[String]): Unit = {
    println("###########################Taxis start###################################")
    val plan= 2 linmc
    val l1= 1 linm
    val l2= 2 lin;
    l1 >> plan
    l2 >> plan
    l2 >> l1
    val initstruct = new GaStructure(plan, TaxisTest)
    var opt = new HeuristicRiseOptimizer(initstruct)
    var best = 0d
    var c = 0
    while (true) {
      c += 1
      println("gen" + c)
      val state = opt.optimize(1)
      best = state.cost
      println("struct:--------------------------------")
      println(neural.utils.createScriptForPlan(state.asInstanceOf[Structure].out))
      println("best" + best)
    }
  }

}