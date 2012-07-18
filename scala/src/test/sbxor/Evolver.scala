package test.sbxor
import actors.Actor
import optimization._
import neural.dsl._
import neural.sb._
import neural.selfbuilding.GrowingLayer

object Evolver extends Actor {
  val plan = ((2 lin) >> (1 linm) >> (1 linmc)).asInstanceOf[MutableLayer]
  val initstruct = new GaStructure(plan, XorTest)
  var opt = new Optimizer(initstruct, 0.1)
  var best = 0d
  override def act() = {
    Input.start()
    var c = 0
    loop {
      if (best > -0.99) {
        c += 1
        println(c)
        best = opt.optimize(1).cost
        println(best)
      }
      else print(neural.utils.createScriptForPlan(opt.optimum.asInstanceOf[GaStructure].out))
      receiveWithin(1) {
        case str: String => tryout(str)
        case _ =>
      }
    }
  }

  def tryout(str: String) = {
    val state = str(0) match {
      case 'c' => opt.curState.asInstanceOf[GaStructure]
      case _ => opt.optimum.asInstanceOf[GaStructure]
    }
    val best = state.gen.get.best
    val n = best.decode()
    str.substring(1, 3) match {
      case "00" => n.inputs(0).set(0d); n.inputs(1).set(0d)
      case "01" => n.inputs(0).set(0d); n.inputs(1).set(1d)
      case "10" => n.inputs(0).set(1d); n.inputs(1).set(0d)
      case "11" => n.inputs(0).set(1d); n.inputs(1).set(1d)
    }
    n.calc()
    println(n.outputs(0).output())
    Input ! readLine()
  }
}