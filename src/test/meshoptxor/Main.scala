package test.meshoptxor
import neural.Net
import neural.meshed._
import actors.Futures._
object Main extends test.TestBase{

  def calc() = {
    val opt = new MeshOptimizer(2, 1, test, 0.999)
    opt.optimize()
    false
  }

  def test(n: Net) = {
    var score = 0d

    n.inputs(0) set 0d
    n.inputs(1) set 0d
    n.calc(10)
    score -= math.pow(n.outputs(0).output + 1d, 2)
    n.inputs(0) set 0d
    n.inputs(1) set 1d
    n.calc(10)
    score -= math.pow(n.outputs(0).output - 1d, 2)
    n.inputs(0) set 1d
    n.inputs(1) set 0d
    n.calc(10)
    score -= math.pow(n.outputs(0).output - 1d, 2)
    n.inputs(0) set 1d
    n.inputs(1) set 1d
    n.calc(10)
    score -= math.pow(n.outputs(0).output + 1d, 2)
    if (!(score.isNaN || score.isInfinity)) math.pow(2, -score) else 0
  }
  
  def interact(input:String)=false
}