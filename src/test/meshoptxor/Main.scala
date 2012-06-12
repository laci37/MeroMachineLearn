package test.meshoptxor
import neural.Net
import neural.meshed._
import actors.Futures._
object Main {

  def main(args: Array[String]) = {
    val opt = new MeshOptimizer(2, 1, test, 0.999)
    opt.optimize()
  }

  def test(n: Net) = {
    var score = 0d
    val start = System.currentTimeMillis()
    n.inputs(0) set 0d
    n.inputs(1) set 0d
    awaitAll(10, future { n.calc() }) //300 ms timeout
    score -= math.pow(n.outputs(0).output + 1d, 2)
    n.inputs(0) set 0d
    n.inputs(1) set 1d
    awaitAll(10, future { n.calc() })
    score -= math.pow(n.outputs(0).output - 1d, 2)
    n.inputs(0) set 1d
    n.inputs(1) set 0d
    awaitAll(10, future { n.calc() })
    score -= math.pow(n.outputs(0).output - 1d, 2)
    n.inputs(0) set 1d
    n.inputs(1) set 1d
    awaitAll(10, future { n.calc() })
    score -= math.pow(n.outputs(0).output + 1d, 2)
    val end = System.currentTimeMillis()
    score *= math.pow(1.00001, end - start)
    if (!(score.isNaN || score.isInfinity)) math.pow(2, -score) else 0
  }
}