package xornga
import nga._
import neural._
import neural.dsl._
import neural.nga._
object Init {

  def main(args: Array[String]): Unit = {
    val plan = (2 lin) >> (2 tanh) >> (1 tanh)
    val gen = new GenerationBase(for (i <- 1 to 5) yield WeightGenome.zero(plan, test)) with DetermineBest with Elitism
    while (true) {
      gen.step()
      println(gen.best.fitness)
    }
  }

  def test(n: Net): Double = {
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
    if (!(score.isNaN || score.isInfinity)) math.pow(2, -score) else 0d
  }
}