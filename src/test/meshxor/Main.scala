package test.meshxor
import nga._
import neural._
import meshed._
import actors.Futures._

object Main {
  def main(args: Array[String]) = {
    val rand = scala.util.Random
    val data = (for (i <- (0 to 3)) yield rand.nextDouble - 0.5).toArray
    val gen = new GenerationBase(for (i <- (1 to 20)) yield new MeshGenome(2, 1, data, xorTest))
    with Elitism with TriggeredHypermutation
    while (gen.best.fitness < 1) {
      gen.step
      println(gen.best.fitness)
    }
  }

  def xorTest(n: Net): Double = {
    var score = 0d
    val start = System.currentTimeMillis()
    n.inputs(0) set 0d
    n.inputs(1) set 0d
    awaitAll(100, future { n.calc() }) //300 ms timeout
    score -= math.pow(n.outputs(0).output + 1d, 2)
    n.inputs(0) set 0d
    n.inputs(1) set 1d
    awaitAll(100, future { n.calc() })
    score -= math.pow(n.outputs(0).output - 1d, 2)
    n.inputs(0) set 1d
    n.inputs(1) set 0d
    awaitAll(100, future { n.calc() })
    score -= math.pow(n.outputs(0).output - 1d, 2)
    n.inputs(0) set 1d
    n.inputs(1) set 1d
    awaitAll(100, future { n.calc() })
    score -= math.pow(n.outputs(0).output + 1d, 2)
    val end = System.currentTimeMillis()
    score *= math.pow(1.00001, end - start)
    if (!(score.isNaN || score.isInfinity)) math.pow(2, score) else 0
  }
}