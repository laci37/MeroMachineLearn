package xor
import neural._
import neural.selfbuilding._
import neural.dsl._
import scala.actors.Futures._
object MyTest extends AltTester {
  override def test(g: GrowingGenome): Double = {
    val n = g.decode
    var score = 0d
    n.inputs(0) set 0d
    n.inputs(1) set 0d
    awaitAll(300, future { n.calc() }) //300 ms timeout
    score -= math.pow(n.outputs(0).output + 1d, 2)
    n.inputs(0) set 0d
    n.inputs(1) set 1d
    awaitAll(300, future { n.calc() })
    score -= math.pow(n.outputs(0).output - 1d, 2)
    n.inputs(0) set 1d
    n.inputs(1) set 0d
    awaitAll(300, future { n.calc() })
    score -= math.pow(n.outputs(0).output - 1d, 2)
    n.inputs(0) set 1d
    n.inputs(1) set 1d
    awaitAll(300, future { n.calc() })
    score -= math.pow(n.outputs(0).output + 1d, 2)
    if(!(score.isNaN||score.isInfinity))math.pow(2, score) else 0
  }
}