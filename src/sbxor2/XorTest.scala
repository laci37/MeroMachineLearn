package sbxor2

import neural.ga._
import neural._
import actors.Futures._

object XorTest extends Tester {

  def test(n: Net): Double = {
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
  
  def test(g: Genome): Double = {
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