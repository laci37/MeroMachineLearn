package test.sumnet
import neural._
object SumTest {
  def test(n: Net) = {
    var score = 0d
    for (i <- (1 to 10)) {
      n.inputs(0) set i
      n.timestep
      score += n.outputs(0) - i * (i + 1) / 2
    }
  }
}