package test.sumnet
import neural._
object SumTest {
  def test(n: Net) = {
    import math._
    var score = 0d
    for (i <- (1 to 15)) {
      n.inputs(0) set i
      n.timestep
      if (i > 5) {
        score += abs(n.outputs(0) - (i - 5) * (i - 4) / 2)
      }
    }

    for (i <- (-10 to 10)) {
      n.inputs(0) set i
      n.timestep
      if (i > -6) {
        score += abs(n.outputs(0) - (((i + 6) * (i + 7) / 2) - (i + 6) * 11))
      }
    }

    for (i <- (-1000 to 1000)) {
      n.inputs(0) set i
      n.timestep
      if (i > -996) {
        score += abs(n.outputs(0) - (((i + 996) * (i + 997) / 2) - (i + 996) * 1001))
      }
    }
  }
}