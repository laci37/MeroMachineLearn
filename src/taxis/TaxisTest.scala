package taxis

import neural.ga._

object TaxisTest extends Tester {

  def test(g: Genome): Double = {
    val rand = new util.Random
    val n = g.decode()
    var sum = 0d
    for (j <- (1 to 1)) {
      var v = new Vector(0, 0)
      var p = Vector.fromAngleLength(j, 100d)

      for (i <- (1 to 100)) {
        n.inputs(0) set p.angle
        n.inputs(1) set p.length
        n.timestep()
        v += Vector.fromAngleLength(n.outputs(0).output, n.outputs(1).output)
        p += v
        sum += p.length
      }
    }
    if (!(sum.isNaN || sum.isInfinite)) 1 / sum
    else 0d
  }

}


