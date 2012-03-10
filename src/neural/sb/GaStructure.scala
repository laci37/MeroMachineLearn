package neural.sb

import neural.ga._
import neural.dsl._
import neural.selfbuilding._
import annotation.tailrec

class GaStructure(out: MutableLayer, test: Tester) extends Structure(out) {

  var gensize = 20
  var maxStallLength = 100
  private var _gen: Option[Generation] = None
  def gen = _gen
  override lazy val baseCost: Double = {
    out.lockPlan()
    var best: Double = 0d
    var stall: Int = 0
    var gen = Generation.random(out, gensize, test)
    _gen = Some(gen)
    @tailrec def cycle(): Unit = {
      print(stall)
      if (stall > maxStallLength) return
      val tpl = gen.generateEx()
      gen = tpl._1
      _gen = Some(gen)
      if (tpl._2 <= best) stall += 1
      else {
        best = tpl._2
        stall = 0
      }
      cycle
    }
    cycle()
    out.unlockPlan()
    println()
    -best
  }

  override def clone(): Structure = {
    new GaStructure(out.clone, test) { gensize = this.gensize; maxStallLength = this.maxStallLength }
  }
}