package neural.selfbuilding
import scala.collection.parallel.mutable.ParArray
import neural.dsl._
import annotation.tailrec
class AltGeneration(var members: ParArray[GrowingGenome], val test: AltTester, var mut: Mutagen) {
  var gen = 0
  val rand = util.Random
  var best = 0d
  var flat = 0
  def generate() = {
    var fitsum = 0d
    var id = 0
    members foreach { g =>
      import neural.utils._
      g.fit = test.test(g)
      fitsum += g.fit
      //NetworkDrawer.drawToFile(g.out,gen.toString+"_"+id+".svg")
      id += 1
    }
    println(fitsum)

    val sorted = members.toArray
    util.Sorting.quickSort(sorted)
    println("Best: " + sorted(sorted.size - 1).fit)
    mut.updateFit(fitsum)
    val newmembers = new ParArray[GrowingGenome](members.size)
    @tailrec def findparent(i: Int, p: Double): GrowingGenome = {
      var p2 = p
      p2 -= sorted(i).fit
      if (p2 <= 0)
        return sorted(i)
      findparent(i + 1, p2)
    }
    if (best == sorted(sorted.size - 1).fit) flat += 1
    best = sorted(sorted.size - 1).fit
    if (flat < 100) {
      newmembers(0) = sorted(sorted.size - 1)
      for (i <- Range(1, newmembers.size)) {
        newmembers(i) = findparent(0, rand.nextDouble * fitsum).clone
        mut(newmembers(i))
      }
    } else {
      for (i <- Range(0, newmembers.size)) {
        newmembers(i) = findparent(0, rand.nextDouble * fitsum).clone
        mut(newmembers(i))
      }
    }
    members = newmembers
    gen += 1
  }
}

object AltGeneration {
  def apply(size: Int, nInputs: Int, nOutputs: Int, test: AltTester) = {
    import neural.dsl._
    val members = new ParArray[GrowingGenome](size)
    val mut = new DefaultMutagen
    for (i <- Range(0, size)) {
      val plans: GrowingLayer = ((nInputs lin) >> (2 tanhg) >> (nOutputs lingc)).asInstanceOf[GrowingLayer]
      val g = new GrowingGenome(plans)
      val mut = new DefaultMutagen
      mut(g)
      members(i) = g
    }
    new AltGeneration(members, test, mut)
  }
}