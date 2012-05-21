package neural.ga
import collection.parallel.mutable.ParArray
import neural.dsl.Layer
import annotation.tailrec
import test._
class Generation(val members: ParArray[Genome], val test: Tester) {
  var fitsum = 0d
  var best = members(0)
  var worst = members(0)
  def generateEx(size: Int): (Generation, Double) = {
    val rand = scala.util.Random
    members foreach (m => {
      m.fit = test.test(m);
      fitsum += m.fit;
      if (m.fit > best.fit) best = m;
      if (m.fit < worst.fit) worst = m;
    })
    //local function for finding parent with roulette wheel
    @tailrec def findparent(i: Int, p: Double): Genome = {
      var p2 = p
      p2 -= members(i).fit
      if (p2 <= 0) { return members(i) }
      findparent(i + 1, p2)
    }
    val newmembers = (for (i <- Range(0, size)) yield {
      if (i == 0) best //elitism
      else {
        val p1 = findparent(0, rand.nextDouble * fitsum)
        val p2 = findparent(0, rand.nextDouble * fitsum)
        p1 X p2
      }
    }).toArray
    (new Generation(newmembers.par, test), best.fit)
  }
  def generateEx(): (Generation, Double) = generateEx(members.length)
  def generate(): Generation = generateEx(members.length)._1
  def generate(size: Int): Generation = generateEx(size)._1

}

object Generation {
  /**
   * generate a random population from the given plans and the size
   */
  def random(l: Layer, size: Int, test: Tester): Generation = {
    var members = new ParArray[Genome](size)
    for (i <- Range(0, size)) members(i) = Genome.random(l)
    new Generation(members, test)
  }

  def zero(l: Layer, size: Int, test: Tester): Generation = {
    var members = new ParArray[Genome](size)
    for (i <- Range(0, size)) members(i) = Genome.zero(l)
    new Generation(members, test)
  }
  
  def randomAdaptive(l: Layer, size: Int, test: Tester): Generation = {
    var members = new ParArray[Genome](size)
    for (i <- Range(0, size)) members(i) = AdaptiveGenome.random(l)
    new Generation(members, test)
  }

  def zeroAdaptive(l: Layer, size: Int, test: Tester): Generation = {
    var members = new ParArray[Genome](size)
    for (i <- Range(0, size)) members(i) = AdaptiveGenome.zero(l)
    new Generation(members, test)
  }
}