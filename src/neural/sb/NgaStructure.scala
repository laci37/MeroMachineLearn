package neural.sb
import nga._
import neural._
import neural.nga._
class NgaStructure(out: MutableLayer, test: (Net => Double)) extends Structure(out) {
  var maxStall = 50
  var genSize = 20
  override lazy val baseCost = {
    val initGenome = Iterable.fill(genSize)(WeightGenome.zero(out, test))
    val gen = new GenerationBase(initGenome) with Elitism with StallCount
    gen.resetBest = false
    while (gen.stall < maxStall) gen.step()
    gen.best.fitness
  }
}