package neural.sb
import nga._
import neural._
import neural.nga._
class NgaStructure(out: MutableLayer, test: (Net => Double)) extends Structure(out) {
  var maxStall = 100
  var genSize = 20
  var _gen: Option[Generation] = None
  def gen() = _gen
  override lazy val baseCost = {
    val initGenome = Iterable.fill(genSize)(WeightGenome.zero(out, test))
    val gen = new GenerationBase(initGenome) with Elitism with StallCount
    gen.resetBest = false
    while (gen.stall < maxStall) {
      print(gen.stall+" ")
      gen.step()
    }
    println()
    _gen = Some(gen)
    gen.best.fitness
  }

  override def clone(): Structure = {
    new NgaStructure(out.clone, test) { genSize = this.genSize; maxStall = this.maxStall }
  }
}