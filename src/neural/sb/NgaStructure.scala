package neural.sb
import nga._
import neural._
import neural.nga._
class NgaStructure(out:MutableLayer,test:(Net=>Double)) extends Structure(out) {
  
  override lazy val baseCost={
    val initGenome=Iterable.fill(20)(WeightGenome.zero(out,test))
    val gen=new GenerationBase(initGenome) with Elitism with StallCount
  }
}