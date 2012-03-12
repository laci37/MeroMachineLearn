package neural.nga
import nga._
import neural._
import neural.dsl._
class WeightGenome(val weights: Array[Double], val plan: Layer, val test: (Net => Double))
  extends DoubleArrayGenome(weights) with Decodable[Net] {

  override def decode() = {
    new RecursiveNetworkBuilder(plan).build(weights)
  }

  override lazy val fitness = test(decode)

  override def create(data: Array[Double]) = new WeightGenome(data,plan,test).asInstanceOf[this.type] 
}

object WeightGenome {
  def zero(plan: Layer, test: (Net=>Double))={
    new WeightGenome(new Array[Double](plan.countWeights), plan, test)
  }
}