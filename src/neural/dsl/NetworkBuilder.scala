package neural.dsl
import neural._
import collection.parallel.mutable.ParArray
abstract class NetworkBuilder(val output: Layer) extends Serializable{
  def build(cdata: Array[Double]): Net
}