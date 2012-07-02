package neural.dsl
import neural._
/**
 * abstract base class for all network builder classes
 */
import collection.parallel.mutable.ParArray
abstract class NetworkBuilder(val output: Layer) extends Serializable{
  /**
   * builds a network with the weights given in cdata
   */
  def build(cdata: Array[Double]): Net
}