package neural.dsl
import neural._
import scala.collection.mutable.Seq
/**
 * class for data describing a single layer of neurons
 *  _size: number of neurons in the layer
 *  _actfunc: the activation function for the neurons to use 
 */
class Layer(private val _size: Int, private val _actfunc: Double => Double) extends Serializable {
  /**
   * Seq to store the input layers
   */
  var inputs: Seq[Layer] = Seq.empty
  
  var debug: String = ""
    
  def size() = _size
  
  def actfunc = _actfunc
  
  /**
   * add debug string operator
   */
  def d(s: String): Layer = {
    debug = s
    this
  }
  
  /**
   * connect operator
   */
  def >>(other: Layer) = {
    other.addInput(this)
    other
  }
  
  /**
   * Function behind the >> operator
   */
  def addInput(il: Layer) = {
    inputs = inputs :+ il
  }
  
  //removes an input layer, used in some experiments
  def removeInput(il: Layer) = {
    inputs = inputs diff Seq(il)
  }
  
  /**
   * Makes a deep copy of this layer, recurses to other layers of this network definition
   */
  override def clone() = {
    val clonedinputs = new Array[Layer](inputs.size)
    for (i <- Range(0, inputs.size)) clonedinputs(i) = inputs(i).clone.asInstanceOf[Layer]
    new Layer(size, actfunc) { inputs = clonedinputs }
  }
  
  /**
   * clone with a map to store already clone Layers
   */
  def clone(done: Map[Layer, Layer]): Layer = {
    val clonedinputs = new Array[Layer](inputs.size)
    val ret = new Layer(size, actfunc)
    for (i <- Range(0, inputs.size)) {
      if (!done.contains(inputs(i))) {
        clonedinputs(i) = inputs(i).clone(done + (this -> ret)).asInstanceOf[Layer]
      } else clonedinputs(i) = done(inputs(i))
    }
    ret.inputs = clonedinputs
    ret
  }

  /**
   * counts the number of weights in the network including bias
   */
  def countWeights(): Int = {
    if (inputs.size != 0) {
      var sum = this.size //bias weights for all neurons
      inputs foreach { il =>
        sum += this.size * il.size
        sum += il.countWeights(Set(this))
      }
      sum
    } else 0 //input layers do not have any weights
  }

  /**
   * countWeights with extra parameter to store already visited layers
   * so recurring networks don't cause a stack overflow
   */
  def countWeights(done: Set[Layer]): Int = {
    if (inputs.size != 0 && !done.contains(this)) {
      var sum = this.size //bias weights for all neurons
      inputs foreach { il =>
        sum += this.size * il.size
        sum += il.countWeights(done + this)
      }
      sum
    } else 0 //input layers do not have any weights, or this layer was already visited
  }
  
  //if this is true the layer is immutable, used in subclasses
  def const: Boolean = true

}

//companion object of Layer
object Layer {
  /**
   * Discovers all layers in a network structure
   * out: the output layer
   */
  def discover(out: Layer): List[Layer] = {
    var ret: List[Layer] = Nil
    def inner(cl: Layer): Unit = {
      if (!ret.contains(cl)) {
        ret = cl :: ret
        cl.inputs.foreach { il => inner(il) }
      }
    }
    inner(out)
    ret
  }
  

}
