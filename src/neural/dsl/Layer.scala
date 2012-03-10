package neural.dsl
import neural._
import scala.collection.mutable.Seq

class Layer(private val _size: Int, private val _actfunc: Double => Double) {
  var inputs: Seq[Layer] = Seq.empty
  var debug: String = ""
  def size() = _size
  def actfunc = _actfunc
  def d(s: String): Layer = {
    debug = s
    this
  }

  def >>(other: Layer) = {
    other.addInput(this)
    other
  }

  def addInput(il: Layer) = {
    inputs = inputs :+ il
  }

  override def clone() = {
    val clonedinputs = new Array[Layer](inputs.size)
    for (i <- Range(0, inputs.size)) clonedinputs(i) = inputs(i).clone.asInstanceOf[Layer]
    new Layer(size, actfunc) { inputs = clonedinputs }
  }

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

  def const: Boolean = true
  
}

object Layer {
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
