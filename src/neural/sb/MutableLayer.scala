package neural.sb

import neural.dsl._
import neural.selfbuilding.GrowingLayer

class MutableLayer(actdecode: (Int => (Double => Double)), initsize: Int, override val const: Boolean) extends Layer(0, null) {
  private var _size2 = initsize
  override def size = _size2
  def size_=(value: Int) = if (!const) _size2 = value
  var actcode = 0
  override def actfunc = actdecode(actcode)
  private var _lock = false;
  def lock = (_lock)

  def this(actdecode: (Int => (Double => Double))) = this(actdecode, 1, false)
  def this(actdecode: (Int => (Double => Double)), const: Boolean) = this(actdecode, 1, const)
  def this(actdecode: (Int => (Double => Double)), initsize: Int) = this(actdecode, initsize, false)

  //locks the network plan, must be called on the output layer
  def lockPlan(): Unit = {
    if (!_lock) {
      _lock = true
      inputs foreach { il =>
        il match {
          case gl: GrowingLayer => gl.lockPlan()
          case ml: MutableLayer => ml.lockPlan()
          case _ =>
        }
      }
    }
  }
  //unlocks the network plan, must be called on the output layer
  def unlockPlan(): Unit = {
    if (_lock) {
      _lock = false
      inputs foreach { il =>
        il match {
          case gl: GrowingLayer => gl.unlockPlan()
          case ml: MutableLayer => ml.unlockPlan()
          case _ =>
        }
      }
    }
  }
  def addNeuron() = if (!const && !lock) _size2 += 1
  def removeNeuron() = if (size > 1 && !const && !lock) _size2 -= 1
  def insertLayer(il: Layer): Option[Layer] = {
    if (inputs.contains(il) && !_lock) {
      val newl = (il.size linm)
      inputs(inputs.indexOf(il)) = newl
      il >> newl
      Some(newl)
    } else None
  }

  def removeInput(il: Layer) = {
    if (!_lock) inputs = inputs diff Seq(il)
  }

  override def clone() = {
    val clonedinputs = new Array[Layer](inputs.size)
    val ret = new MutableLayer(actdecode, this.size, const) { actcode = this.actcode; }
    for (i <- Range(0, inputs.size)) clonedinputs(i) = inputs(i).clone(Map(this -> ret)).asInstanceOf[Layer]
    ret.inputs = clonedinputs
    ret
  }

  override def clone(done: Map[Layer, Layer]): Layer = {
    val clonedinputs = new Array[Layer](inputs.size)
    val ret = new MutableLayer(actdecode, this.size, const) { actcode = this.actcode; inputs = this.inputs }
    for (i <- Range(0, inputs.size)) {
      if (!done.contains(inputs(i))) {
        clonedinputs(i) = inputs(i).clone(done + (this -> ret)).asInstanceOf[Layer]
      } else clonedinputs(i) = done(inputs(i))
    }
    ret.inputs = clonedinputs
    ret
  }
}