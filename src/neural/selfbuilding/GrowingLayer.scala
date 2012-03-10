package neural.selfbuilding

import neural.dsl._
import collection.mutable.ArrayBuffer
import collection.mutable.Seq

class GrowingLayer(initialsize: Int) extends Layer(0, null) {
  override def size = _size2
  private var _size2 = initialsize

  override def actfunc() = {
    val unsigned: Int = math.abs(actcode)
    unsigned match {
      case a if a % 4 == 0 => x => math.tanh(x)
      case a if a % 4 == 1 => x => x
      case a if a % 4 == 2 => x => x
      case a if a % 4 == 3 => x => math.signum(x)
    }
  }

  var actcode: Byte = 0

  var data = new ArrayBuffer[Double]

  def wpn() = data.length / size

  var outputs = new ArrayBuffer[GrowingLayer]
  private var _const = false
  var lock = false
  override def const = _const || lock
  def const_=(value: Boolean) = _const = value

  override def >>(other: Layer): GrowingLayer = {
    other match {
      case gl: GrowingLayer => {
        outputs = outputs :+ gl
        gl.addInput(this)
        gl
      }
      case _ => throw new Exception("Growing layer connected to general Layer")
    }
  }

  override def addInput(il: Layer) = {
    if (!this.inputs.contains(il)) {
      inputs = inputs :+ il
      val wpn = this.wpn
      if (data.length != 0) {
        for (i <- Range(1, size + 1)) {
          for (j <- Range(0, il.size)) {
            data.insert(i * wpn, 0d)
          }
        }
      } else {
        for (i <- Range(0, size * il.size)) data += 0
      }
      if (il.isInstanceOf[GrowingLayer]) {
        val gl = il.asInstanceOf[GrowingLayer]
        if (!gl.outputs.contains(this)) gl.outputs += this
      }
      debug += " addInput"
    }
  }

  def updateDataSize(il: Layer, oldsize: Int) = {
    val index = inputs.indexOf(il) //index of updating layer
    var relloc = oldsize //relative location to insert
    val wpn = this.wpn //cache wpn, so inserting wont change it
    for (i <- Range(0, index)) relloc += inputs(i).size
    for (i <- Range(0, size)) {
      for (j <- Range(0, il.size - oldsize)) {
        data.insert(i * wpn + relloc, 0d)
      }
    }
    debug += " uDS"
  }

  def addNeuron() = {
    val wpn = this.wpn
    if (!const) {
      for (i <- Range(0, wpn)) data += 0d
      _size2 += 1
      outputs foreach { o => o.updateDataSize(this, size - 1) }
    }
    debug += " aN"
  }

  def insertLayer(il: Layer): Option[GrowingLayer] = {
    if (inputs.contains(il)) {
      val newlayer = (il.size ling)
      il >> newlayer
      inputs(inputs.indexOf(il)) = newlayer
      newlayer.outputs += this
      if (il.isInstanceOf[GrowingLayer]) {
        var oseq = il.asInstanceOf[GrowingLayer].outputs
        oseq = oseq.diff(Seq(this))
        il.asInstanceOf[GrowingLayer].outputs = oseq
      }
      debug += " iL"
      Some(newlayer)
    } else None
  }

  override def clone() = {
    val clonedinputs = new Array[Layer](inputs.size)
    val cloneddata = this.data.clone()
    val ret = new GrowingLayer(this.size) {
      actcode = this.actcode
      data = cloneddata
      debug = this.debug + "c"
      const = this.const
    }
    for (i <- Range(0, inputs.size)) {
      clonedinputs(i) = inputs(i).clone(Map(this -> ret)).asInstanceOf[Layer]
    }
    ret.inputs = clonedinputs
    ret.correctOutputs(Nil)
    ret
  }

  override def clone(done: Map[Layer, Layer]): Layer = {
    val clonedinputs = new Array[Layer](inputs.size)
    val cloneddata = this.data.clone()
    val ret = new GrowingLayer(this.size) {
      actcode = this.actcode
      data = cloneddata
    }
    for (i <- Range(0, inputs.size)) {
      if (!done.contains(inputs(i))) {
        clonedinputs(i) = inputs(i).clone(done + (this -> ret)).asInstanceOf[Layer]
      } else clonedinputs(i) = done(inputs(i))
    }
    ret.inputs = clonedinputs
    ret
  }

  def correctOutputs(done: List[GrowingLayer]): Unit = {
    inputs foreach { il =>
      il match {
        case gl: GrowingLayer => {
          if (!gl.outputs.contains(this)) gl.outputs = gl.outputs :+ this
          if (!done.contains(gl)) gl.correctOutputs(this :: done)
        }
        case _ =>
      }
    }
  }

  //locks the network plan, must be called on the output layer
  def lockPlan(): Unit = {
    lock = true
    inputs foreach { il =>
      il match {
        case gl: GrowingLayer => gl.lockPlan()
        case _ =>
      }
    }
  }

  //unlocks the network plan, must be called on the output layer
  def unlockPlan(): Unit = {
    lock = false
    inputs foreach { il =>
      il match {
        case gl: GrowingLayer => gl.unlockPlan()
        case _ =>
      }
    }
  }

}

object GrowingLayer {
  def apply() = new GrowingLayer(1)
  def apply(size: Int) = new GrowingLayer(size)
  def apply(size: Int, const: Boolean) = new GrowingLayer(size) { const = const }
  def apply(size: Int, actcode: Byte) = new GrowingLayer(size) { actcode = actcode }
  def apply(size: Int, actcode: Byte, const: Boolean) = {
    new GrowingLayer(size) { const = const; actcode = actcode }
  }
}