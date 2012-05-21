package neural.sb

import optimization.State
import neural.dsl._
import annotation.tailrec
abstract class Structure(val out: MutableLayer) extends State {
  def layers = _layers
  private var _layers = Layer.discover(out)
  val rand = scala.util.Random
  def baseCost(): Double

  def cost(): Double = {
    import math._
    baseCost + abs(baseCost * 0.001 * _layers.size) + abs(baseCost * 0.0001 * nNeurons)
  }

  def nNeurons = {
    var n = 0
    _layers foreach { l => n += l.size }
    n
  }

  def neighbor(): State = {
    val r = rand.nextDouble()
    r match {
      case a if a < 0.20 => mutActFunc
      case a if a < 0.40 => addNeuron
      case a if a < 0.60 => insertLayer
      case a if a < 0.80 => removeNeuron
      case _ => addConnection
    }
  }

  //immutable state changes
  def mutActFunc = clone.asInstanceOf[Structure]._mutActFunc
  def addNeuron = clone.asInstanceOf[Structure]._addNeuron
  def insertLayer = clone.asInstanceOf[Structure]._insertLayer
  def addConnection = clone.asInstanceOf[Structure]._addConnection
  def removeNeuron = clone.asInstanceOf[Structure]._removeNeuron

  //mutable state changes
  protected def _mutActFunc: Structure = {
    randomMutableLayer.actcode = (rand.nextInt(256) - 128).toByte
    this
  }
  protected def _addNeuron: Structure = {
    randomMutableLayer.addNeuron()
    this
  }

  protected def _insertLayer: Structure = {
    val ol = randomMutableLayer
    val il = ol.inputs(rand.nextInt(ol.inputs.length))
    val layerOption = ol.insertLayer(il)
    if (layerOption.isDefined) _layers = layerOption.get :: _layers
    this
  }

  protected def _addConnection: Structure = {
    val ol = randomMutableLayer
    val il = randomLayer
    ol.addInput(il)
    this
  }

  protected def _removeNeuron() = {
    randomMutableLayer.removeNeuron()
    this
  }

  protected def randomLayer = {
    layers(rand.nextInt(layers.length))
  }

  @tailrec protected final def randomMutableLayer: MutableLayer = {
    val randlayer = randomLayer
    if (randlayer.isInstanceOf[MutableLayer] && !randlayer.const) return randlayer.asInstanceOf[MutableLayer]
    randomMutableLayer
  }

}