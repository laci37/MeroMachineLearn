package neural.sb

import optimization.State
import neural.dsl._
import annotation.tailrec
abstract class Structure(val out: MutableLayer) extends State {
  def layers = _layers
  private var _layers = Layer.discover(out)
  val input = layers.find(l => l.inputs.isEmpty).get
  protected def mutableLayers =
    for (l <- layers if (l.isInstanceOf[MutableLayer] && !l.const)) yield l.asInstanceOf[MutableLayer]
  protected def connections =
    (for (l <- layers) yield (for (il <- l.inputs) yield (l, il))).flatten
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
      case a if a < 1 / 7d => mutActFunc
      case a if a < 2 / 7d => addNeuron
      case a if a < 3 / 7d => insertLayer
      case a if a < 4 / 7d => removeNeuron
      case a if a < 5 / 7d => removeLayer
      case a if a < 6 / 7d => removeConnection
      case _ => addConnection
    }
  }

  //immutable state changes
  def mutActFunc = clone.asInstanceOf[Structure]._mutActFunc
  def addNeuron = clone.asInstanceOf[Structure]._addNeuron
  def insertLayer = clone.asInstanceOf[Structure]._insertLayer
  def addConnection = clone.asInstanceOf[Structure]._addConnection
  def removeNeuron = clone.asInstanceOf[Structure]._removeNeuron
  def removeLayer = clone.asInstanceOf[Structure]._removeLayer
  def removeConnection = clone.asInstanceOf[Structure]._removeConnection

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

  protected def _removeLayer() = {
    val toRemove = randomMutableLayer
    val outputs = for (l <- layers if (l.inputs.contains(toRemove))) yield l
    outputs foreach { ol =>
      ol.removeInput(toRemove)
      toRemove.inputs foreach { il =>
        il >> ol
      }
    }
    _layers = _layers diff Seq(toRemove)
    this
  }

  protected def _removeConnection() = {
    var conns = connections
    @tailrec def inner(): Unit = {
      val toRemove = conns(rand.nextInt(conns.length))
      toRemove._1.inputs = toRemove._1.inputs diff Seq(toRemove._2)
      if (checkPath) return
      conns = conns diff Seq(toRemove)
      toRemove._1.inputs = toRemove._1.inputs :+ toRemove._2
      inner
    }
    inner
    trim
    this
  }

  protected def checkPath(): Boolean = checkPath(out)

  protected def checkPath(l: Layer): Boolean = {
    if (l == input) return true
    l.inputs foreach { il =>
      if (il == input) return true
      if (checkPath(il)) return true
    }
    return false
  }

  protected def trim() = {
    def inner(l:Layer):Unit={
      l.inputs foreach{il=>
        if(il.inputs.isEmpty && il!=input){
          l.inputs= l.inputs diff Seq(il)
          _layers = _layers diff Seq(il)
        }
        else inner(il)
      }
    }
    inner(out)
  }

  protected def randomLayer = {
    layers(rand.nextInt(layers.length))
  }

  protected def randomMutableLayer: MutableLayer = {
    mutableLayers(rand.nextInt(mutableLayers.length))
  }

}