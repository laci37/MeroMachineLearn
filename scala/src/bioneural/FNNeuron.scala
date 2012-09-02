package bioneural
import math._
import collection.mutable.Set
import util.realtime.Simulable
class FNNeuron extends Simulable {

  protected var _v = -1.2d
  protected var lastv = -1.2d
  var w = 0.625d
  val inputs = Set.empty[(Double) => Double]

  def I: Double = {
    (for (func <- inputs) yield func(v)).sum
  }

  def v = lastv
  def wd=0.08 * (_v + 0.7 - 0.8 * w)
  def vd=(_v - pow(_v, 3) / 3 - w + I)

  def timestep(dt: Double) = {
    w += dt * wd
    _v += dt * vd
  }

  def update() = {
    lastv = _v
  }
}