package bioneural
import math._
import collection.mutable.Set
import util.realtime.Simulable
class FNNeuron extends Simulable {

  protected var _v = -2d
  protected var lastv = -2d
  var w = 0.625d
  val inputs = Set.empty[(Double) => Double]

  def I: Double = {
    (for (func <- inputs) yield func(v)).sum
  }

  def v = lastv

  def timestep(dt: Double) = {
    w += dt * 0.08 * (_v + 0.7 - 0.8 * w)
    _v += dt * (_v - pow(_v, 3) / 3 - w + I)
  }

  def update() = {
    lastv = v
  }
}