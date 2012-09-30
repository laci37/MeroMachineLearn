package bioneural
import util.realtime.Simulable
class Detector(n: FNNeuron) extends Simulable {
  var f = 0d
  var c = 1/30d
  def fd = -c
  protected var _out = false
  def firing = _out

  def timestep(dt: Double) = {
    f += dt * fd
    if (n.v > 1) f = 1
  }

  def update() = _out = f > 0
}