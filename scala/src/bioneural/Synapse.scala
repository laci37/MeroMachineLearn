package bioneural
import math._
import util.realtime.Simulable
class Synapse(val pre: FNNeuron, val post: FNNeuron) extends Simulable {
  //add input to post neuron
  post.inputs(this.apply) = true
  //calculation variables, these define the synapse
  var pc = 1d //presynaptic transmitter release
  var mc = 0.1d //transmitter reuptake/decomposition
  var pk = 0.1d //transmitter bonding to receptors 
  var mk = 0.1d //transmitter release from receptors
  var g = 0.5d //conductivity

  //state variables, these define the actual state of the synapse
  var c = 0d
  var k = 0d

  def sigma(x: Double) = 1 / (1 + exp(-50 * x))
  def cd = (pc * sigma(pre.v - 1) - c * (mc + (1 - k) * pk) + k * mk) //derivative of c
  def kd = (c * (1 - k) * pk - k * mk) //derivative of k

  def timestep(dt: Double) = {
    c += dt * cd
    k += dt * kd
    nextI = g * k
  }
  var _I = 0d
  var nextI = 0d

  def I = _I

  def update() = {
    _I = nextI
  }

  /**
   * input function for postsynaptic neuron
   */
  def apply(v: Double) = I
}