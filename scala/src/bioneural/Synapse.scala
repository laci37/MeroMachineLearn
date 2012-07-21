package bioneural
import math._
import util.realtime.Simulable
class Synapse(val pre: FNNeuron) extends Simulable{
  //calculation variables, these define the synapse
  var s = 1d //presynaptic transmitter release
  var b = 0.1d //transmitter reuptake/decomposition
  var a = 0.1d //transmitter bonding to receptors 
  var d = 0.1d //transmitter release from receptors
  var g = 0.5d //conductivity

  //state variables, these define the actual state of the synapse
  var c = 0d
  var k = 0d

  def sigma(x: Double) = 1 / (1 + exp(-50 * x))

  def timestep(dt: Double) = {
    c += s * sigma(pre.v - 1) - c * (b + (1 - k) * a) + k * d
    k += c * (1 - k) * a - k * d
    nextI=g*k
  }
  var _I=0d
  var nextI=0d
  
  def I = _I
  
  def update()={
    _I=nextI
  }

  /**
   * input function for postsynaptic neuron
   */
  def apply(v: Double) = I
}