package bioneural
import collection.mutable.Set
import math._
class HHNeuron(val membraneCap: Double) {

  /**
   * the set of input current functions
   */
  val currents = Set.empty[(Double, Double) => Double]

  protected var _V = 0d //membrane potential in mV
  protected var _time = 0d //time

  /**
   * membrane potential in mV
   */
  def V = _V
  /**
   * time spent in simulation
   */
  def time = _time

  def timestep(dt: Double) = {
    _time += dt
    n+=dt*(alphan*(1-n)-betan*n)
    m+=dt*(alpham*(1-m)-betam*m)
    h+=dt*(alphah*(1-h)-betah*h)
    val sumI = (for (Ii <- currents) yield Ii(V, dt)).sum + Ik + Ina
    _V += (-1d / membraneCap) * sumI * dt
  }

  /* Potassium ion channel */
  protected var n = alphan / (alphan + betan)

  def alphan() = {
    0.01 * (V + 10) / exp((V + 10) / 10 - 1)
  }

  def betan() = {
    0.125 * exp(V / 80)
  }

  def Ik() = {
    36 * pow(n, 4) * (V - 12)
  }

  /*Sodium ion channel*/
  var m = alpham / (alpham + betam)
  var h = alphah / (alphah + betah)

  def alpham() = {
    0.1 * (V + 25) / exp((V + 25) / 10 - 1)
  }

  def betam() = {
    4 * exp(V / 18)
  }

  def alphah() = {
    0.07 * exp(V / 20)
  }

  def betah() = {
    1 / exp((V + 30) / 10 + 1)
  }

  def Ina = 120 * pow(m, 3) * h * (V + 115)
  
  /* leak current*/
  def Il = 0.3*(V+10.613)
}