package control.rotator
import math._

class SimulatedRotator(val theta: Double, initpos: Double) extends Rotator {
  import SimulatedRotator._
  def this(theta: Double) = this(theta, 0d)

  private var _pos = initpos
  def pos(): Double = _pos

  private var _vel = 0d //angular velocity
  def vel() = _vel

  /**
   * collection of interacting torques
   */
  var torques: Iterable[TorqueFunc] = Seq.empty[TorqueFunc]

  /**
   * iterations per timestep
   */
  var iterations = 10

  /**
   * do a timestep of dt time
   */
  def timestep(dt: Double) = {
    def inner(pos: Double, vel: Double, acc: Double, c: Int): Double = {
      var newacc = calc(pos, vel, acc)
      if (c <= 0) return newacc
      inner(pos, vel, newacc, c - 1)
    }
    var acc = inner(pos, vel, 0, iterations)
    if (vel == 0) acc -= signum(acc) * min(abs(acc), stcFriction / theta)
    else acc -= signum(acc) * min(abs(acc), dynFriction / theta)
    _vel += acc * dt
    _pos += vel * dt
    pos
  }

  /**
   * a single step of calculation
   */
  def calc(pos: Double, vel: Double, acc: Double) = {
    var newacc = 0d
    torques foreach { f =>
      newacc += f(pos, vel, acc) / theta
    }
    newacc
  }

  /**
   * friction coefficents, since they can't be described with torque functions,
   * but they need to be applied only once,since they are independent of acceleration
   */
  var dynFriction = 0d
  var stcFriction = 0d
}

object SimulatedRotator {
  /**
   * typedef for functions describing interacting torques
   * parameter order: pos,vel,acc
   */
  type TorqueFunc = (Double, Double, Double) => Double

  //some useful torque functions

  def load(theta: Double)(pos: Double, vel: Double, acc: Double) =
    -theta * acc

  //gravity acts here in the direction 3/2*pi and g=1
  def eccentricLoad(mass: Double, dist: Double)(pos: Double, vel: Double, acc: Double) =
    -mass * cos(pos) * dist - mass * dist * acc
      
  def spring(springConst: Double, zeroPoint: Double)(pos: Double, vel: Double, acc: Double) =
    springConst * (zeroPoint - pos)

  def resistance(coeff: Double)(pos: Double, vel: Double, acc: Double) =
    -vel * vel * coeff
  
}