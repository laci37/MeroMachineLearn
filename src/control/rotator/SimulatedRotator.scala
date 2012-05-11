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
    var acc = 0d
    for (i <- (1 to iterations)) {
      torques foreach { f =>
        acc += f(pos, vel, acc) / theta
      }
    }
    if (vel == 0) acc -= min(acc, signum(acc) * stcFriction / theta)
    else acc -= acc -= min(acc, signum(acc) * dynFriction / theta)
    vel += acc * dt
    pos += vel * dt
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

  def load(theta: Double)(pos: Double, vel: Double, acc: Double) = -theta * acc

}