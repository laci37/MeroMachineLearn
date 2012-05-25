package control.rotator
import math._

class SimulatedRotator(val theta: Double, initpos: Double) extends Rotator {
  def this(theta: Double) = this(theta, 0d)

  private var _pos = initpos
  def pos(): Double = _pos

  private var _vel = 0d //angular velocity
  def vel() = _vel

  /**
   * collection of connected objects
   */
  var objects: Seq[SimObject] = Seq.empty

  /**
   * iterations per timestep
   */
  var iterations = 10

  /**
   * do a timestep of dt time
   */
  def timestep(dt: Double) = {
    var torque=0d
    var sumTheta=theta
    objects foreach {obj=>
       sumTheta+=obj.theta
       torque+=obj.extTorque(pos,vel)
    }
    if (vel == 0) torque -= signum(torque) * min(abs(torque), stcFriction / theta)
    _vel += torque * dt /sumTheta
    _vel -= signum(vel) * min(abs(vel), dt * dynFriction / theta) //dynamic friction kills velocity not torque
    _pos += vel * dt
    pos
  }

  /**
   * friction coefficents, since they can't be described with SimObjects,
   * but they need to be applied only once, since they are independent of acceleration
   */
  var dynFriction = 0d
  var stcFriction = 0d
}
