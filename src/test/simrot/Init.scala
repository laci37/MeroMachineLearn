package test.simrot
import util.plot._
import control.rotator._
import SimObject._
import java.awt._
class Init extends {
  val stroke = new BasicStroke(1)
  val plot = new TimePlotter((Color.blue, stroke), (Color.red, stroke))
  val wnd = new TimePlotFrame(plot)
  var rot = new SimulatedRotator(1, 1)
  rot.objects = Seq(new ExcentricLoad(1, 1))
  rot.dynFriction = 0.49
  rot.stcFriction = 0.5

  def calc() = {
    for (i <- (1 to 1000)) {
      rot.timestep(0.01)
      plot.step(0.01, rot.vel, rot.pos)
    }
  }
}