package test.simrot
import util.plot._
import control.rotator._
import SimulatedRotator._
import java.awt._
class Init {
  val stroke = new BasicStroke(1)
  val plot = new TimePlotter((Color.blue, stroke), (Color.red, stroke), (Color.black, stroke))
  val wnd= new TimePlotFrame(plot)
  var red = 1d
  var blue = -1d
  var rot=new SimulatedRotator(1,1)
  rot.torques = rot.torques :+ spring(1,0) _
  rot.dynFriction=0.49
  rot.stcFriction=0.5
  
  def calc()={
   for(i<-(1 to 100)){
     rot.timestep(0.1)
     plot.step(0.1,red,blue,rot.pos)
   } 
  }
}