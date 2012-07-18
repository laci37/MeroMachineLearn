package control.rotator

trait SimObject {
 def theta():Double
 def extTorque(pos:Double, vel:Double):Double //external torques (gravity, springs, motors) acting on the object
}

object SimObject{
  import math._
//some basic objects
class Load(var theta:Double) extends SimObject{
  def extTorque(pos:Double,vel:Double)=0d
}

class ExcentricLoad(var mass:Double, var dist:Double, var dir:Double, var g:Double) extends SimObject{
  def this(mass:Double, dist:Double)=this(mass,dist,0,10)
  def this(mass:Double, dist:Double,dir:Double)=this(mass,dist,dir,10)
  override def theta()=mass*dist
  override def extTorque(pos:Double,vel:Double)= -cos(pos+dir)*mass*g
}

class Spring(var spConst:Double, var nullPoint:Double) extends SimObject{
  override val theta=0d
  override def extTorque(pos:Double,vel:Double)= spConst*(nullPoint-pos)
}

}