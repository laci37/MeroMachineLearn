package taxis

case class Vector(val x: Double, val y: Double) {
  import math._
  def +(that: Vector) = {
    new Vector(this.x + that.x, this.y + that.y)
  }

  def -(that: Vector) = {
    new Vector(this.x - that.x, this.y - that.y)
  }

  lazy val length = sqrt(pow(x, 2) + pow(y, 2))

  lazy val angle = {
    val alpha1 = acos(x / length)
    if (y >= 0) alpha1
    else 2 * Pi - alpha1
  }
}

object Vector {
  def fromAngleLength(alpha: Double, l: Double) = {
    new Vector(l * math.cos(alpha), l * math.sin(alpha))
  }
}