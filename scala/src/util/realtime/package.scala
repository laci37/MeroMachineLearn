package util

package object realtime {
  implicit def Double2UnitFunc(d: => Double): (Unit => Double) = (Unit => d)
}