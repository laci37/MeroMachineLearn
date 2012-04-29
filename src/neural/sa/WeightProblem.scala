package neural.sa

import sa._
import neural._
import dsl._

class WeightProblem(plan: Layer, test: Net=>Double, val initialTemp: Double) extends Problem {

  def initialState(): State = WeightState.zero(plan,test)

  def probability(s1: State, s2: State, temp: Double): Double = { 0.0d }

  def cool(temp:Double)={
    val root=(2*initialTemp-math.sqrt(4*initialTemp*temp+4*initialTemp*1E-5))/2
    val ret= (math.pow((root+1-initialTemp),2)/initialTemp)- 1E-5
    ret
  }

  def cycles(): Int = { 0 }

}