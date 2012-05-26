package knapsa
import sa._
class KnapProblem(val size: Double, val itemSet: List[Item], override val initialTemp: Double) extends Problem with Serializable {
  override val initialState = new KnapState(size, itemSet, Nil)
  override def cool(temp: Double) = temp - 1
  override def probability(s1: State, s2: State, temp: Double) = {
    def f = { x: Double =>
      if (x > 0) math.pow(1 / 2, x / 2 + 1)
      else 1 - math.pow(2, x / 2 - 1)
    }
    f((s2.energy - s1.energy) / (math.pow(2, temp / 100)))
  }
  override def cycles = initialTemp.toInt
  override def toString()={
    "KnapProblem["+size+" "+itemSet+" "+initialTemp+"]"
  }
}

class SqKnapProblem(size: Double, itemSet: List[Item], initialTemp: Double) extends KnapProblem(size,itemSet,initialTemp){
  override def cool(temp:Double)={
    val root=(2*initialTemp-math.sqrt(4*initialTemp*temp+4*initialTemp*1E-5))/2
    val ret= (math.pow((root+1-initialTemp),2)/initialTemp)- 1E-5
    ret
  }
  override def toString()={
    "Sq"+super.toString
  }
}