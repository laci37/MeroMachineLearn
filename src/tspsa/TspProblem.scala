package tspsa
import sa._
import tsp.{ CityGraph, City }
class TspProblem(val graph: CityGraph, override val initialTemp:Double) extends Problem with Serializable {
  override val initialState = new TspState(graph.cities)
  override def probability(s1: State, s2: State, temp: Double) = {
    def f = { x: Double =>
      if (x > 0) math.pow(1 / 2, x / 20 + 1)
      else 1 - math.pow(2, x / 20 - 1)
    }
    f((s2.energy - s1.energy) / (math.pow(2, temp / 10)))
  }
  override def cool(temp:Double)=temp-1
  override def cycles=initialTemp.toInt
  override def toString()={
    "TspProblem["+initialTemp+" "+graph+"]"
  }
}