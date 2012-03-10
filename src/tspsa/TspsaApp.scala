package tspsa
import tsp.{ CityGraph, City }
import collection.mutable.Map
import sa.Optimizer
object TspsaApp extends App {
  val graph = new CityGraph((for (i <- Range(0, 50)) yield new City(Map.empty[Int, Double],i)).toList)
  for (i <- (0 to 48)) for (j <- (1 to 49)) if (j > i) {
    graph.setDist(i, j, math.round(20d * math.abs(math.sin(i + j))).toInt)
  }
  val problem = new TspProblem(graph,64000d)
  for (i <- (1 to 50)) {
    val opt = new Optimizer(problem)
    println(opt.optimize.energy)
  }
}

class SqTspProblem(graph:CityGraph,initialTemp:Double) extends TspProblem(graph,initialTemp)
{
  override def cool(temp:Double)={
    val root=(2*initialTemp-math.sqrt(4*initialTemp*temp+4*initialTemp*1E-5))/2
    val ret= (math.pow((root+1-initialTemp),2)/initialTemp)- 1E-5
    ret
  }
  override def toString()={
    "Sq"+super.toString
  }
}