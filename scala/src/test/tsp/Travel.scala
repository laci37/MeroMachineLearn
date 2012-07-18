package tsp
import collection.mutable.Map
import optimization._
import java.io._
object Travel {
  val graph = new CityGraph((for (i <- Range(0, 50)) yield new City(Map.empty[Int, Double],i)).toList)
  def main(args: Array[String]): Unit = {
    for (i <- (0 to 48)) for (j <- (1 to 49)) if (j > i) {
      graph.setDist(i, j, math.round(20d * math.abs(math.sin(i + j))).toInt)
    }
    val m= new MainActor(graph)
    m.start()
  }
}
