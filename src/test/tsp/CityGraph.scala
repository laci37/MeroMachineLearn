package tsp
import collection.mutable.Map
class CityGraph(val cities: List[City]) extends Serializable{
  def getDist(a: Int, b: Int): Double = {
    if (cities(a).dist.contains(b)) cities(a).dist(b)
    else Double.PositiveInfinity
  }

  def setDist(a: Int, b: Int, dist: Int) = {
    cities(a).dist+=b->dist
    cities(b).dist+=a->dist
  }
  
  override def toString={
   cities.toString
  }
}

case class City(var dist: Map[Int, Double], var index:Int)