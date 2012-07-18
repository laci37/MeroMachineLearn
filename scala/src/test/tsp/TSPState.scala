package tsp
import optimization._
class TSPState(val permut: List[City], val graph: CityGraph) extends State with Serializable {
  override def cost() = {
    def inner(l:List[City]):Double={
      if(l.tail!=Nil){
        l.head.dist(l.tail.head.index)+inner(l.tail)
      }
      else 0d
    }
    inner(permut)
  }

  override def neighbor() = {
    val rand = scala.util.Random
    val first = rand.nextInt(permut.length)
    var right = rand.nextBoolean()
    if (first == 0) right = true
    else if (first == permut.length - 1) right = false
    var second = (if (right) first + 1 else first - 1)
    while (rand.nextBoolean && second > 0 && second < permut.length-1) {
      if (right) second += 1
      else second -= 1
    }
    val newpermut = if (right) {
      permut.slice(0, first) ::: (permut(second) ::
        permut.slice(first + 1, second)) ::: (permut(first) ::
        (if (second < permut.length - 1)
          permut.slice(second + 1, permut.length)
        else Nil))
    } else {
      permut.slice(0, second) ::: (permut(first) ::
        permut.slice(second + 1, first)) ::: (permut(second) ::
        (if (first < permut.length - 1)
          permut.slice(first + 1, permut.length)
        else Nil))
    }
    new TSPState(newpermut.asInstanceOf[List[City]], graph)
  }
  
  override def toString()="TspState["+this.cost.toString+"]"

}