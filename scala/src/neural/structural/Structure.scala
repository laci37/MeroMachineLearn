package neural.structural
import neural.dsl._

abstract class Structure {
  class Node {
    var inputs: Set[Node] = Set.empty
    var size:Int=1
    var actcode:Int=1
  }
  case class Edge(val begin: Node, val end: Node)
  var nodes:Set[Node]
  var edges:Set[Edge]
  
}