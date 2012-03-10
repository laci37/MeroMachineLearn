package xor
import neural._
import neural.dsl._
import neural.selfbuilding._
import actors._
object Evolver extends Actor {
  var gen = AltGeneration(20,2,1,MyTest)
  override def act(): Unit = {
    var c = 0
    loop {
      printf("Generation %d \n", c)
      val nextgen = gen.generate()
      c += 1
      //tryout("gen" + c.toString + ".jpg")
      receiveWithin(1) {
        case str: String => tryout(str)
        case _ => Unit
      }
    }
  }

  def tryout(str: String):Unit = {
    var best = gen.members(0)
    for (g <- gen.members) {
      if (g.fit > best.fit) best = g
    }
    val n = best.decode
    str match {
      case "00" => {
        n.inputs(0) set 0
        n.inputs(1) set 0
      }
      case "01" => {
        n.inputs(0) set 0
        n.inputs(1) set 1
      }
      case "10" => {
        n.inputs(0) set 1
        n.inputs(1) set 0
      }
      case "11" => {
        n.inputs(0) set 1
        n.inputs(1) set 1
      }
      case _ => {
        println("bad input")
        Console.readLine()
        return
      }
    }
    n.calc()
    println(n.outputs(0).output)
    Console.readLine()
    Input ! Unit
  }
}