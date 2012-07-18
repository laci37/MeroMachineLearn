package test.xornga
import nga._
import neural._
import neural.dsl._
import neural.nga._
import test._
object Init extends TestBase {
  val plan = (2 lin) >> (2 tanh) >> (1 tanh)
  val gen = new GenerationBase(for (i <- 1 to 20) yield WeightGenome.zero(plan, test)) with Elitism //with TriggeredHypermutation
  var c = 0

  def calc() = { 
    gen.step
    c += 1
    print(".")
    //stop if score is good or if number of cycles is exceeded
    gen.best.fitness >= 0.9999 || c > 100000
  }

  def interact(s:String) = {
    println()
    try { //interactive testing
      val n = gen.best.asInstanceOf[WeightGenome].decode //create network
      //set inputs
      println("first input?")
      n.inputs(0) set Console.readDouble()
      println("second input?")
      n.inputs(1) set Console.readDouble()
      
      n.calc() //calculate first stable output
      print("output: ") // print it
      println(n.outputs(0).output)
    }
    false //program never stops on its own
  }

  //problem definition
  def test(n: Net): Double = {
    var sqerrsum = 0d //squared error sum
    n.inputs(0) set -1d
    n.inputs(1) set -1d
    n.calc() 
    sqerrsum += math.pow(n.outputs(0).output + 1d, 2)
    n.inputs(0) set -1d
    n.inputs(1) set 1d
    n.calc()
    sqerrsum += math.pow(n.outputs(0).output - 1d, 2)
    n.inputs(0) set 1d
    n.inputs(1) set -1d
    n.calc()
    sqerrsum += math.pow(n.outputs(0).output - 1d, 2)
    n.inputs(0) set 1d
    n.inputs(1) set 1d
    n.calc()
    sqerrsum += math.pow(n.outputs(0).output + 1d, 2)
    //if the network produces Nan-s it will have no chance to reproduce
    //we transform the scores using an exponential function
    if (!(sqerrsum.isNaN || sqerrsum.isInfinity)) math.pow(2, -sqerrsum) else 0d
  }
}