package test.hoptest
import neural.mem._
import test.TestBase

object Init extends TestBase {
  val net = new HopNet(8, 0d) //with neural.utils.CalcLogger
  val rand = scala.util.Random
  var c = 0
  net.neurons foreach { n => ; println(n + " " + n.inputs) }

  def calc(): Boolean = {
    println("TEST " + c)
    c % 3 match {
      case 0 => {
        net.inputs(0) set 1d
        net.inputs(1) set 1d
        net.inputs(2) set 1d
        net.inputs(3) set 1d

        net.inputs(4) set 0d
        net.inputs(5) set 0d
        net.inputs(6) set 0d
        net.inputs(7) set 0d
      }
      case 1 => {
        net.inputs(0) set 0d
        net.inputs(1) set 0d
        net.inputs(2) set 1d
        net.inputs(3) set 1d

        net.inputs(4) set 1d
        net.inputs(5) set 1d
        net.inputs(6) set 0d
        net.inputs(7) set 0d
      }
      case 2 => {
        net.inputs(0) set 0d
        net.inputs(1) set 0d
        net.inputs(2) set 0d
        net.inputs(3) set 0d

        net.inputs(4) set 1d
        net.inputs(5) set 1d
        net.inputs(6) set 1d
        net.inputs(7) set 1d
      }
    }
    net.calc()
    net.reset()
    c += 1
    c >= 100
  }

  def interact(input: String): Boolean = {
    if (input == "d") {
      net.neurons foreach { n => ; println(n + " " + n.inputs) }
      return false
    }
    val split = input.split(' ')
    for (i <- (0 to 7)) {
      try {
        net.inputs(i) set split(i).toDouble
      }
    }
    net.calc()
    net.outputs foreach { n => print(n.output.toInt + " ") }
    println
    net.reset()
    net.outputs foreach { n => print(n.output.toInt + " ") }
    println
    println
    false
  }

}