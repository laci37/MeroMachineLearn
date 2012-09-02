package test.biorecog
import javax.swing._
import java.awt._
import bioneural._
import mem._
import util.realtime._
class Panel extends JPanel {
  val net = new HopNetwDetect(9) with LimitICNeurons
  var inhibit = true
  var inputs = new Array[Boolean](9)
  val sim = new SimulatorRoot(net)
  val ldc = net.createLDC()
  var alwaysRepaint=true

  override def paintComponent(g: Graphics) = {
    for (i <- (0 to 2)) for (j <- (0 to 2)) {
      if (net.detect(i * 3 + j).firing) g.setColor(Color.RED)
      else g.setColor(Color.BLACK)
      g.fillRect(i * 50, j * 50, 50, 50)
      if (inputs(i * 3 + j)) g.setColor(Color.RED)
      else g.setColor(Color.BLACK)
      g.fillRect(i * 50 + 150, j * 50, 50, 50)
    }
  }

  var samples = Array(
    Array(false, false, false,
      true, true, true,
      false, false, false),
    Array(false, true, false,
      true, true, true,
      false, true, false),
    Array(true, false, true,
      false, true, false,
      true, false, true),
    Array(true, false, true,
      true, true, true,
      true, false, true))
  var rest = Array.fill(9)(false)

  var restTime=100
  var teachTime=100
  
  def present(example: Array[Boolean], time: Double) = {
    inputs = example
    net.setInputs(inhibit, example)
    sim.simulate(0.05, (time / 0.05).toInt)
    if(alwaysRepaint) repaint()
    net.setInputs(false, rest)
    sim.simulate(0.05, (restTime/0.05).toInt)
  }

  def teach(c: Int) = {
    for(i<-(1 to c)) samples foreach { s => present(s, teachTime) }
  }
}