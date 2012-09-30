package test.hopcomp
import javax.swing.{ JFrame }
import java.awt.{ Graphics, Graphics2D, Color, BasicStroke, FlowLayout }
import util.display._
import util.realtime._
import bioneural.mem.{ HopNetwDetect ⇒ BioHopNet, LimitICNeurons }
import neural.mem.{ HopNet ⇒ DiscHopNet }
import neural.utils.ActWeightCollector
import actors.Futures._
class CompareWindow extends JFrame {
  //UI
  val gdIn = new GridDisplay(4, 4)
  val gdDiscOut = new GridDisplay(4, 4)
  val gdFNOut = new GridDisplay(4, 4)
  initUI

  //neural networks
  val bio = new BioHopNet(16) with LimitICNeurons
  bio.createLDC()
  val sim = new SimulatorRoot(bio)
  //example presentation variables
  var presentTime = 100
  var restTime = 100
  var inhibit = true
  var restPattern = Array.fill(16)(false)
  var restInhibit= -0.5d

  val disc = new DiscHopNet(16) with ActWeightCollector

  protected def initUI() = {
    gdIn.autoRepaint = true
    gdFNOut.autoRepaint = true
    gdDiscOut.autoRepaint = true
    getContentPane.setLayout(new FlowLayout)
    getContentPane.add(gdIn)
    getContentPane.add(gdDiscOut)
    getContentPane.add(gdFNOut)
    this.pack()
  }

  def present(example: Array[Boolean]) = {
    if (example.size != 16) throw new IllegalArgumentException("bad example")
    gdIn.data = example
    gdDiscOut.colFalse = Color.BLUE
    gdFNOut.colFalse = Color.BLUE
    future { //calculate disc time on separate thread
      for (i ← (0 to 15))
        if (example(i)) disc.inputs(i).set(1)
        else if (inhibit) disc.inputs(i).set(-1)
        else disc.inputs(i).set(0)
      disc.calc(10000)
      updateDisc()
      disc.reset()
    }
    future { //calc FN network on another thread
      bio.setInputs(inhibit, example)
      sim.simulate(0.05, (presentTime / 0.05).toInt)
      updateFN()
      bioRest()
    }
  }

  def bioRest() = {
    val defInSave = bio.defaultIn
    bio.defaultIn = restInhibit
    bio.setInputs(true, restPattern)
    sim.simulate(0.05, (restTime / 0.05).toInt)
    bio.defaultIn = defInSave
  }

  def presentBlocking(example: Array[Boolean]) = {
    present(example)()
  }

  def updateDisc() = {
    gdDiscOut.data = (for (i ← (0 to 15)) yield disc.outputs(i) > 0.5).toArray
    gdDiscOut.colFalse = Color.BLACK
  }

  def updateFN() = {
    gdFNOut.data = (for (i ← (0 to 15)) yield bio.detect(i).firing).toArray
    gdFNOut.colFalse = Color.BLACK
  }

  def teach(examples: Array[Array[Boolean]], cycles: Int) = {
    for (i ← (1 to cycles)) {
      examples foreach presentBlocking _
    }
  }

  def randomExample() = {
    (for (i ← (0 to 15)) yield scala.util.Random.nextBoolean).toArray
  }
}