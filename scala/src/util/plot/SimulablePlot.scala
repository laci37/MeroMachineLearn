package util.plot
import util.realtime._
import collection.mutable.{ Seq, ListBuffer }
import java.awt.{ Point, Color, BasicStroke }
import annotation.tailrec
class SimulablePlot extends Plotter with Simulable {
  var xscale = 1d
  var yscale = 20d
  var xaxis = 200
  var time = 0d
  var qtime = 0
  def timestep(dt: Double) = {
    time += dt
    if (qtime != (time * xscale).toInt) {
      qtime = (time * xscale).toInt
      addPoints()
    }
  }

  def addPoints(): Unit = {
    if (graphs.size == data.size) {
      for (i <- (0 to data.size - 1)) {
        graphs(i)._1.append(new Point(qtime, data(i)().toInt))
      }
    } else {
      while (graphs.size < data.size) {
        graphs = graphs :+ (ListBuffer[Point](), getColor(graphs.size), stroke)
      }
      addPoints()
    }
  }

  var data = Seq[Unit => Double]()
  var stroke = new BasicStroke

  def getColor(num: Int) = {
    new Color(num * 127)
  }

  def addData(d: Unit => Double) = data = data :+ d

  def update() = {}
}