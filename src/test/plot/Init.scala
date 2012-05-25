package test.plot
import util.plot._
import java.awt._
class Init {
  val plot = new TimePlotter((Color.red, new BasicStroke(2)),
    (Color.blue, new BasicStroke(4)))
  val wnd = new TimePlotFrame(plot)
}