package util.plot
import javax.swing.JPanel
import java.awt._
import collection.mutable.ListBuffer
class Plotter() extends JPanel {
  var graphs: Seq[(ListBuffer[Point], Color, Stroke)] = Seq()
  override def paintComponent(g: Graphics) = {
    val g2d = g.asInstanceOf[Graphics2D]
    graphs foreach { data =>
      val points = data._1
      g2d.setColor(data._2)
      g2d.setStroke(data._3)
      for (i <- (0 to points.size - 2)) {
        g2d.drawLine(points(i).x,
          points(i).y,
          points(i + 1).x,
          points(i + 1).y)
      }
    }
  }
}