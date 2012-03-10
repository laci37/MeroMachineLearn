package neural.utils
import neural.dsl._
import neural.selfbuilding._
import collection.mutable.Map

object NetworkDrawer {
  def draw(output: Layer): String = {
    var str =
      "<svg viewBox=\"-500 -500 1000 1000\" version=\"1.1\">\n" +
        "<defs> \n" +
        "<marker viewBox=\"-1 -1 1 2\" " +
        "id=\"arrowhead\" refx=\"0\" refy=\"0\" orient=\"auto\" " +
        "fill=\"#ff0000\" markerWidth=\"1\" markerHeight=\"2\">\n" +
        "<polygon points=\"0,0 -1,-1 -1,1\" />\n" +
        "</marker>\n" +
        "<marker id = \"StartMarker\" viewBox = \"0 0 12 12\" " +
        "refX = \"12\" refY = \"6\" markerWidth = \"3\" " +
        "markerHeight = \"3\" stroke = \"green\" stroke-width = \"2\" " +
        "fill = \"none\" orient = \"auto\">\n" +
        "<circle cx = \"6\" cy = \"6\" r = \"5\"/>\n" +
        "</marker>\n" +
        "</defs>\n"

    var done = Map.empty[Layer, Info]
    def recursive(l: Layer): Unit = {
      var lasty = done(l).y - 200
      l.inputs foreach { il =>
        if (!done.contains(il)) {
          val x = done(l).x - 200
          val y = lasty + 200
          done += il -> new Info(x, y)
          lasty += 200
          val text = il match {
            case gl: GrowingLayer => {
              val unsigned: Int = math.abs(gl.actcode)
              gl.size.toString + " " +
                (unsigned match {
                  case a if a % 4 == 0 => "tanh"
                  case a if a % 4 == 1 => "lin"
                  case a if a % 4 == 2 => "lin"
                  case a if a % 4 == 3 => "signum"
                }) + "g"
            }
            case _ => il.size.toString
          }
          str += "<rect x=\"" + x.toString + "\" y=\"" +
            y.toString + "\" height=\"100\" width=\"100\" fill=\"white\"></rect>\n" + "" +
            "<text x=\"" + x + "\" y=\"" + (y + 50).toString + "\" fill=\"black\">" +
            text + "</text>\n"
          recursive(il)
        }
        arrow(il, l)
      }
    }
    def arrow(l1: Layer, l2: Layer) = {
      val relx = done(l2).x - done(l1).x
      val rely = done(l2).y - done(l1).y
      if (rely <= relx && rely <= (-relx)) {
        val x1 = done(l1).x + 50
        val y1 = done(l1).y
        val x2 = done(l2).x + 50
        val y2 = done(l2).y + 100
        str += "<line x1=\"" + x1 + "\" y1=\"" + y1 + "\" x2=\"" + x2 + "\" y2=\"" +
          y2 + "\" end=\"url(#arrowhead)\" stroke=\"black\" strokeWidth=\"0.1\"/>"
      } else if (rely <= relx && rely >= (-relx)) {
        val x1 = done(l1).x + 100
        val y1 = done(l1).y + 50
        val x2 = done(l2).x
        val y2 = done(l2).y + 50
        str += "<line x1=\"" + x1 + "\" y1=\"" + y1 + "\" x2=\"" + x2 + "\" y2=\"" +
          y2 + "\" end=\"url(#arrowhead)\" stroke=\"black\" strokeWidth=\"0.1\"/>"
      } else if (rely >= relx && rely >= (-relx)) {
        val x1 = done(l1).x + 50
        val y1 = done(l1).y + 100
        val x2 = done(l2).x + 50
        val y2 = done(l2).y
        str += "<line x1=\"" + x1 + "\" y1=\"" + y1 + "\" x2=\"" + x2 + "\" y2=\"" +
          y2 + "\" end=\"url(#arrowhead)\" stroke=\"black\" strokeWidth=\"0.01\"/>"
      } else if (rely >= relx && rely <= (-relx)) {
        val x1 = done(l1).x
        val y1 = done(l1).y + 50
        val x2 = done(l2).x + 100
        val y2 = done(l2).y + 50
        str += "<line x1=\"" + x1 + "\" y1=\"" + y1 + "\" x2=\"" + x2 + "\" y2=\"" +
          y2 + "\" end=\"url(#arrowhead)\" stroke=\"black\" strokeWidth=\"0.01\"/>"
      }
    }
    val text = output match {
      case gl: GrowingLayer => {
        val unsigned: Int = math.abs(gl.actcode)
        gl.size.toString + " " +
          (unsigned match {
            case a if a % 4 == 0 => "tanh"
            case a if a % 4 == 1 => "lin"
            case a if a % 4 == 2 => "lin"
            case a if a % 4 == 3 => "signum"
          }) + "g" + (if (gl.const) "c" else "")
      }
      case _ => output.size.toString
    }
    str += "<rect x=\"0\" y=\"0\" height=\"100\" width=\"100\" fill=\"white\"></rect>\n" +
      "<text x=\"0\" y=\"50\" fill=\"black\">" + text + "</text>"
    done += output -> new Info(0, 0)
    recursive(output)
    str += "</svg>"
    str
  }
  def drawToFile(output: Layer, filename: String): Unit = {
    import java.io._
    val out = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(filename), "UTF8"))
    out.write(draw(output))
    out.flush()
    out.close()
  }
  class Info(val x: Double, val y: Double)
}