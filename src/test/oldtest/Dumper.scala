package test
import neural._
import java.io._
object Dumper {
  def dump(n: Net, filename: String) = {
    val out = new StringBuilder
    n.neurons.seq foreach { ne =>
      out append ne.debug + "\n--------\n"
      ne.inputs foreach { in =>
        out append in._1.debug + " "
        out append in._2
        out append "\n"
      }
      out append "\n"
    }
    try {
      val file = new File(filename)
      val fw = new FileWriter(file)
      fw.write(out.mkString)
      fw.flush()
    } catch {
      case e => println("Exception: " + e)
    }
  }
}