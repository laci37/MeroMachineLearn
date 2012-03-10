package debug
import java.io.FileWriter
import java.io.IOException
class Logger(filename: String) {
  val stream = new FileWriter(filename)
  def log(obj:AnyRef, comment:String)={
    try{
      stream.write(obj.hashCode().toString())
      stream.write(comment)
    } catch {
      case ex:IOException => println("Failed to write log file")
    }
  }
}