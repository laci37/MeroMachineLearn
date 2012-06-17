package nopt
import java.io._

/**
 * Mixin trait for implementing the logging of visited states
 */
trait StateLogging extends OptimizerBase{ 
  var out:Writer= new OutputStreamWriter(java.lang.System.out)
  var c=0
  
  override def collectHeuristicData(newState:State)={
    c+=1
    out.write("##### c= "+c+" ######\n")
    out.write("curState= "+ curState+"\n")
    out.write("newState= "+ newState+"\n")
    out.write("optimum= "+ optimum+"\n")
    out.flush()
    super.collectHeuristicData(newState)
  }
}