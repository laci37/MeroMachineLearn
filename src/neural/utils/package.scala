package neural
import neural.dsl._
import neural.sb.MutableLayer
import neural.selfbuilding.GrowingLayer
import annotation.tailrec
import collection.mutable.Map

package object utils {
  def createScriptForPlan(plan: Layer): String = {
    var ret = ""
    var c = 0 //counter for assigning layer names
    var names = Map.empty[Layer, String]

    def inner(l: Layer): Unit = {
      if (!names.contains(l)) {
        l match {
          case ml: MutableLayer => {
            ret += "val l" + c + " = " + ml.size + " "
            math.abs(ml.actcode) match {
              case a if a % 4 == 0 => ret += "tanhm"
              case a if a % 4 == 1 => ret += "linm"
              case a if a % 4 == 2 => ret += "linm"
              case a if a % 4 == 3 => ret += "sgnm"
            }
            if (ml.const) ret += "c"
          }
          case _ => {
            ret += "val l" + c + " = " + l.size + " " + l.debug
          }
        }
        ret += "\n"
        names += l -> ("l" + c)
        c += 1
        l.inputs foreach { il =>
          inner(il)
          ret += names(il) + " >> " + names(l) + "\n"
        }
      }
    }
    inner(plan)
    ret
  }
}