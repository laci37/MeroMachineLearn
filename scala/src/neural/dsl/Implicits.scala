package neural
import neural.selfbuilding._
import neural.sb._
/**
 * package object for dsl, the implicit functions and classes used for the neural 
 * network structure domain specific language are defined here
 */
package object dsl {
  //cast for the class implementing extra functionality for building layers of an Int
  implicit def Int2PimpedInt(a: Int) = new PimpedInt(a)

  //class for extra integer methods
  class PimpedInt(val inner: Int) {
    //these methods construct different kinds of layers
    
    def act(func: Double => Double) = new Layer(inner, func) { debug = "custom" }
    def lin() = new Layer(inner, x => x) { debug = "lin" }
    def tanh() = new Layer(inner, x => math.tanh(x)) { debug = "tanh" }
    
    //failed experimental layer subclasses
    def ling() = GrowingLayer(inner, 1.toByte)
    def tanhg() = GrowingLayer(inner, 0.toByte)
    def lingc() = GrowingLayer(inner, 1.toByte, true)
    def tanhgc() = GrowingLayer(inner, 0.toByte, true)

    def linm() = new MutableLayer(defaultActDecode, inner, false) { actcode = 1 }
    def tanhm() = new MutableLayer(defaultActDecode, inner, false) { actcode = 0 }
    def linmc() = new MutableLayer(defaultActDecode, inner, true) { actcode = 1}
    def tanhmc() = new MutableLayer(defaultActDecode, inner, true) { actcode = 0}
  }

  def defaultActDecode(actcode: Int): (Double => Double) = {
    val unsigned: Int = math.abs(actcode)
    unsigned match {
      case a if a % 4 == 0 => x: Double => math.tanh(x)
      case a if a % 4 == 1 => x: Double => x
      case a if a % 4 == 2 => x: Double => x
      case a if a % 4 == 3 => x: Double => math.signum(x)
    }
  }
}