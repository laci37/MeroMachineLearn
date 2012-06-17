package test
import annotation.tailrec
import actors.Future
import actors.Futures._
/**
 * base class for tests, provides basic IO for interactive testing
 */
abstract class TestBase() {
  //abstract functions for tests, if true is returned calculation will be stopped
  protected def calc(): Boolean
  protected def interact(input: String): Boolean

  def main(): Unit = {
    @tailrec def loop(): Unit = {
      loopBody
      loop
    }
    //call loop
    loop
  }

  /**
   * main as entry point, discards args
   */
  def main(args: Array[String]): Unit = main()

  protected var readFuture = future(Console.readLine)
  protected var doCalc = true
  protected def loopBody(): Boolean = {
    if (doCalc) doCalc = !calc
    if (readFuture.isSet) {
      val ret = interact(readFuture())
      readFuture = future(Console.readLine)
      ret
    } else false
  }
}