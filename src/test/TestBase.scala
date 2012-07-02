package test
import annotation.tailrec
import actors.Future
import actors.Futures._
/**
 * base class for tests, provides basic IO for interactive testing
 */
abstract class TestBase() {
  
  /**
   * abstract function, body of the work loop, if returns true it wont be called anymore
   */
  protected def calc(): Boolean
  
  /**
   * abstract function for interaction with the user, if returns true the program stops
   */
  protected def interact(input: String): Boolean

  /**
   * the main function
   */
  def main(): Unit = {
    //inner function for using tailrec without final
    @tailrec def loop(): Unit = {
      if(loopBody) return
      loop
    }
    
    //call loop
    loop
  }

  /**
   * main as entry point, discards args
   */
  def main(args: Array[String]): Unit = main()
  
  /**
   * this future reads the console input async
   */
  protected var readFuture = future(Console.readLine)
  
  /**
   * if set to false calc won't be called
   */
  protected var doCalc = true
  
  /**
   * the body of the main loop
   */
  protected def loopBody(): Boolean = {
    if (doCalc) doCalc = !calc //do work
    if (readFuture.isSet) { // of the future has received input
      val ret = interact(readFuture()) //handle it with interact
      readFuture = future(Console.readLine) //reset the future
      ret
    } else false //if no input was received continue
  }
}