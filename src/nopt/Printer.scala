package nopt

/**
 * debug extension for OptimizerBase, prints out all energies
 */
trait Printer extends OptimizerBase {

  override def optCycle()={
    val result=super.optCycle()
    println(result.energy())
    result
  }
}