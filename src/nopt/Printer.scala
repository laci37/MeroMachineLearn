package nopt


trait Printer extends OptimizerBase {

  override def optCycle()={
    val result=super.optCycle()
    println(result.energy())
    result
  }
}