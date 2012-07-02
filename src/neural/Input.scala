package neural

/**
 * trait for class Neuron, turns the Neurons to an Input
 */
trait Input extends Neuron {
  override def calc(): Boolean = true
  override def timestep(): Boolean = { false }
  
  /**
   * set the output of this input neuron
   */
  def set(a: Double): Unit = poutput = a
  
  def output_=(a: Double): Unit = poutput = a
}