package neural

trait Input extends Neuron {
  override def calc(): Boolean = true
  override def timestep(): Boolean = { false }
  def set(a: Double): Unit = poutput = a
  def output_=(a: Double): Unit = poutput = a
}