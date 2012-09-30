package bioneural

trait ICNoise extends FNNeuron{
  val rand=scala.util.Random
  var magnitude=0.1
  override def I={
    super.I+rand.nextGaussian*magnitude
  }
}