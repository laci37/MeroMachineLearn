package neural.meshed
import nga._
import neural._
class MeshGenome(nInputs: Int, nNeurons: Int, weights: Array[Double], test: (Net => Double), var minNeurons: Int)
  extends DoubleArrayGenome(weights) with Decodable[MeshedNet] {

  def this(nInputs: Int, nNeurons: Int, weights: Array[Double], test: (Net => Double)) = {
    this(nInputs, nNeurons, weights, test, nNeurons)
  }

  def this(nInputs: Int, nNeurons: Int, test: (Net => Double)) = {
    this(nInputs, nNeurons, new Array[Double](nNeurons * (nNeurons + nInputs + 1)), test)
  }

  lazy val fitness = test(decode) * math.pow(0.999, nNeurons)

  def decode = MeshedNet(nInputs, nNeurons, weights)

  protected override def create(data: Array[Double]) =
    new MeshGenome(nInputs, nNeurons, data, test) { minNeurons = this.minNeurons }.asInstanceOf[this.type]

  var pAdd = 0.01d
  var pRemove = 0.005d
  protected def createMutated(data: Array[Double]) = {
    val rand = scala.util.Random
    if (rand.nextDouble() < pAdd) {
      val ndata = new Array[Double]((nNeurons + 1) * (nNeurons + nInputs + 2))
      Array.copy(data, 0, ndata, 0, data.size)
      new MeshGenome(nInputs, nNeurons + 1, ndata, test) { minNeurons = this.minNeurons }.asInstanceOf[this.type]
    } else if ((rand.nextDouble() < pAdd) && nNeurons > minNeurons) {
      val ndata = new Array[Double]((nNeurons - 1) * (nNeurons + nInputs))
      Array.copy(data, 0, ndata, 0, ndata.size)
      new MeshGenome(nInputs, nNeurons - 1, ndata, test) { minNeurons = this.minNeurons }.asInstanceOf[this.type]
    } else create(data.clone)
  }

  override def X(that: Genome) = {
    that match {
      case that: MeshGenome => {
        val thatdata = new Array[Double](this.data.size)
        Array.copy(that.data, 0, thatdata, 0, math.min(that.data.size, this.data.size))
        createMutated(calcData(thatdata))
      }
      case _ => throw new IllegalArgumentException("that must be of type MeshGenome")
    }
  }

  override def mutateDouble(d: Double) = {
    import math._
    if (rand.nextDouble < mut) {
      val change = rand.nextGaussian() * mut2
      if (signum(change) != signum(d) && abs(change) > abs(d)) 0d
      else d + change
    } else d
  }
}