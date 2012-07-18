package neural.dsl
import neural._
import collection.parallel.mutable.ParArray
/**
 * BuilderClass for non-recursive networks, can be used to build 
 * single or multi-layer perceptrons
 */
class NRNetworkBuilder(override val output: Layer) extends NetworkBuilder(output) {
  //discovering size of network with a recursive function
  var nInputs = 0
  var nNeurons = 0
  var nOutputs = output.size

  private def discover(l: Layer): Unit = {
    if (l.inputs == Seq.empty) nInputs += l.size //if there are no previous layers this is the input layer
    else {
      nNeurons += l.size
      l.inputs.foreach(
        x => discover(x)) //discover previous layers
    }
  }
  discover(output)

  
  override def build(cdata: Array[Double]): Net = {
    val n = new GeneralNet(nInputs, nNeurons, nOutputs)
    var dPtr = 0 //pointer in data
    var iPtr = 0 //pointer in inputs
    var nPtr = 0 //pointer in neurons
    var oPtr = 0 //pointer for outputs
    //local function for fetching numbers from data
    def getNextDouble() = {
      val ret = cdata(dPtr)
      dPtr += 1
      ret
    }
    //local function for building layers
    def buildLayer(l: Layer): (ParArray[Neuron], Int) = {
      if (l.inputs != Seq.empty) {
        val begin = nPtr //store nPtr for returning
        nPtr += l.size
        //creating neurons, applying bias
        for (i <- Range(0, l.size)) {
          n.neurons(begin + i) = new Neuron(l.actfunc) { debug = l.debug + " " + i }
          n.neurons(begin + i).inputs += n.bias -> getNextDouble()
        }
        //going through all input layers
        l.inputs foreach (il => {
          val prev = buildLayer(il) //build it
          //connect each neuron to each fetching weights from data
          for (i <- Range(0, l.size)) for (j <- Range(prev._2, prev._2 + il.size)) {
            n.neurons(begin + i).inputs += prev._1(j) -> getNextDouble()
          }
        })
        (n.neurons, begin)
      } else //input layer
      {
        val begin = iPtr //store for returning
        iPtr += l.size
        for (i <- Range(0, l.size)) {
          n.inputs(begin + i) = new Neuron(l.actfunc) with Input
        }
        (n.inputs.asInstanceOf[ParArray[Neuron]], begin)
      }
    }
    //enough to call on output layer, will recurse on other layers
    val outp = buildLayer(output)
    for (i <- Range(outp._2, outp._2 + output.size)) {
      n.outputs(oPtr) = n.neurons(i)
      oPtr += 1
    }
    n
  }
}