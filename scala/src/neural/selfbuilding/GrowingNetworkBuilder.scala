package neural.selfbuilding
import collection.mutable.Map
import neural.dsl._
import neural._

class GrowingNetworkBuilder(output: GrowingLayer) {
  var nInputs = 0
  var nNeurons = 0
  var nOutputs = output.size
  //class for storing info about the network building process
  private class Info {
    var input = false
    var built = false
    var neurons = Seq.empty[Neuron]
  }
  //we will map info to layers here
  private var infomap = Map.empty[Layer, Info]
  //recursive function for discovering the structure and mapping info 
  private def discover(l: Layer): Unit = {
    if (!infomap.contains(l)) {
      if (l.inputs == Seq.empty) {
        nInputs += l.size //if there are no previous layers this is the input layer
        infomap += l -> new Info { input = true }
      } else {
        nNeurons += l.size
        infomap += l -> new Info
        l.inputs.foreach(
          x => discover(x)) //discover previous layers
      }
    }
  }
  discover(output)

  private def buildLayer(l: Layer): Unit = {
    l match {
      case g: GrowingLayer => buildGrowing(g)
      case _ => {
        if (l.inputs != Seq.empty) throw new Exception("Data not present in layer")
        else {
          infomap(l).neurons = for (i <- Range(0, l.size)) yield new Neuron(l.actfunc) with Input
          infomap(l).built = true
        }
      }
    }
  }

  private def buildGrowing(l: GrowingLayer): Unit = {
    val info = infomap(l)
    var dp = 0
    info.neurons = for (i <- Range(0, l.size)) yield new Neuron(l.actfunc())
    info.built = true
    l.inputs foreach { il =>
      if (!infomap(il).built) buildLayer(il)
    }
    info.neurons foreach { n =>
      l.inputs foreach { il =>
        infomap(il).neurons foreach { in =>
          try {
            n.inputs += in -> l.data(dp)
          } catch {
            case _ =>
          }
          dp += 1
        }
      }
    }
    l.debug += "|"
  }

  def build(): Net = {
    buildLayer(output)
    val ret = new GeneralNet(nInputs, nNeurons, nOutputs)
    var np = 0 //pointer fo array copy
    var ip = 0
    var op = 0
    infomap foreach { kv =>
      if (kv._2.input) {
        kv._2.neurons foreach { n =>
          ret.inputs(ip) = n.asInstanceOf[Neuron with Input]
          if (ret.inputs(ip) == null) {
            println("fuck")
          }
          ip += 1
        }
      } else {
        kv._2.neurons foreach { n =>
          ret.neurons(np) = n
          if (ret.neurons(np) == null) {
            println("fuck")
          }
          np += 1
        }
      }
    }
    infomap(output).neurons foreach { n =>
      ret.outputs(op) = n
      if (ret.outputs(op) == null) {
        println("fuck")
      }
      op += 1
    }
    ret
  }

}