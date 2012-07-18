package neural.mem
import neural._
import collection.parallel.mutable.ParArray
class HopNet(protected val size: Int, initWeight:Double) extends Net {
  var inputs = new ParArray[Input](size)
  var neurons = new ParArray[Neuron](size)
  def outputs = neurons
  def outputs_=(value:ParArray[Neuron]) = neurons=value
  
  init

  def init() = {

    for (i <- (0 to size - 1)) {
      inputs(i) = new InputNeuron { debug="i"+i}
      neurons(i) = new HebbNeuron { debug="n"+i; mu=0.08/size} //mu heuristic
      neurons(i).inputs += (inputs(i) -> 2)

      for (j <- (0 to i - 1)) {
        neurons(i).inputs += (neurons(j) -> initWeight)
        neurons(j).inputs += (neurons(i) -> initWeight)
      }
    }
  }
  
  override def timestep()={
    val ret=super.timestep()
    learn()
    ret
  }
  
  def learn()={
    neurons foreach {n=>
      n.asInstanceOf[HebbNeuron].learn()
    }
  }

 def this(size: Int) = this(size, 0d)
    
  def mu()=neurons(0).asInstanceOf[HebbNeuron].mu
  
  def mu_=(value:Double)={
    neurons foreach{n=>
      n.asInstanceOf[HebbNeuron].mu=value
    }
  }
  
  def heuristicSetMu(value:Double)={
    this.mu_=(value*(0.08/size))
  }
    
}