package bioneural

class InputCurrent(n:FNNeuron) {
  var I=0d
  def apply(v:Double)=I
  n.inputs.add(this.apply)
  
  def this(n:FNNeuron,i:Double)={
    this(n)
    I=i
  }
  
  def set(d:Double)= I=d
}

