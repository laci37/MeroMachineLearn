package neural.selfbuilding


class GrowingGenome(var out: GrowingLayer) extends Ordered[GrowingGenome] {
  var fit = Double.NaN
  def decode() = {
    val bdr = new GrowingNetworkBuilder(out)
    bdr.build()
  }
  
  override def compare(that:GrowingGenome)= this.fit.compare(that.fit)
  override def clone()={
    new GrowingGenome(out.clone){fit=this.fit}
  }
}

/*object GrowingGenome{
  def apply(nInputs:Int, nOutputs:Int)={
    
  }
}*/