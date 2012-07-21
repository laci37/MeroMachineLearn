package mathext

class Vector(val size:Int) extends Matrix(1,size) {
  //will use this array for opted calculations
  protected var darr=arr(0)
  
  def this(initarr:Array[Double])={
    this(initarr.size)
    this.arr(0)=initarr
    darr=arr(0)
  }
}