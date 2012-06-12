package nopt

abstract class DoubleArrayState(data:Array[Double]) extends State{
  val rand=scala.util.Random
 
  protected def calcNeighborData()={
    val newdata=data.clone()
    for(i<-(0 to newdata.size-1)){
      newdata(i)=mutateDouble(newdata(i))
    }
    newdata
  }
  
  protected def mutateDouble(d:Double)={
    if(rand.nextDouble<0.1){
        d+(rand.nextDouble()-0.5)*0.2
    } else d 
  }
  
}