package taxisopt
import neural.dsl._
import optimization._
import neural.opt._
import neural.Net
object Init {

  def main(args: Array[String]): Unit = {
    val plan= 2 lin
    val l1= 1 lin
    val l2= 2 lin;
    l1 >> plan
    l2 >> plan
    l2 >> l1
    val weights=Array[Double](0d,0d,0d,0d,-1d,0d,-1d,1d,0d,0d,-1d)
    val opt= new Optimizer(new WeightState(weights,plan,test),1d)
    while (true){
      opt.optimize(1)
      println(opt.optimalCost)
    }
  }
  
  def test(n:Net)={
    import taxis.Vector
    var sum = 0d
    for (j <- (1 to 10)) {
      var v = new Vector(0, 0)
      var p = Vector.fromAngleLength(j, 100d)

      for (i <- (1 to 100)) {
        n.inputs(0) set p.angle
        n.inputs(1) set p.length
        n.timestep()
        v += Vector.fromAngleLength(n.outputs(0).output, n.outputs(1).output)
        p += v
        sum += p.length
      }
    }
    if (!(sum.isNaN || sum.isInfinite)) -sum
    else Double.PositiveInfinity
  }

}