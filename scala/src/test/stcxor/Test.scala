package test.stcxor
import test.TestBase
import neural._
import meshed._
object Test extends TestBase{
  val evolver=new MeshEvolver(2,1,xorTest,(mg=> mg.fitness>0.999))
  
  def interact(s:String)=false
   
  def calc()={
    evolver.evolve()
    true
  }
  
  def xorTest(n: Net): Double = {
    var errsum = 0d
    n.inputs(0) set 0d
    n.inputs(1) set 0d
    n.calc(10)
    errsum += math.pow(n.outputs(0).output + 1d, 2)
    n.inputs(0) set 0d
    n.inputs(1) set 1d
    n.calc(10)
    errsum += math.pow(n.outputs(0).output - 1d, 2)
    n.inputs(0) set 1d
    n.inputs(1) set 0d
    n.calc(10)
    errsum += math.pow(n.outputs(0).output - 1d, 2)
    n.inputs(0) set 1d
    n.inputs(1) set 1d
    n.calc(10)
    errsum += math.pow(n.outputs(0).output + 1d, 2)  
    if (!(errsum.isNaN || errsum.isInfinity)) math.pow(2, -errsum) else 0
  }
}