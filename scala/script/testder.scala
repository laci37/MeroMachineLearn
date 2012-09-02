import bioneural._
import dsl._
import util.plot._
import util.realtime._

val n = new FNNeuron
val i = n ic 0.5

val dc = new DataCollector(Seq(0d, 1d, n.v, n.vd))
dc.metadata=Array("0","1","V","Vd")

val sim= new SimulatorRoot(n,dc)

sim.simulate(0.05,10000)
