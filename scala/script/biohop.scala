import bioneural._
import mem._
import util.plot._
import util.realtime._

val net = new HopNet(4)
net.createDC()
val sim = new SimulatorRoot(net)
net.setInputs(true, Array(true, true, false, false))
sim.simulate(0.05, 20000)
net.setInputs(true, Array(false, true, true, false))
sim.simulate(0.05, 20000)
net.setInputs(true, Array(false, false, true, true))
sim.simulate(0.05, 20000)
net.setInputs(false, Array(true, false, false, false))
sim.simulate(0.05, 20000)
net.setInputs(false, Array(false, false, false, true))
sim.simulate(0.05, 20000)
net.createDC.desc = "Simulating four unit hopnet, teaching patterns (1100)(0110)(0011) and testing with inputs(1000)(0001)"