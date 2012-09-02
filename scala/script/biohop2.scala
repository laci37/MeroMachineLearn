import bioneural._
import mem._
import util.plot._
import util.realtime._

val net = new HopNet(4)
val dc = net.createDC()
val sim = new SimulatorRoot(net)
for (i <- (1 to 10)) {
  net.setInputs(true, Array(true, true, false, false))
  sim.simulate(0.05, 1000)
  net.setInputs(false, Array(false, false, false, false))
  sim.simulate(0.05, 500)
  net.setInputs(true, Array(false, true, true, false))
  sim.simulate(0.05, 1000)
  net.setInputs(false, Array(false, false, false, false))
  sim.simulate(0.05, 500)
  net.setInputs(true, Array(false, false, true, true))
  sim.simulate(0.05, 1000)
  net.setInputs(false, Array(false, false, false, false))
  sim.simulate(0.05, 500)
}
net.setInputs(false, Array(true, false, false, false))
sim.simulate(0.05, 10000)
net.setInputs(false, Array(false, false, false, true))
sim.simulate(0.05, 10000)
net.createDC.desc = "Simulating four unit hopnet, teaching patterns (1100)(0110)(0011) and testing with inputs(1000)(0001)"