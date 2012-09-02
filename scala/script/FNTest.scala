import bioneural._
import util.plot._
import util.realtime._
import javax.swing.JFrame

var n1 = new FNNeuron
var n2 = new FNNeuron
var s1 = new Synapse(n1,n2)
var s2 = new Synapse(n2,n1)
s2.g = -0.1
var i1=0.5d
def in1(v:Double)=i1

n1.inputs.add(in1)


var plot= new SimulablePlot()
plot.addData(Unit=>0d)
plot.addData(Unit=>n1.v)
plot.addData(Unit=>n2.v)
plot.addData(Unit=>s1.c)
plot.addData(Unit=>s2.c)
plot.addData(Unit=>s1.k)
plot.addData(Unit=>s2.k)
plot.addData(Unit=>s1.I)
plot.addData(Unit=>s2.I)
plot.addData(Unit=>1d)

var wnd= new JFrame
wnd.setSize(500,200)
wnd.getContentPane().add(plot)
wnd.show()

var sim=new SimulatorRoot(n1,n2,s1,s2,plot)

var timestep=0.05d
def simulate(c:Int)={
  for(i<-(1 to c)) sim.timestep(timestep)
  plot.repaint()
}