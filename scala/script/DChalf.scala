/*
 * Simulates a neuron firing half the frequency as its input 
 * Stores data with a DataCollector
 */

import bioneural._
import util.plot._
import util.realtime._
import javax.swing.JFrame

var n1 = new FNNeuron
var n2 = new FNNeuron
var s1 = new Synapse(n1,n2)
s1.mc=1

var i1=0.5d
def in1(v:Double)=i1


n1.inputs.add(in1)

var dc= new DataCollector(Seq(
Unit=>0d,
Unit=>n1.v,
Unit=>n1.w,
Unit=>n2.v,
Unit=>n2.w,
Unit=>s1.c,
Unit=>s1.k,
Unit=>s1.I,
n1.vd,
n2.vd,
Unit=>1d))

dc.metadata= Array[String](
"0",
"n1.v",
"n1.w",
"n2.v",
"n2.w",
"s1.c",
"s1.k",
"s1.I",
"1"
)

var sim=new SimulatorRoot(n1,n2,s1,dc)

var timestep=0.05d
def simulate(c:Int)={
  for(i<-(1 to c)) sim.timestep(timestep)
}