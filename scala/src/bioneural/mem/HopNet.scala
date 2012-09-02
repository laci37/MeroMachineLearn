package bioneural.mem
import bioneural._
import util.realtime._
import dsl._
import collection.mutable.ArrayBuffer
class HopNet(val size: Int) extends Simulable {
  val neurons =new Array[FNNeuron](size)
  val synapses = new Array[Array[HebbSynapse]](size)
  val ic = new Array[InputCurrent](size)
  val sim = new Simulator()
  protected var dc: Option[DataCollector] = None
  protected var ldc: Option[LargeDataCollector]= None
  //init synapses and input currents
  for (i <- (0 to size - 1)) {
    initNeuron(i)
  }
  
  protected def createNeuron(i:Int)={
    neurons(i)=new FNNeuron
  }
  
  protected def initNeuron(i:Int)={
    createNeuron(i)
    sim.objects.add(neurons(i))
    ic(i) = neurons(i) ic 0d
    synapses(i) = new Array[HebbSynapse](size)
    for (j <- (0 to i-1)) {
      createSynapse(i,j)
      createSynapse(j,i)
    }
  }
  
  protected def createSynapse(i:Int,j:Int)={
    synapses(i)(j) = neurons(i) hsyn neurons(j)
    sim.objects.add(synapses(i)(j))
  }
  //synapse varaible set methods
  def setPc(pc: Double) = synapses foreach { arr => arr foreach { syn => if (syn != null) syn.pc = pc } }
  def setPk(pk: Double) = synapses foreach { arr => arr foreach { syn => if (syn != null) syn.pk = pk } }
  def setMc(mc: Double) = synapses foreach { arr => arr foreach { syn => if (syn != null) syn.mc = mc } }
  def setMk(mk: Double) = synapses foreach { arr => arr foreach { syn => if (syn != null) syn.mk = mk } }
  def setMu(mu: Double) = synapses foreach { arr => arr foreach { syn => if (syn != null) syn.mu = mu } }

  //export simulator methods
  def timestep(dt: Double) = sim.timestep(dt)
  def update() = sim.update()
  var defaultEx = 0.5
  var defaultIn = -0.5

  def setInputs(inhibit: Boolean, in: Array[Boolean]) = {
    for (i <- (0 to size - 1)) {
      ic(i) set (if (in(i)) defaultEx else if (inhibit) defaultIn else 0d)
    }
  }

  def setInputs(in: Array[Double]) = {
    for (i <- (0 to size - 1)) {
      ic(i) set in(i)
    }
  }

  def createDC(): DataCollector = {
    if (dc.isEmpty) {
      val func = new ArrayBuffer[Unit => Double](size * (3 + (size - 1) * 3))
      val meta = new ArrayBuffer[String](size * (2 + (size - 1) * 3))
      for (i <- (0 to size - 1)) {
        1
        func.append(neurons(i).v, neurons(i).vd, ic(i).I)
        meta.append("n(" + i + ").v", "n(" + i + ").vd", "ic(" + i + ")")
        for (j <- (0 to size - 1) if (i != j)) {
          func.append(synapses(i)(j).g, synapses(i)(j).k, synapses(i)(j).c)
          meta.append("s(" + i + ")(" + j + ").g", "s(" + i + ")(" + j + ").k", "s(" + i + ")(" + j + ").c")
        }
      }
      dc = Some(new DataCollector(func) { metadata = meta.toArray })
      sim.objects.add(dc.get)
    }
    dc.get
  }
  def createLDC(): LargeDataCollector = {
    if (ldc.isEmpty) {
      val func = new ArrayBuffer[Unit => Double](size * (3 + (size - 1) * 3))
      val meta = new ArrayBuffer[String](size * (2 + (size - 1) * 3))
      for (i <- (0 to size - 1)) {
        1
        func.append(neurons(i).v, neurons(i).vd, ic(i).I)
        meta.append("n(" + i + ").v", "n(" + i + ").vd", "ic(" + i + ")")
        for (j <- (0 to size - 1) if (i != j)) {
          func.append(synapses(i)(j).g, synapses(i)(j).k, synapses(i)(j).c)
          meta.append("s(" + i + ")(" + j + ").g", "s(" + i + ")(" + j + ").k", "s(" + i + ")(" + j + ").c")
        }
      }
      ldc = Some(new LargeDataCollector(func) { metadata = meta.toArray })
      sim.objects.add(ldc.get)
    }
    ldc.get
  }
}