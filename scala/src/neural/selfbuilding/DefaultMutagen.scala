package neural.selfbuilding
import neural.dsl._
import collection.mutable._
class DefaultMutagen extends Mutagen {
  override def apply(g: GrowingGenome) = {
    g.fit = Double.NaN
    var layers = new ArrayBuffer[Layer]
    def discover(g: GrowingLayer): Unit = {
      g.inputs foreach { l =>
        if (!layers.contains(l)) {
          layers += l
          l match {
            case gl: GrowingLayer => discover(gl)
            case _ =>
          }
        }
      }
    }
    layers += g.out
    discover(g.out)
    layers foreach { l =>
      l match {
        case gl: GrowingLayer => {
          if (addInput > rand.nextDouble) {
            gl.addInput(layers(rand.nextInt(layers.size)))
          }
          apply(gl)
        }
        case _ =>
      }
    }
  }

  override def apply(l: GrowingLayer): Unit = {
    if (insertLayer > rand.nextDouble) {
      val input = l.inputs(rand.nextInt(l.inputs.size))
      val newlayer = l.insertLayer(input)
      apply(newlayer.get)
    }
    if (addNeuron > rand.nextDouble) l.addNeuron()
    for (i <- Range(0, l.data.size)) {
      if (wrate > rand.nextDouble) l.data(i) += (rand.nextDouble * 2 - 1) * wmag
    }
    if (changeact > rand.nextDouble) l.actcode = (rand.nextInt % 256).asInstanceOf[Byte]
  }

  override def updateFit(newfit: Double): Unit = {
    lastfit = fit
    fit = newfit
    wmagcorr += (
        if((fit-lastfit)<0.01) 0.001
        else if(wmagcorr>0) -wmagcorr
        else 0d)
    println("mc: "+wmagcorr)
    wratecorr += (
        if((fit-lastfit)<0.01) 0.001
        else if(wratecorr>0) -wratecorr
        else 0d)
    println("rc: "+wmagcorr)
    gen += 1
    state match {
      case Begin => state = FirstGen
      case FirstGen => {
        state = SBegin(1)
        gen = 0
      }
      case SBegin(n) => {
        if (fit - lastfit > 0.5) {
          state = S(n)
          gen = 0
        } else if (gen > 20) {
          state = F(n)
          gen = 0
        }
      }
      case F(n) =>
        if (fit - lastfit > 0.5 || gen > 1000) {
          state = S(n)
          gen = 0
        }
      case S(n) => {
        if (fit - lastfit < 0.001 && gen > 200) {
          state = SBegin(n + 1)
          gen = 0
        }
      }
    }
    println(state)
  }

  def insertLayer() = {
    state match {
      //case SBegin(n) if n > 2 => 0.5d
      case _ => 0d
    }
  }

  def addInput() = {
    state match {
      //case SBegin(n) if n > 3 => 0.1d
      case _ => 0d
    }
  }

  def addNeuron() = {
    state match {
      //case SBegin(n) if n > 1 => 0.9d
      case _ => 0d
    }
  }

  def wrate() = {
    (state match {
      case S(n) if n > 1 => 0.1d
      case F(n) => 0.1d
      case _ => 1d
    }) + wratecorr
  }
  def wmag() = (0.1 / fit) + wmagcorr
  var changeact = 0.0d
  var lastfit = 0.d
  var fit = 0.01d
  var state: State = Begin
  val rand = scala.util.Random
  var gen = 0
  var wratecorr = 0d
  var wmagcorr = 0d

  abstract class State
  case object Begin extends State
  case object FirstGen extends State
  case class S(n: Int) extends State
  case class SBegin(n: Int) extends State
  case class F(n: Int) extends State

}
