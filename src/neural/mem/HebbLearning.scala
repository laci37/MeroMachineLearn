package neural.mem

import neural.Neuron

trait HebbLearning extends Neuron {
  //learning rate
  var mu = 0.1d
  //max of abs(weight) achievable by learning 
  var maxabs=1d
  //nonlinear incrementation
  def inc(x: Double, d: Double) = {
    import math._
    def atanh(x: Double) = {
      (1 / 2d) * log((1 + x) / (1 - x))
    }
    if (abs(x) < maxabs && abs(maxabs * tanh(atanh(x / maxabs) + d)) < maxabs) 
      maxabs * tanh(atanh(x / maxabs) + d)
    else x
  }

  def learn() = {
    inputs foreach { kv =>
      if (kv._1.output > 0.5) {
        if (this.pnextout > 0.5) inputs.update(kv._1, inc(kv._2, mu))
        else inputs.update(kv._1, inc(kv._2, -mu))
      }
    }
  }
}

class HebbNeuron extends Neuron(x => if (x > 1) 1d else 0d) with HebbLearning