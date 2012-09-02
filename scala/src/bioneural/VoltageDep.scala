package bioneural

trait VoltageDep extends Synapse{
  override def timestep(dt:Double)={
    super.timestep(dt)
    nextI *= 1-sigma(post.v-1)
  }
}

class VDSynapse(pre:FNNeuron,post:FNNeuron) extends Synapse(pre,post) with VoltageDep
class VDHebbSynapse(pre:FNNeuron,post:FNNeuron) extends HebbSynapse(pre,post) with VoltageDep