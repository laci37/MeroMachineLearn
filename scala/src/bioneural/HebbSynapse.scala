package bioneural
import math._
class HebbSynapse(pre: FNNeuron, post: FNNeuron) extends Synapse(pre, post) {
  g = 0
  var mu = 0.05
  var ming= -0.5
  var maxg= 0.5
  def gd=mu*(
		  	  (maxg-g)*
		  	  sigma(post.v-1)*
		  		(
		  		sigma(post.vd-pre.vd-1.25)+
		  		1-sigma(abs(post.v-pre.v)-0.1)
		  		)
		  	  -
		  	  (-ming+g)*
		  	  (1-sigma(pre.vd+0.5))*
		  	  (1-sigma(abs(post.vd)-0.3))*
		  	  (1-sigma(post.v-1)) 
		  	)
  
  override def timestep(dt: Double) = {
    import math._
    super.timestep(dt)
    g+=dt*gd
  }
}