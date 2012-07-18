package nga
/**
 * extension of GenerationBase, if the evolution stalls long enough, 
 * the mutation rates will be increased. All Genomes used must be the subclass of Hypermutable!!!
 */
trait TriggeredHypermutation extends GenerationBase with StallCount {
  var trigStall = 50

  override def step() = {
    if (stall >= trigStall) {
      _members foreach { m =>
        val hm = m.asInstanceOf[Hypermutable]
        hm.hypermutate_=(true)
      }
    }
    super.step()
  }
  override def toString()= super.toString + " with TriggeredHypermutation"
}