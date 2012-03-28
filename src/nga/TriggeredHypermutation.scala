package nga

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