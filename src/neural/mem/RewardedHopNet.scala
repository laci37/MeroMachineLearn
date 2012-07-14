package neural.mem

trait Reward extends HopNet {

  protected var _expOut: Option[Seq[Boolean]] = None
  var learnWithoutExpectedOut = false

  def expectedOut = _expOut
  def expectedOut_=(value: Seq[Boolean]) = _expOut = Some(value)

  override def reset() = {
    super.reset
    _expOut = None
  }

  override def learn() = {
    if (outputCorrect) heuristicSetMu(0.1d)
    else if (_expOut.isDefined) heuristicSetMu(-0.01d)
    super.learn()
  }

  def outputCorrect(): Boolean = {
    if (_expOut.isDefined) {
      val expOut = _expOut.get
      val vector = for (i <- (0 to size - 1)) yield {
        if (expOut(i)) outputs(i).output == 1d
        else outputs(i).output == 0d
      }
      vector.forall(x => x) //returns true if there are no errors in the output 
    } else learnWithoutExpectedOut
  }
}

class RewardedHopNet(size: Int, initWeight:Double) extends HopNet(size, initWeight) with Reward