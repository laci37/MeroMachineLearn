package nga

trait StallCount extends GenerationBase with DetermineBest {
  override def step() = {
    super.step()
    if (best.fitness > _globalBestScore) _globalBestScore = best.fitness
    if (best.fitness > _globalBestScore + minDiff) {
      _stall = 0
    } else _stall += 1
  }
  
  protected var _globalBestScore = best.fitness
  protected var _stall = 0

  /**
   * The minimal difference between the new best and the old best to reset counter
   */
  var minDiff = 1e-6

  def stall(): Int = _stall
}