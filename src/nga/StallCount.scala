package nga

trait StallCount extends GenerationBase with DetermineBest {
  override def step() = {
    super.step()
    if (best.fitness > _globalBestScore) {
      _stall = 0
      _globalBestScore = best.fitness
    } else _stall += 1
  }
  protected var _globalBestScore = best.fitness
  protected var _stall = 0
  def stall(): Int = _stall
}