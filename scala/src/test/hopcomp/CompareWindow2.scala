package test.hopcomp

class CompareWindow2 extends CompareWindow {
  override def bioRest() = {
    val defInSave = bio.defaultIn
    bio.defaultIn = restInhibit
    bio.setInputs(true, restPattern)
    sim.simulate(0.05, (restTime / 0.1).toInt)
    for (i ← (1 to 6)) {
      bio.defaultIn -= 0.5
      bio.setInputs(true, restPattern)
      sim.simulate(0.05, (restTime / 0.6).toInt)
    }
    bio.defaultIn = defInSave
  }
}