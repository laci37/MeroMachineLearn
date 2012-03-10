package neural.selfbuilding

abstract class Mutagen {
  def apply(g: GrowingGenome): Unit
  def apply(l: GrowingLayer): Unit
  
  /*
   * update for fitness score dependent mutations
   */
  def updateFit(fit: Double): Unit
}