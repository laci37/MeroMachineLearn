package knapsa
import sa._
class KnapState(val sackSize: Double, val itemSet: List[Item], val items: List[Item]) extends State with Serializable {
  override def energy() = {
    var sum = 0d
    items foreach { i =>
      sum += i.value
    }
    -sum
  }

  override def neighbor() = {
    val rand = util.Random
    val addIndex = rand.nextInt(itemSet.size - items.size)
    def item2Add(curr: Int, dist: Int): Item = {
      if (!items.contains(itemSet(curr))) {
        if (dist == 0) itemSet(curr)
        else item2Add(curr + 1, dist - 1)
      } else item2Add(curr + 1, dist)
    }
    var newitems = item2Add(0, addIndex) :: items
    def checksize(): Unit = {
      var newsize = 0d
      newitems foreach { i =>
        newsize += i.size
      }
      if (newsize > sackSize) {
        var itemtoRm = newitems(0)
        newitems foreach { i =>
          if (i.size < itemtoRm.size) itemtoRm = i
        }
        newitems = newitems diff itemtoRm :: Nil
        checksize
      }
    }
    checksize()
    new KnapState(sackSize, itemSet, newitems)
  }
  override def toString()={
    "["+energy+";"+items+"]"
  }
}

case class Item(val size:Double, val value:Double)