package knapsack
import optimization._
object Knapsack {
  def main(args: Array[String]): Unit = {
    val itemnum=50
    val sacksize=10
    val rand= new util.Random(itemnum)
    val items= (for(i<-(1 to itemnum)) 
      yield new Item(rand.nextDouble*10,rand.nextDouble)).toList
    val mainact=new MainActor(sacksize,items)
    mainact.start()
    
  }
}
