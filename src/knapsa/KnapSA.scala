package knapsa
import sa.State
object KnapSA extends App {
  val itemnum = 50
  val sacksize = 10
  val rand = new util.Random(itemnum)
  val items = (for (i <- (1 to itemnum))
    yield new Item(rand.nextDouble * 10, rand.nextDouble)).toList
  for (i <- (1 to 50)) {
    val opt = new sa.Optimizer(new SqKnapProblem(sacksize, items, 32000d))
    println(opt.optimize().asInstanceOf[State].energy)
  }
}