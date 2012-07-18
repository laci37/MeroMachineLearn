package test
object Main {
  val rand = new scala.util.Random
  def main(args: Array[String]): Unit = {
    //test1()
    Evolver.start()
    while (true) Evolver ! Console.readLine()
  }

  def test1() = {
    import neural._
    import neural.ga._
    import neural.dsl._
    val plans = ((64 lin) d "input") >> ((64 lin) d "encode") >> ((8 lin) d "bottleneck") >> ((64 lin) d "decode") >> ((64 lin) d "output")
    val g = Genome.random(plans)
    println(MyTest.test(g))
    println(MyTest.test(g))
  }
}
