package taxisga
import neural.dsl._
import neural.ga._
object Init {
  
  def main(args: Array[String]): Unit = {
    val plan= 2 lin
    val l1= 1 lin
    val l2= 2 lin;
    l1 >> plan
    l2 >> plan
    l2 >> l1
    var gen= Generation.zeroAdaptive(plan,20,taxis.TaxisTest)
    while (true){
      gen=gen.generate()
      println(gen.best.fit)
    }
  }

}