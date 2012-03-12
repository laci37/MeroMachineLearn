package xornga
import neural.dsl._
import nga._
import neural.nga._
object Init {

  def main(args: Array[String]): Unit = {
    val plan= 2 lin
    val l1= 1 lin
    val l2= 2 lin;
    l1 >> plan
    l2 >> plan
    l2 >> l1
    
  }

}