package test

import neural.ga._
import neural.dsl.NetworkBuilder
import neural.dsl.NRNetworkBuilder
import neural._

object MyTest extends Tester {
  var bdr: Option[NetworkBuilder] = None
  def testonce(g: Genome): Double = {
    import java.awt.image._
    import javax.imageio._
    import java.io.File
    val image: BufferedImage = ImageIO.read(new File("test.jpg"))
    val rand = util.Random
    val x = rand.nextInt(image.getWidth - 8)
    val y = rand.nextInt(image.getHeight - 8)
    val n = if (bdr.isEmpty) g.decode()
    else g.decode(bdr.get)
    for (i <- Range(0, 8)) for (j <- Range(0, 8)) {
      n.inputs(i * 8 + j) set (image.getRGB(x + i, y + j) & 0x000000ff) / 255d
    }
    val pblue = if (n.calc) {
      var sqsumerr = 0d
      for (i <- Range(0, 8)) for (j <- Range(0, 8)) {
        val err = n.outputs(i * 8 + j).output - (image.getRGB(x + i, y + j) & 0x000000ff) / 255d
        sqsumerr += math.pow(err, 2)
      }
      //println(1 / sqsumerr)
      1 / sqsumerr
    } else Double.NaN

    for (i <- Range(0, 8)) for (j <- Range(0, 8)) {
      n.inputs(i * 8 + j) set ((image.getRGB(x + i, y + j) & 0x0000ff00) >> 8) / 255d
    }
    val pgreen = if (n.calc) {
      var sqsumerr = 0d
      for (i <- Range(0, 8)) for (j <- Range(0, 8)) {
        val err = n.outputs(i * 8 + j).output - ((image.getRGB(x + i, y + j) & 0x0000ff00) >> 8) / 255d
        sqsumerr += math.pow(err, 2)
      }
      //println(1 / sqsumerr)
      1 / sqsumerr
    } else Double.NaN
    for (i <- Range(0, 8)) for (j <- Range(0, 8)) {
      n.inputs(i * 8 + j) set ((image.getRGB(x + i, y + j) & 0x00ff0000) >> 16) / 255d
    }
    val pred = if (n.calc) {
      var sqsumerr = 0d
      for (i <- Range(0, 8)) for (j <- Range(0, 8)) {
        val err = n.outputs(i * 8 + j).output - ((image.getRGB(x + i, y + j) & 0x00ff0000) >> 16) / 255d
        sqsumerr += math.pow(err, 2)
      }
      //println(1 / sqsumerr)
      1 / sqsumerr
    } else Double.NaN
    if(java.lang.Double.isNaN(pred+pblue+pgreen)) 0d
    else pred+pblue+pgreen
  }


  def test(g: Genome)={
   val results=for(i<-Range(0,20)) yield testonce(g)
   math.max(0,(results.sum/results.length)/(results.max-results.min+1))
  }
}