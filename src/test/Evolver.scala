package test
import scala.actors.Actor
import scala.collection.parallel.mutable._
import neural._
import neural.ga._
import neural.dsl._

object Evolver extends Actor {
  val plans = (64 lin) >> (64 lin) >> (8 act adc) >> (64 lin) >> (64 act { x => if (math.abs(x) < 1) math.abs(x) else 1d })
  var gen: Generation = Generation.zero(plans, 20, MyTest)
  MyTest.bdr = Some(new NRNetworkBuilder(plans))
  override def act(): Unit = {
    var c = 0
    loop {
      printf("Generation %d \n", c)
      val nextgen = gen.generate()
      c += 1
      //tryout("gen" + c.toString + ".jpg")
      receiveWithin(1) {
        case str: String => tryout(str)
        case _ => Unit
      }
      gen = nextgen
    }
  }

  def tryout(filename: String): Unit = {
    import java.awt.image._
    import javax.imageio._
    import java.io.File
    println("Trying to Dump!")
    //load image
    val image: BufferedImage = ImageIO.read(new File("test.jpg"))
    val output: BufferedImage = new BufferedImage(256, 256, BufferedImage.TYPE_INT_RGB)
    //find best network in current generation
    var best = gen.members(0)
    for (g <- gen.members) {
      if (g.fit > best.fit) best = g
    }
    //network to use for compression
    val n = (best.decode(), best.decode(), best.decode())
    for (xchunk <- Range(0, 256, 8)) for (ychunk <- Range(0, 256, 8)) {
      //load inputs
      for (i <- Range(0, 8)) for (j <- Range(0, 8)) {
        n._1.inputs(i * 8 + j).set(((image.getRGB(i + xchunk, j + ychunk) & 0x00ff0000) >> 16) / 255d)
        n._2.inputs(i * 8 + j).set(((image.getRGB(i + xchunk, j + ychunk) & 0x0000ff00) >> 8) / 255d)
        n._3.inputs(i * 8 + j).set((image.getRGB(i + xchunk, j + ychunk) & 0x000000ff) / 255d)
      }

      if (!n._1.calc) return ;
      if (!n._2.calc) return ;
      if (!n._3.calc) return ;
      //write outputs
      for (i <- Range(0, 8)) for (j <- Range(0, 8)) {
        var rgb: Int = 0
        rgb |= (math.round(n._1.outputs(i * 8 + j).output * 255).asInstanceOf[Int] & 0x000000ff) << 16
        rgb |= (math.round(n._2.outputs(i * 8 + j).output * 255).asInstanceOf[Int] & 0x000000ff) << 8
        rgb |= (math.round(n._3.outputs(i * 8 + j).output * 255).asInstanceOf[Int] & 0x000000ff)
        output.setRGB(xchunk + i, ychunk + j, rgb)
      }

    }
    ImageIO.write(output, "JPEG", new File(filename))
    println("Dump OK!")
  }

  def adc(x: Double): Double = {
    val round = math.max(math.min(math.round(x * 16), 16d), -15d)
    round / 16
  }

}