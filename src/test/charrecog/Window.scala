package test.charrecog
import java.awt.image.BufferedImage
import java.awt.{ Graphics, Graphics2D }
import javax.swing._
import javax.imageio.ImageIO
import java.io.File
import neural.mem._
import scala.collection.mutable.Map
class Window extends JFrame {
  val toDraw = Map.empty[(Int, Int), BufferedImage]
  this.setSize(300, 300)
  this.show()
  val charset = ImageIO.read(new File("curses_640x300.bmp"))

  def getChar(code: Int) = {
    charset.getSubimage(code % 16 * 8, code / 16 * 12, 8, 12)
  }

  var net = new RewardedHopNet(12 * 8 * 2 + 256, 0d)

  def loadInputs(img: BufferedImage, code: Int) = {
    net.reset
    net.expectedOut = createBooleanSeq(img,code)
    for (i <- (0 to 255)) {
      net.inputs(i) set (if (i == code) 1d else 0d)
    }
    for (i <- (0 to 7)) for (j <- (0 to 11)) {
      val in = (if (img.getRGB(i, j) == 0xFF000000) 0d else 1d)
      net.inputs((i * 12 + j) * 2 + 256) set in
      net.inputs((i * 12 + j) * 2 + 257) set 1 - in
    }
  }

  def noisyLoadInputs(img: BufferedImage, noise: Double): BufferedImage = {
    net.reset
    val rand = scala.util.Random
    val noisyInput = new BufferedImage(8, 12, BufferedImage.TYPE_INT_RGB)
    for (i <- (0 to 255)) net.inputs(i) set 0d
    for (i <- (0 to 7)) for (j <- (0 to 11)) {
      val in = (if (img.getRGB(i, j) == 0xFF000000 ^ rand.nextDouble() < noise) 0d else 1d)
      net.inputs((i * 12 + j) * 2 + 256) set in
      net.inputs((i * 12 + j) * 2 + 257) set 1 - in
      if (in == 0d) noisyInput.setRGB(i, j, 0xFF000000)
      else noisyInput.setRGB(i, j, 0xFFFFFFFF)
    }
    noisyInput
  }

  def drawOutput(): BufferedImage = {
    val img = new BufferedImage(8, 12, BufferedImage.TYPE_INT_RGB)
    for (i <- (0 to 7)) for (j <- (0 to 11))
      img.setRGB(i, j, net.outputs((i * 12 + j) * 2 + 256).toInt * 0xFFFFFFFF)

    img
  }

  def test(code: Int) = {
    val input = getChar(code)
    loadInputs(input, code)
    net.calc(100)
    toDraw.update((20, 50), drawOutput())
    toDraw.update((10, 50), input)
    this.repaint()
  }

  def noisyTest(code: Int, noise: Double) {
    var input = getChar(code)
    input = noisyLoadInputs(input, noise)
    net.calc(100)
    toDraw.update((20, 50), drawOutput())
    toDraw.update((10, 50), input)
    this.repaint()
  }

  def createBooleanSeq(img: BufferedImage, code: Int) = {
    val result = for (i <- (0 to 7)) yield for (j <- (0 to 11)) yield {
      (img.getRGB(i, j) == 0xFFFFFFFF, img.getRGB(i, j) != 0xFFFFFFFF)
    }
    val part1 = for(i<-(0 to 255)) yield code==i
    part1 ++ result.flatten.flatMap(tpl => Seq(tpl._1, tpl._2))
  }

  override def paint(g: Graphics) = {
    val g2d = g.asInstanceOf[Graphics2D]
    g2d.fillRect(0, 0, 100, 100)
    toDraw foreach { elem =>
      g2d.drawImage(elem._2, elem._1._1, elem._1._2, this)
    }
  }

  def train(c: Int) = {
    for (i <- (1 to c)) for (code <- (0 to 255))
      test(code)
  }

}