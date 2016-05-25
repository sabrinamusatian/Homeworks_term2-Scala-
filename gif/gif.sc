package GIFReader

import java.util.{MissingResourceException, IllegalFormatException}
import scodec._
import bits._
import scodec.codecs._
import java.nio.file.{Files, Paths}
import java.io._
import scodec.bits.ByteOrdering.LittleEndian
import scala.NoSuchElementException
import swing.{Panel, MainFrame, SimpleSwingApplication}
import java.awt.{Color, Graphics2D, Dimension}
import java.awt.image.BufferedImage

object Timer {
  def apply(interval: Int, repeats: Boolean = true)(op: => Unit) {
    val timeOut = new javax.swing.AbstractAction() {
      def actionPerformed(e : java.awt.event.ActionEvent) = op
    }
    val t = new javax.swing.Timer(interval, timeOut)
    t.setRepeats(repeats)
    t.start()
  }
}

class DataPanel(data: Array[Array[Color]]) extends Panel {
  override def paintComponent(g: Graphics2D) {
    val width = data.length
    val height = data.map(_.length).max
    val dx = g.getClipBounds.width.toFloat  / width
    val dy = g.getClipBounds.height.toFloat / height
    for {
      x <- 0 until data.length
      y <- 0 until data(x).length
      x1 = (x * dx).toInt
      y1 = (y * dy).toInt
      x2 = ((x + 1) * dx).toInt
      y2 = ((y + 1) * dy).toInt
    } {
      data(x)(y) match {
        case c: Color => g.setColor(c)
        case _ => g.setColor(Color.WHITE)
      }
      g.fillRect(x1, y1, x2 - x1, y2 - y1)
    }
  }
}

object Draw extends SimpleSwingApplication {
  case class gifHeader (width: Int, height: Int, flagForGCT: Boolean, colorResol: Int, flagForSF: Boolean,
                         sizeOfGCT: Int, backgroundColor: Int, ratio: Int)
  case class image (LP: Int, TP: Int, width: Int, height: Int, flagForLCT: Boolean, interlaceFlag: Boolean,
                    sortFlag: Boolean, reservedBits: Int, sizeOfLCT: Int)
  case class animation (disposalMethos: Int, flafForUI: Boolean, flagForTC: Boolean, delay: Int, transColor: Int)
  case class gifFile (header: gifHeader, globColtable: Array[Int], descriptors: List[image],
                       images: List[Array[Int]], animations: List[animation])
                       
  def LZW(minRoot: Int, input: BitVector, CT: List[Int]): Array[Int] = {
    def listofarrays(list: List[Int], accumulator: List[Array[Int]]): List[Array[Int]] = {
      if (list.isEmpty) {
        accumulator.reverse
      }
      else {
        listofarrays(list.tail, Array(list.head) :: accumulator)
      }
    }
    def iterator(sizeOfElem: Int, prefix: Array[Int], in: BitVector, table: List[Array[Int]], out: Array[Int],
             endElem: Int): Array[Int] = {
      val elem = in.take(sizeOfElem).reverseBitOrder.toInt(false, LittleEndian)
      if (elem == endElem) {
        out
      }
      else if (elem < table.length) {
        val newTable = table ::: List((prefix :+ table(elem)(0)))
        val newOut = out ++ table(elem)
        iterator(1 + (Math.log(newTable.length) / Math.log(2)).toInt, table(elem), in.drop(sizeOfElem),
          newTable, newOut, endElem)
      }
      else {
        val newTable = table ::: List((prefix :+ prefix(0)))
        val newOut = out ++ (prefix :+ prefix(0))
        iterator(1 + (Math.log(newTable.length) / Math.log(2)).toInt, prefix :+ prefix(0), in.drop(sizeOfElem),
          newTable, newOut, endElem)
      }
    }
    val clearElem = input.take(minRoot).reverseBitOrder.toInt(false, LittleEndian)
    val firstElem = input.drop(minRoot).take(minRoot).reverseBitOrder.toInt(false, LittleEndian)
    iterator(minRoot, Array(CT(firstElem)), input.drop(2 * minRoot), listofarrays(CT, Nil),
      Array(CT(firstElem)), clearElem + 1)
  }
  def decode(path: String): gifFile = {
    val headerCodec = (uint16L :: uint16L :: bool :: uintL(3) :: bool :: uintL(3) :: uint8L :: uint8L).as[gifHeader]
    val imageCodec = (uint16L :: uint16L :: uint16L :: uint16L :: bool :: bool :: bool :: uint2L :: uintL(3)).as[image]
    val animationCodec = (uintL(3) :: bool :: bool :: uint16L :: uint8L).as[animation]
    def createColorTable(table: Array[Int], size: Int, vector: BitVector): Array[Int] = {
      if (size == 0) {
        table
      }
      else {
        val red = vector.take(8).toInt(false, LittleEndian)
        val green = vector.drop(8).take(8).toInt(false, LittleEndian)
        val blue = vector.drop(16).take(8).toInt(false, LittleEndian)
        val newColor = red * 1000000 + green * 1000 + blue 
        createColorTable(table :+ newColor, size - 1, vector.drop(24))
      }
    }
    def drop(vector: BitVector): BitVector = {
      val size = vector.take(8).toInt(false, LittleEndian)
      val newVector = vector.drop(8)
      if (size == 0) {
        newVector
      }
      else {
        drop(newVector.drop(8 * size))
      }
    }
    def getImage(vec: BitVector, accumulator: BitVector): BitVector = {
      def reversebits(inBl: BitVector, ouBl: BitVector): BitVector = {
        if (inBl.isEmpty) {
          ouBl
        }
        else {
          reversebits(inBl.drop(8), ouBl ++ inBl.take(8).reverseBitOrder)
        }
      }
      val size = vec.take(8).toInt(false, LittleEndian)
      var vector = vec.drop(8)
      if (size == 0) {
        accumulator
      }
      else {
        val block = vector.take(size * 8)
        getImage(vector.drop(size * 8), accumulator ++ reversebits(block, BitVector(Nil)))
      }
    }
    def casesDescriotor(vector: BitVector, giffile: gifFile): gifFile = {
      val extension = vector.take(8).toInt(false, LittleEndian)
      if (extension == 44) {
        val newDescriptorForImage = imageCodec.decode(vector.drop(8)).require.value
        var newLCT: Array[Int] = Array[Int]()
        var newVector = vector.drop(8).drop(9 * 8)
        if (newDescriptorForImage.flagForLCT) {
          newLCT = createColorTable(newLCT, Math.pow(2, newDescriptorForImage.sizeOfLCT + 1).toInt, vector)
          newVector = newVector.drop(Math.pow(2, newDescriptorForImage.sizeOfLCT + 1).toInt * 3 * 8)
        }
        val root = (newVector.take(8).toInt(false, LittleEndian))
        newVector = newVector.drop(8)
        var imageVector = getImage(newVector, BitVector(Nil))
        var CT: Array[Int] = Array[Int]()
        if (newDescriptorForImage.flagForLCT) {
          CT = newLCT
        }
        else {
          CT = giffile.globColtable
        }
        val newImage = LZW(root + 1, imageVector, CT.toList ::: List(1000000000, 2000000000))
        val newGif = new gifFile(giffile.header, giffile.globColtable, newDescriptorForImage ::
          giffile.descriptors, newImage :: giffile.images,
          giffile.animations)
        newVector = drop(newVector)
        casesDescriotor(newVector, newGif)
      }
      else if (extension == 59) {
        val gifOut = new gifFile(giffile.header, giffile.globColtable, giffile.descriptors.reverse,
          giffile.images.reverse, giffile.animations.reverse)
        gifOut
      }
      else if (extension == 33) {
        val label = vector.drop(8).take(8).toInt(false, LittleEndian)
        if (label == 249) {
          val newAnimation = animationCodec.decode(vector.drop(27)).require.value
          val newGif = new gifFile(giffile.header, giffile.globColtable, giffile.descriptors,
            giffile.images, newAnimation :: giffile.animations)
          casesDescriotor(vector.drop(16).drop(6 * 8), newGif)
        }
        else if (label == 254) {
          val newVector = drop(vector.drop(16))
          casesDescriotor(newVector, giffile)
        }
        else if (label == 1) {
          val newVector = drop(vector.drop(16).drop(13 * 8))
          casesDescriotor(newVector, giffile)
        }
        else if (label == 255) {
          val newVector = drop(vector.drop(16))
          casesDescriotor(newVector, giffile)
        }
        else {
          throw new NoSuchElementException()
        }
      }
      else {
        throw new NoSuchElementException()
      }
    }
    val byteArray = Files.readAllBytes(Paths.get(path))
    var bitVector = (BitVector(byteArray)).drop(48)
    val decoded_header = headerCodec.decode(bitVector).require.value
    bitVector = bitVector.drop(7 * 8)
    var GCT: Array[Int] = Array[Int](0)
    if (decoded_header.flagForGCT) {
      GCT = createColorTable(Array[Int](), Math.pow(2, decoded_header.sizeOfGCT + 1).toInt, bitVector)
      bitVector = bitVector.drop(Math.pow(2, decoded_header.sizeOfGCT + 1).toInt * 3 * 8)
    }
    val file = new gifFile(decoded_header, GCT, Nil, Nil, Nil)
    casesDescriotor(bitVector, file)
  }
  def color(col: Int): Color = {
    new Color(col / 1000000, (col / 1000) % 1000, col % 1000)
  }
  def getFrames(ims: List[Array[Int]], dscs: List[image], accumulator: List[Array[Array[Color]]]): List[Array[Array[Color]]] = {
    def frame(back: Array[Array[Color]], im: Array[Int], w: Int, h: Int, desc: image): Array[Array[Color]] = {
      val newFrame = back
      var i = 0
      for {
        x <- 0 until w
        y <- 0 until h
      } {
        if (x >= desc.LP && y >= desc.TP) {
          newFrame(x)(y) = color(im(i))
          i = i + 1
        }
      }
      newFrame
    }
    if(ims.isEmpty) {
      accumulator
    }
    else {
      getFrames(ims.tail, dscs.tail, accumulator :+ frame(Array.fill(gif.header.width,
        gif.header.height)(color(gif.globColtable(gif.header.backgroundColor))), ims.head,
        gif.header.width, gif.header.height, dscs.head))
    }
  }
  val gif: gifFile = decode("file1.gif")
  val scale = 30
  val back = Array.fill(gif.header.width, gif.header.height)(color(gif.globColtable(gif.header.backgroundColor)))
  val frames = getFrames(gif.images, gif.descriptors, Nil).toArray
  val module = frames.length
  var number = 0
  def top = new MainFrame {
    def tick() = {
      var dispMeth = gif.animations(number % module).disposalMethos
      if (dispMeth == 2) {
        contents = new DataPanel(back) {
          preferredSize = new Dimension(gif.header.width * scale, gif.header.height * scale)
        }
      }
      else if (dispMeth == 3) {
        contents = new DataPanel(frames((number - 1) % module)) {
          preferredSize = new Dimension(gif.header.width * scale, gif.header.height * scale)
        }
      }
      number = number + 1
      contents = new DataPanel(frames((number - 1) % module)) {
        preferredSize = new Dimension(gif.header.width * scale, gif.header.height * scale)
      }
    }
    Timer(gif.animations(number % module).delay) {tick()}
  }
}
