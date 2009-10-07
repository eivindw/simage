package simage.io

import java.awt.image.BufferedImage
import java.awt.image.BufferedImage._
import java.io.File
import javax.imageio.ImageIO

import structs.{Image, Matrix}

object SImageIO {
   def loadImageCP(name: String) = loadImage(ImageIO.read(getClass.getResourceAsStream(name)))

   def loadImageFile(name: String) = loadImage(ImageIO.read(new File(name)))

   def saveImage(img: Image, name: String) {
      val rows = img.height
      val cols = img.width
      val buf = new BufferedImage(cols, rows, TYPE_BYTE_GRAY)
      val db = buf.getRaster.getDataBuffer
      for(row <- 0 until rows; col <- 0 until cols)
         db.setElem(col + row * rows, img(col, row))

      ImageIO.write(buf, "png", new File(name))
   }

   private def loadImage(img: BufferedImage) = {
      val w = img.getWidth
      val db = img.getRaster.getDataBuffer
      val data = for (i <- 0 to db.getSize - 1) yield db.getElem(i)

      Image(Matrix(w, data.toList))
   }
}
