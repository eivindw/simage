package simage.operations

import org.scalatest.Suite
import structs.StrElType._
import io.SImageIO._
import structs.{StrEl, Image, Matrix}

class BasicMorphologyTest extends Suite {
   private val sse = StrEl(Square, 3)

   def testBinaryErosion {
      val img = Image(Matrix(5, List(
         0, 0, 0, 0, 0,
         0, 1, 1, 1, 0,
         0, 1, 1, 1, 0,
         0, 1, 1, 1, 0,
         0, 0, 0, 0, 0)))
      val imgHse = Image(Matrix(5, List(
         0, 0, 0, 0, 0,
         0, 0, 1, 0, 0,
         0, 0, 1, 0, 0,
         0, 0, 1, 0, 0,
         0, 0, 0, 0, 0)))
      val imgVse = Image(Matrix(5, List(
         0, 0, 0, 0, 0,
         0, 0, 0, 0, 0,
         0, 1, 1, 1, 0,
         0, 0, 0, 0, 0,
         0, 0, 0, 0, 0)))
      val imgSse = Image(Matrix(5, List(
         0, 0, 0, 0, 0,
         0, 0, 0, 0, 0,
         0, 0, 1, 0, 0,
         0, 0, 0, 0, 0,
         0, 0, 0, 0, 0)))
      val hse = StrEl(HLine, 3)
      val vse = StrEl(VLine, 3)

      assert(imgHse == img.erode(hse))
      assert(imgVse == img.erode(vse))
      assert(imgSse == img.erode(sse))
   }

   def testGrayScale {
      val base = "rice"
      val img = loadImageCP("/" + base + ".gif")

      saveImage(img.erode(sse), "target/" + base + "_erode.png")
      saveImage(img.dilate(sse), "target/" + base + "_dilate.png")
      saveImage(img.open(sse), "target/" + base + "_open.png")
      saveImage(img.close(sse), "target/" + base + "_close.png")
      saveImage(img.topHat(sse), "target/" + base + "_tophat.png")
      saveImage(img.botHat(sse), "target/" + base + "_bothat.png")
   }
}