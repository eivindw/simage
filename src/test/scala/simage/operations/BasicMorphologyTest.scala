package simage.operations

import org.scalatest.Suite
import structs.StrElType._
import Morphology._
import io.SImageIO._
import structs.{StrEl, Image, Matrix}

class BasicMorphologyTest extends Suite {
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
      val sse = StrEl(Square, 3)

      assert(imgHse == erode(img, hse))
      assert(imgVse == erode(img, vse))
      assert(imgSse == erode(img, sse))
   }

   def testGrayScale {
      val img = loadImageCP("/small_grad.png")
      val sse = StrEl(Square, 3)

      close(img, sse)
      //saveImage(img - img2, "target/numbers_close_sub.png")
      //saveImage(erode(img, sse), "target/rice_erode.png")
      //saveImage(dilate(img, sse), "target/rice_dilate.png")
      //saveImage(open(img, sse), "target/rice_open.png")
      //saveImage(close(img, sse), "target/numbers_close.png")
      //saveImage(topHat(img, sse), "target/numbers_tophat.png")
      //saveImage(botHat(img, sse), "target/rice_bothat.png")
   }
}