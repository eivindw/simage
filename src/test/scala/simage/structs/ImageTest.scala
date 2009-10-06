package simage.structs

import org.scalatest.Suite

class ImageTest extends Suite {
   private val img = Image(Matrix(3, List(
      1, 2, 3,
      4, 5, 6,
      7, 8, 9)))

   def testGrayScaleImage {
      val m = Matrix(2, Array(128, 0, 255, 128))
      val img = Image(m)
      val img2 = Image(m)

      assert(m.nCols == img.width)
      assert(m.nRows == img.height)
      assert(img == img2)
   }

   def testContains {
      val img = Image(Matrix(2, Array(1, 2, 3, 4)))

      assert(img.hasPoint(0, 0))
      assert(!img.hasPoint(-1, 0))
      assert(!img.hasPoint(0, -1))
      assert(img.hasPoint(1, 1))
      assert(!img.hasPoint(2, 1))
      assert(!img.hasPoint(1, 2))
   }

   def testBasicMathOps {
      val img = Image(Matrix(2, Array(1, 4, 2, 3)))
      val img2 = Image(Matrix(2, Array(1, 3, 2, 2)))
      val imgExp = Image(Matrix(2, Array(0, 1, 0, 1)))

      val result = img - img2

      assert(imgExp == result)
   }
}
