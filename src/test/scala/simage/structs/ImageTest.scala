package simage.structs

import org.scalatest.Suite
import StrElType._

class ImageTest extends Suite {
   private val img = Image(Matrix(3, List(
      1, 2, 3,
      4, 5, 6,
      7, 8, 9)))
   private val imgExp = Image(Matrix(3, List(
      3, 3, 4,
      4, 5, 5,
      6, 6, 7)))
   private val se = StrEl(Square, 3)

   def testGrayScaleImage {
      val m = Matrix(2, List(128, 0, 255, 128))
      val img = Image(m)
      val img2 = Image(m)

      assert(m.nCols == img.width)
      assert(m.nRows == img.height)
      assert(img == img2)
   }

   def testContains {
      val img = Image(Matrix(2, List(1, 2, 3, 4)))

      assert(img.hasPoint(0, 0))
      assert(!img.hasPoint(-1, 0))
      assert(!img.hasPoint(0, -1))
      assert(img.hasPoint(1, 1))
      assert(!img.hasPoint(2, 1))
      assert(!img.hasPoint(1, 2))
   }

   def testBasicMathOps {
      val img = Image(Matrix(2, List(1, 4, 2, 3)))
      val img2 = Image(Matrix(2, List(1, 3, 2, 2)))
      val imgExp = Image(Matrix(2, List(0, 1, 0, 1)))

      assert(imgExp == img - img2)
   }

   def testSeOpAvg {
      assert(imgExp == img.seOp(se, (seq) => seq.reduceLeft(_ + _) / seq.size))
   }

   def testSplitImage {
      val regions = img.split

      assert(regions(0).win == ((0, 0), (0, 2)))
      assert(regions(1).win == ((1, 0), (1, 2)))
      assert(regions(2).win == ((2, 0), (2, 2)))
   }

   def testPartialSeOpAvg {
      val imgExp1 = Image(Matrix(3, List(3, 3, 4)))
      val win1 = new ImagePart((0, 0), (0, 2))
      val imgExp2 = Image(Matrix(3, List(4, 5, 5)))
      val win2 = new ImagePart((1, 0), (1, 2))
      val imgExp3 = Image(Matrix(3, List(6, 6, 7)))
      val win3 = new ImagePart((2, 0), (2, 2))

      val op = img.partialSeOp(se, (seq) => seq.reduceLeft(_ + _) / seq.size, _: ImagePart )

      assert(imgExp1 == op(win1))
      assert(imgExp2 == op(win2))
      assert(imgExp3 == op(win3))
   }

   def testAppendPartialImages {
      val imgExp1 = Image(Matrix(3, List(3, 3, 4)))
      val imgExp2 = Image(Matrix(3, List(4, 5, 5)))
      val imgExp3 = Image(Matrix(3, List(6, 6, 7)))

      assert(imgExp == imgExp1.merge(imgExp2).merge(imgExp3))
   }
}
