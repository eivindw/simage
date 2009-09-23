package simage.morphology

import org.scalatest.Suite
import Morphology._
import structs._
import structs.StrElType._

class PerfTest extends Suite {
   val size = 100000

   val arr2d = new IntArray2DMatrix(1000, (1 to size).toList)
   val mtx = ListMatrix(1000, (1 to size).toList)

   def testArray2DImage {
      val se = StrEl(Square, 3)
      val img = new GrayScaleImage(arr2d)

      erode(img, se)
   }

   def testMatrixImage {
      val se = StrEl(Square, 3)
      val img = Image(mtx)

      erode(img, se)
   }
}
