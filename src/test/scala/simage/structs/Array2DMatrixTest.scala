package simage.structs

import org.scalatest.Suite

class Array2DMatrixTest extends Suite {
   def testGetValues {
      val m = new IntArray2DMatrix(2, List(1, 2, 3, 4))

      assert(1 == m(0, 0))
      assert(2 == m(1, 0))
      assert(3 == m(0, 1))
      assert(4 == m(1, 1))
   }
}