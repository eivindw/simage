package simage.structs

import org.scalatest.Suite

class StrElTest extends Suite {
   def testStrElCreation {
      import StrElType._

      val hse = StrEl(HLine, 3)
      assert(hse.nCols == 3)
      assert(hse.nRows == 1)

      val hse2 = StrEl(HLine, 5)
      assert(hse2.nCols == 5)
      assert(hse2.nRows == 1)

      val vse = StrEl(VLine, 3)
      assert(vse.nCols == 1)
      assert(vse.nRows == 3)

      val sse = StrEl(Square, 3)
      assert(sse.nCols == 3)
      assert(sse.nRows == 3)
   }
}