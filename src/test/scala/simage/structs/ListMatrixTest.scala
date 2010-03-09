package simage.structs

import org.scalatest.Suite

import StrElType._

class ListMatrixTest extends Suite {
   private val mtx = ListMatrix(3, List(
      1, 2, 3,
      4, 5, 6,
      7, 8, 9))
   private val mtxExp = ListMatrix(3, List(
      3, 3, 4,
      4, 5, 5,
      6, 6, 7))
   private val se = StrEl(Square, 3)

   def testSeOpAvg {
      val result = mtx.seOp(se, (seq) => seq.reduceLeft(_ + _) / seq.size)
      assert(mtxExp == result)
   }
}