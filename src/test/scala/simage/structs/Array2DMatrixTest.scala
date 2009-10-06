package simage.structs

import org.scalatest.Suite

import StrElType._

class Array2DMatrixTest extends Suite {
   private val mtx = Matrix(3, Array(
      1, 2, 3,
      4, 5, 6,
      7, 8, 9))
   private val mtxExp = Matrix(3, Array(
      3, 3, 4,
      4, 5, 5,
      6, 6, 7))
   private val se = StrEl(Square, 3)

   def testGetValues {
      val m = new IntArray2DMatrix(2, Array(1, 2, 3, 4))

      assert(1 == m(0, 0))
      assert(2 == m(1, 0))
      assert(3 == m(0, 1))
      assert(4 == m(1, 1))
   }

   def testSeOpAvg {
      val result = mtx.seOp(se, (seq) => seq.reduceLeft(_ + _) / seq.size)
      assert(mtxExp == result)
   }

   def testSplitImage {
      val regions = mtx.split

      assert(regions.size == 1)
      assert(regions.first.tl == (0, 0))
      assert(regions.first.br == (2, 2))
   }

   def testPartialSeOpAvg {
      val mtxExp1 = Matrix(3, Array(3, 3, 4))
      val win1 = new MatrixWindow((0, 0), (0, 2))
      val mtxExp2 = Matrix(3, Array(4, 5, 5))
      val win2 = new MatrixWindow((1, 0), (1, 2))
      val mtxExp3 = Matrix(3, Array(6, 6, 7))
      val win3 = new MatrixWindow((2, 0), (2, 2))

      val op = mtx.seOp(se, (seq) => seq.reduceLeft(_ + _) / seq.size, _: MatrixWindow)

      println(op(win1))
      println(op(win2))
      println(op(win3))

      assert(mtxExp1 == op(win1))
      assert(mtxExp2 == op(win2))
      assert(mtxExp3 == op(win3))
   }

   def testAppendPartialImages {
      val mtxExp1 = Matrix(3, Array(3, 3, 4))
      val mtxExp2 = Matrix(3, Array(4, 5, 5))
      val mtxExp3 = Matrix(3, Array(6, 6, 7))

      val result = mtxExp1.merge(mtxExp2).merge(mtxExp3)

      assert(mtxExp == result)
   }
}