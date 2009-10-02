package simage.structs

import org.scalatest.Suite

class PrimitiveOpsTest extends Suite {

   class ArrayMatrix(cols: Int, array: Array[Int]) {
      private val arr = array

      val nRows = arr.size / cols
      val nCols = cols

      def apply(i: Int, j: Int) = arr(i + j * nCols)

      def hasPoint(x: Int, y: Int) = x >= 0 && x < nCols && y >= 0 && y < nRows

      def seValues(se: ArrayMatrix, row: Int, col: Int) = {
         val w = se.nCols / 2
         val h = se.nRows / 2
         val p = row + col * nCols

         Array(
            arr(p - nCols - 1), arr(p - nCols), arr(p - nCols + 1),
            arr(p - 1), arr(p), arr(p + 1),
            arr(p + nCols - 1), arr(p + nCols), arr(p + nCols + 1)
         )
      }

      def seOp(se: ArrayMatrix, op: (Seq[Int]) => Int) = {
         def pointOp(row: Int, col: Int) = {
            val w = se.nCols / 2
            val h = se.nRows / 2
            val tmp = for {
               x <- -w to w
               cx = col + x
               y <- -h to h
               ry = row + y
               if(hasPoint(cx, ry))
            } yield {
               apply(cx, ry) * se(w + x, h + y)
            }
            op(tmp)
         }
         val range = for(i <- 0 until nRows; j <- 0 until nCols) yield {
            pointOp(i, j)
         }
         new ArrayMatrix(nCols, range.toArray)
      }

      override def toString = {
         nRows + "x" + nCols + " matrix"
      }
   }

   val arr = (1 to 1000000).toArray
   val arrMtx = new ArrayMatrix(1000, arr)
   val se = new ArrayMatrix(3, Array(1, 1, 1, 1, 1, 1, 1, 1, 1))

   /*def testAvgOperation {
      val res = arrMtx.seOp(se, (seq) => seq.reduceLeft(_ + _) / seq.size)
      println(res)
   }*/

   def testGetSeValues {
      val mtx = new ArrayMatrix(3, Array(1, 2, 3, 4, 5, 6, 7, 8, 9))
      val e1 = Array(1, 2, 4, 5)
      val e2 = Array(1, 2, 3, 4, 5, 6, 7, 8, 9)
      val e3 = Array(5, 6, 8, 9)

      val v1 = mtx.seValues(se, 0, 0)
      assert(e1 deepEquals v1)
      val v2 = mtx.seValues(se, 1, 1)
      assert(e2 deepEquals v2)
      val v3 = mtx.seValues(se, 2, 2)
      assert(e3 deepEquals v3)
   }
}