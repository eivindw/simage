package simage.structs

import org.scalatest.Suite

class PrimitiveOpsTest extends Suite {

   class ArrayMatrix(cols: Int, array: Array[Int]) {
      private val arr = array

      val nRows = arr.size / cols
      val nCols = cols

      def apply(i: Int, j: Int) = arr(i + j * nCols)

      def seOp2(se: ArrayMatrix, op: (Seq[Int]) => Int) = {
         val w = se.nCols / 2
         val h = se.nRows / 2

         def seValues(row: Int, col: Int) = {
            for {
               x <- -w to w
               cx = col + x
               y <- -h to h
               ry = row + y
               if(cx >= 0 && cx < nCols && ry >= 0 && ry < nRows)
            } yield {
               arr(cx + ry * nCols)
            }
         }
         val range = for(i <- 0 until nRows; j <- 0 until nCols) yield {
            op(seValues(i, j))
         }
         new ArrayMatrix(nCols, range.toArray)
      }

      def seOp3(se: ArrayMatrix, op: (Seq[Int]) => Int) = {
         val w = se.nCols / 2
         val h = se.nRows / 2

         val newArr = Array.make(arr.size, 0)
         var points: List[Int] = Nil
         for(j <- 0 until nCols; i <- 0 until nRows) {
            points = Nil
            for {
               x <- -w to w
               cx = i + x
               y <- -h to h
               ry = j + y
               if(cx >= 0 && cx < nCols && ry >= 0 && ry < nRows)
            } {
               points = arr(cx + ry * nCols) :: points
            }
            newArr(i + j * nCols) = op(points)
         }
         new ArrayMatrix(nCols, newArr)
      }

      def seOp(se: ArrayMatrix, op: (Seq[Int]) => Int) = {
         val w = se.nCols / 2
         val h = se.nRows / 2

         val newArr = Array.make(arr.size, 0)
         var points: List[Int] = Nil
         var j, i, x, y, cx, ry = 0
         while(j < nCols) {
            i = 0
            while(i < nRows) {
               points = Nil
               x = -w
               while(x <= w) {
                  cx = i + x
                  y = -h
                  while(y <= h) {
                     ry = j + y
                     if(cx >= 0 && cx < nCols && ry >= 0 && ry < nRows) {
                        points = arr(cx + ry * nCols) :: points
                     }
                     y = y + 1
                  }
                  x = x + 1
               }
               newArr(i + j * nCols) = op(points)
               i = i + 1
            }
            j = j + 1
         }
         new ArrayMatrix(nCols, newArr)
      }

      override def toString = {
         nRows + "x" + nCols + " matrix"
      }
   }

   val arr = (1 to 1000000).toArray
   val arrMtx = new ArrayMatrix(1000, arr)
   val se = new ArrayMatrix(3, Array(1, 1, 1, 1, 1, 1, 1, 1, 1))

   def testAvgOperation {
      val res = arrMtx.seOp(se, (seq) => seq.reduceLeft(_ + _) / seq.size)
      println(res)
   }

   def testAvgOperation2 {
      val res = arrMtx.seOp2(se, (seq) => seq.reduceLeft(_ + _) / seq.size)
      println(res)
   }
}
