package simage.structs

import parallel.Splittable

class MatrixWindow(val tl: (Int, Int), val br: (Int, Int)) {
   val nCols = br._1 - tl._1 + 1
   val nRows = br._2 - tl._2 + 1
   val size = nCols * nRows
}

trait Matrix[T] extends Splittable[MatrixWindow] {
   val nRows: Int
   val nCols: Int
   protected val defFill: T

   private lazy val MTX_WIN = new MatrixWindow((0, 0), (nRows - 1, nCols - 1))
   private val SPLIT_THRESHOLD = 100

   def apply(col: Int, row: Int): T

   //def +(other: Matrix[T]): Matrix[T]

   def -(other: Matrix[T]): Matrix[T]

   //def *(other: Matrix[T]): Matrix[T]

   //def transpose: Matrix[T]

   def appendBelow(other: Matrix[T]): Matrix[T]

   def seOp(se: StrEl[Int], op: (Seq[T]) => T): Matrix[T] =
      seOp(se, op, false, defFill, MTX_WIN)

   def seOp(se: StrEl[Int], op: (Seq[T]) => T, region: MatrixWindow): Matrix[T] =
      seOp(se, op, false, defFill, region)

   protected def seOp(
      se: StrEl[Int],
      op: (Seq[T]) => T,
      fillMissing: Boolean,
      fill: T,
      region: MatrixWindow): Matrix[T]

   def merge(other: Splittable[MatrixWindow]): Splittable[MatrixWindow] = (this, other) match {
      case (m1: Matrix[T], m2: Matrix[T]) => m1 appendBelow m2
   }

   def split = {
      nCols * nRows match {
         case x if x < SPLIT_THRESHOLD => Array(MTX_WIN)
         case _ => {
            val splitVals = Array(2, 4, 5, 6, 7, 3, 2)
            val sVal = splitVals.find(nRows % _ == 0)
            val step = nRows / sVal.get

            (for(i <- 0 until nRows by step) yield new MatrixWindow((i, 0), (i + step - 1, nRows - 1))).toArray
         }
      }
   }
}

object Matrix {
   def apply(nCols: Int, els: Array[Int]) = new IntArray2DMatrix(nCols, els)

   def apply(nCols: Int, els: List[Int]) = new IntArray2DMatrix(nCols, els.toArray)
}
