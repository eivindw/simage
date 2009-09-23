package simage.structs

trait Matrix[T] {
   val nRows: Int
   val nCols: Int

   def apply(col: Int, row: Int): T

   //def +(other: Matrix[T]): Matrix[T]

   def -(other: Matrix[T]): Matrix[T]

   //def *(other: Matrix[T]): Matrix[T]

   //def transpose: Matrix[T]

   def appendBelow(other: Matrix[T]): Matrix[T]
}

object Matrix {
   def apply(nCols: Int, els: List[Int]) = new IntArray2DMatrix(nCols, els)

   def apply(nCols: Int, els: List[Double]) = ListMatrix(nCols, els)
}
