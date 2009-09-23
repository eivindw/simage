package simage.structs

abstract class Array2DMatrix[T](cols: Int, els: List[T]) extends Matrix[T] {
   require(els.size % cols == 0)

   private val arr = els.toArray

   val nRows = els.size / cols
   val nCols = cols

   def apply(i: Int, j: Int) = arr(i + j * nCols)

   override def toString: String = "Array2D rows:" + nRows + " cols:" + nCols + "\n" + arr.toString

   def -(other: Matrix[T]): Matrix[T] = (this, other) match {
      case (t: Array2DMatrix[T], o: Array2DMatrix[T]) => {
         val thisList = t.arr.toList
         val otherList = o.arr.toList
         toMatrix(nCols, List.map2(thisList, otherList)(subElems(_, _)))
      }
   }

   def appendBelow(other: Matrix[T]) = {
      val arrMtx = other.asInstanceOf[Array2DMatrix[T]]
      toMatrix(nCols, arr.toList ::: arrMtx.arr.toList)
   }

   protected def subElems(a: T, b: T): T

   protected def toMatrix(cols: Int, els: List[T]): Array2DMatrix[T]
}

case class IntArray2DMatrix(cols: Int, els: List[Int]) extends Array2DMatrix[Int](cols, els) {
   def subElems(a: Int, b: Int) = a - b

   def toMatrix(cols: Int, els: List[Int]) = new IntArray2DMatrix(cols, els)
}
