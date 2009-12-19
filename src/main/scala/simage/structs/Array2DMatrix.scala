package simage.structs

abstract class Array2DMatrix[T](cols: Int, els: Array[T]) extends Matrix[T] {
   require(els.size % cols == 0)

   protected val arr = els

   val nRows = els.size / cols
   val nCols = cols

   def apply(i: Int, j: Int) = arr(i + j * nCols)

   override def toString: String = "Array2D rows:" + nRows + " cols:" + nCols + "\n" + arr.toString

   def -(other: Matrix[T]): Matrix[T] = (this, other) match {
      case (t: Array2DMatrix[_], o: Array2DMatrix[_]) => {
         val thisList = t.arr.toList
         val otherList = o.arr.toList
         toMatrix(nCols, (List.map2(thisList, otherList)(subElems(_, _))).toArray)
      }
   }

   def appendBelow(other: Matrix[T]) = {
      val arrMtx = other.asInstanceOf[Array2DMatrix[T]]
      toMatrix(nCols, arr ++ arrMtx.arr)
   }

   protected def subElems(a: T, b: T): T

   protected def toMatrix(cols: Int, els: Array[T]): Array2DMatrix[T]

   def toArray = arr
}

case class IntArray2DMatrix(cols: Int, els: Array[Int]) extends Array2DMatrix[Int](cols, els) {
   protected val defFill = 0

   def subElems(a: Int, b: Int) = a - b

   def toMatrix(cols: Int, els: Array[Int]) = new IntArray2DMatrix(cols, els)

   def seOp(
      se: StrEl[Int],
      op: (Seq[Int]) => Int,
      fillMissing: Boolean,
      fill: Int,
      region: MatrixWindow) = {
      val w = se.wpad
      val h = se.hpad

      val newArr = Array.make(region.size, 0)
      var points: List[Int] = Nil
      var i, x, y, cx, ry, ii, jj = 0
      var j = region.tl._1

      while(j <= region.br._1) {
         i = region.tl._2
         ii = 0
         while(i <= region.br._2) {
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
            newArr(ii + jj * nCols) = op(points)
            i = i + 1
            ii = ii + 1
         }
         j = j + 1
         jj = jj + 1
      }
      IntArray2DMatrix(nCols, newArr)
   }

   override def equals(other: Any) = other match {
      case that: IntArray2DMatrix =>
         nCols == that.nCols &&
         (arr deepEquals that.arr)
      case _ => false
   }
}
