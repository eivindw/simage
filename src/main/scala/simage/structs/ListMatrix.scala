package simage.structs

/**
 * Code inspired from:<br/>
 * http://jsmerritt.blogspot.com/2008/07/matrix-multiplication-in-scala.html
 */
abstract class ListMatrix[T](els: List[List[T]]) extends Matrix[T] {
   val elements: List[List[T]] = els

   val nRows = elements.length
   val nCols = if (elements.isEmpty) 0 else elements.head.length

   require(elements.forall(_.length == nCols))

   def apply(col: Int, row: Int): T = elements(row)(col)

   def +(other: ListMatrix[T]): ListMatrix[T] = matOp(other, addElems)

   def -(other: Matrix[T]): Matrix[T] = other match {
      case o: ListMatrix[_] => matOp(o, subElems)
   }

   def *(other: ListMatrix[T]): ListMatrix[T] = {
      require(nCols == other.nRows)
      val t = other.transpose
      toMatrix(
         for (row <- elements) yield {
            for (otherCol <- t.elements) yield dotVectors(row, otherCol)
         }
      )
   }

   def transpose: ListMatrix[T] = toMatrix(List.transpose(elements))

   override def toString: String = {
      val rowStrings =
         for (row <- elements)
            yield row.mkString("[", ", ", "]")
      rowStrings.mkString("", "\n", "\n")
   }

   private def matOp(other: ListMatrix[T], op: (T, T) => T): ListMatrix[T] = {
      require((other.nRows == nRows) && (other.nCols == nCols))
      toMatrix(List.map2(elements, other.elements)(List.map2(_, _)(op(_, _))))
   }

   private def dotVectors(a: List[T], b: List[T]): T = {
      val multipliedElements = List.map2(a, b)(mulElems(_, _))
      (zeroVal /: multipliedElements)(addElems(_, _))
   }

   protected val zeroVal: T

   protected def addElems(a: T, b: T): T

   protected def subElems(a: T, b: T): T

   protected def mulElems(a: T, b: T): T

   protected def toMatrix(els: List[List[T]]): ListMatrix[T]
}

case class IntListMatrix(els: List[List[Int]]) extends ListMatrix[Int](els) {
   def toMatrix(els: List[List[Int]]) = new IntListMatrix(els)

   val zeroVal = 0

   def appendBelow(other: Matrix[Int]) = {
      val lMtx = other.asInstanceOf[ListMatrix[Int]]
      ListMatrix(nCols, List.flatten(elements) ::: List.flatten(lMtx.elements))
   }

   def addElems(a: Int, b: Int) = a + b

   def subElems(a: Int, b: Int) = a - b

   def mulElems(a: Int, b: Int) = a * b

   def toDoubleMatrix = new DoubleListMatrix(elements.map(_.map(_.toDouble)))

   def isBinary = List.flatten(elements).forall(i => i == 0 || i == 255)
}

case class DoubleListMatrix(els: List[List[Double]]) extends ListMatrix[Double](els) {
   def toMatrix(els: List[List[Double]]) = new DoubleListMatrix(els)

   val zeroVal = 0.0

   def appendBelow(other: Matrix[Double]) = {
      val lMtx = other.asInstanceOf[ListMatrix[Double]]
      ListMatrix(nCols, List.flatten(elements) ::: List.flatten(lMtx.elements))
   }

   def addElems(a: Double, b: Double) = a + b

   def subElems(a: Double, b: Double) = a - b

   def mulElems(a: Double, b: Double) = a * b
}

object ListMatrix {
   private def splitRowsWorker[T](in: List[T], work: List[List[T]], nCols: Int): List[List[T]] = {
      if (in.isEmpty)
         work
      else {
         val (a, b) = in.splitAt(nCols)
         splitRowsWorker(b, work ::: List(a), nCols)
      }
   }

   private def splitRows[T](inList: List[T], nCols: Int) = splitRowsWorker(inList, List[List[T]](), nCols)

   def apply(nCols: Int, els: List[Int]) = new IntListMatrix(splitRows(els, nCols))

   def apply(nCols: Int, els: List[Double]) = new DoubleListMatrix(splitRows(els, nCols))
}
