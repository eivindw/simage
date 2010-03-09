package simage.structs

/**
 * List implementation of the matrix data structure.
 *
 * @author eivindw
 */
abstract class ListMatrix[T](els: List[List[T]]) extends Matrix[T] {

   val elements: List[List[T]] = els

   val nRows = elements.length
   val nCols = if (elements.isEmpty) 0 else elements.head.length

   require(elements.forall(_.length == nCols))

   def apply(col: Int, row: Int): T = elements(row)(col)

   def -(other: Matrix[T]): Matrix[T] = other match {
      case o: ListMatrix[_] => matOp(o, subElems)
   }
/*
   override def toString: String = {
      val rowStrings =
         for (row <- elements)
            yield row.mkString("[", ", ", "]")
      rowStrings.mkString("", "\n", "\n")
   }
*/
   private def matOp(other: ListMatrix[T], op: (T, T) => T): ListMatrix[T] = {
      require((other.nRows == nRows) && (other.nCols == nCols))
      toMatrix(List.map2(elements, other.elements)(List.map2(_, _)(op(_, _))))
   }

   protected val zeroVal: T

   protected def subElems(a: T, b: T): T

   protected def toMatrix(els: List[List[T]]): ListMatrix[T]
}

case class IntListMatrix(els: List[List[Int]]) extends ListMatrix[Int](els) {
   def toMatrix(els: List[List[Int]]) = new IntListMatrix(els)

   def toArray = List.flatten(els).toArray

   def subElems(a: Int, b: Int) = a - b

   val zeroVal = 0
   protected val defFill = 0

   def appendBelow(other: Matrix[Int]) = {
      toMatrix(els ::: other.asInstanceOf[IntListMatrix].els)
   }

   protected def seOp(
      se: StrEl[Int],
      op: (Seq[Int]) => Int,
      fillMissing: Boolean,
      fill: Int,
      region: MatrixWindow) = {
      val w = se.wpad
      val h = se.hpad

      def seValues(row: Int, col: Int) = {
         for {
            x <- -w to w
            cx = col + x
            y <- -h to h
            ry = row + y
            if(cx >= 0 && cx < nCols && ry >= 0 && ry < nRows)
         } yield {
            apply(cx, ry)
         }
      }
      val range = for(i <- 0 until nRows; j <- 0 until nCols) yield {
         op(seValues(i, j))
      }
      ListMatrix(nCols, range.toList)
   }
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
}