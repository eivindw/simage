package simage.structs

trait StrEl[T] {
   val nCols: Int
   val nRows: Int
   val wpad = nCols / 2
   val hpad = nRows / 2
   def apply(col: Int, row: Int): T
}

class ListMatrixStrEl(m: IntListMatrix) extends IntListMatrix(m.elements) with StrEl[Int]

class Array2DStrEl(cols: Int, els: List[Int]) extends IntArray2DMatrix(cols, els) with StrEl[Int]

object StrElType extends Enumeration {
   type StrElType = Value
   val VLine, HLine, Square, Diamond, Cross, Circle = Value
}

object ListMatrixStrEl {
   import StrElType._

   def apply(t: StrElType, num: Int): StrEl[Int] = {
      def ones(n: Int) = (for(i <- 1 to n) yield 1).toList
      t match {
         case HLine => new ListMatrixStrEl(ListMatrix(num, ones(num)))
         case VLine => new ListMatrixStrEl(ListMatrix(1, ones(num)))
         case Square => new ListMatrixStrEl(ListMatrix(num, ones(num * num)))
      }
   }
}

object Array2DStrEl {
   import StrElType._

   def apply(t: StrElType, num: Int): StrEl[Int] = {
      def ones(n: Int) = (for(i <- 1 to n) yield 1).toList
      t match {
         case HLine => new Array2DStrEl(num, ones(num))
         case VLine => new Array2DStrEl(1, ones(num))
         case Square => new Array2DStrEl(num, ones(num * num))
      }
   }
}

object StrEl {
   import StrElType._
   def apply(t: StrElType, num: Int) = Array2DStrEl(t, num)
}
