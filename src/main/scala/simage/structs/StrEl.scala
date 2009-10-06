package simage.structs

trait StrEl[T] {
   val nCols: Int
   val nRows: Int
   val wpad = nCols / 2
   val hpad = nRows / 2
   def apply(col: Int, row: Int): T
}

class Array2DStrEl(cols: Int, els: Array[Int]) extends IntArray2DMatrix(cols, els) with StrEl[Int]

object StrElType extends Enumeration {
   type StrElType = Value
   val VLine, HLine, Square, Diamond, Cross, Circle = Value
}

object Array2DStrEl {
   import StrElType._

   def apply(t: StrElType, num: Int): StrEl[Int] = {
      def ones(n: Int) = Array.make(n, 1)
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
