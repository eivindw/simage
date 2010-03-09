package simage.structs

import org.scalatest.Suite
import util.Time
import StrElType._

class MatrixComparisonTest extends Suite {
   private val cols = 1000
   private val basis = List.make(200000, 42)
   private val listMtx = ListMatrix(cols, basis)
   private val arrMtx = Matrix(cols, basis)
   private val se = StrEl(Square, 3)

   def testSePerformance {
      Time("Array Matrix SeOp"){
         val res = arrMtx.seOp(se, (seq) => seq.reduceLeft(_ + _) / seq.size)
         println(res)
      }
      Time("List Matrix SeOp"){
         val res = listMtx.seOp(se, (seq) => seq.reduceLeft(_ + _) / seq.size)
         println(res)
      }
      Time("Array Matrix -"){
         val res = arrMtx - arrMtx
         println(res)
      }
      Time("List Matrix -"){
         val res = listMtx - listMtx
         println(res)
      }
   }
}