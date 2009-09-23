package simage

import structs.IntListMatrix

object Predef {
   implicit def intToDoubleMatrix(m: IntListMatrix) = m.toDoubleMatrix
}
 