package simage.structs

import org.scalatest.Suite

class ListMatrixTest extends Suite {

   def testEquality {
      val m1 = ListMatrix(2, List(1, 1, 0, 1))
      val m2 = ListMatrix(2, List(1, 1, 0, 1))
      val m3 = ListMatrix(2, List(1.0, 1.0, 0.0, 1.1))
      val m4 = ListMatrix(2, List(1.0, 1.0, 0.0, 1.1))

      assert(m1 == m2)
      assert(m3 == m4)
   }

   def testSimpleOps {
      val m1 = ListMatrix(2, List(1, 1, 0, 1))

      val add = ListMatrix(2, List(2, 2, 0, 2))
      val sub = ListMatrix(2, List(0, 0, 0, 0))
      val mul = ListMatrix(2, List(1, 2, 0, 1))
      val trans = ListMatrix(2, List(1, 0, 1, 1))

      assert(add == m1 + m1)
      assert(sub == m1 - m1)
      assert(mul == m1 * m1)
      assert(trans == m1.transpose)
   }

   def testGetValues {
      val m = ListMatrix(2, List(1, 2, 3, 4))

      assert(1 == m(0, 0))
      assert(2 == m(1, 0))
      assert(3 == m(0, 1))
      assert(4 == m(1, 1))
   }

   def testConvert {
      import Predef._

      val m1 = ListMatrix(2, List(1, 2, 2, 1))
      val m2 = ListMatrix(2, List(1.0, 2.0, 2.0, 1.0))

      val add = ListMatrix(2, List(2.0, 4.0, 4.0, 2.0))
      val sub = ListMatrix(2, List(0.0, 0.0, 0.0, 0.0))

      assert(m2 == m1.toDoubleMatrix)

      assert(add == m1 + m2)
      assert(add == m2 + m1)
      assert(sub == m1 - m2)
      assert(sub == m2 - m1)
   }

   def testBinary {
      val m1 = ListMatrix(2, List(1, 2, 3, 4))
      val m2 = ListMatrix(2, List(0, 255, 255, 0))

      assert(!m1.isBinary)
      assert(m2.isBinary)
   }
}