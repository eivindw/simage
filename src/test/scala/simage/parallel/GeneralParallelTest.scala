package simage.parallel

import org.scalatest.Suite

import Operations._

class GeneralParallelTest extends Suite {

   def testDoubleNumbersList {
      val splitList = MyList(List(2, 5, 7, 1))
      val expList = MyList(List(4, 10, 14, 2))

      val doubledList = parallel (
         splitList,
         (i: Int) => {
            Thread.sleep(100) // Slow things down a little
            new MyList(List(i * 2))
         }
      ).asInstanceOf[MyList]

      assert(expList == doubledList)
   }

   case class MyList(val list: List[Int]) extends Splittable[Int] {
      def split = list.toArray

      def merge(other: Splittable[Int]): Splittable[Int] = (this, other) match {
         case (t: MyList, o: MyList) => new MyList(t.list ::: o.list)
      }
   }
}
