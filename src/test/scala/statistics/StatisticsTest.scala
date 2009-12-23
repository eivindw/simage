package statistics

import org.scalatest.Suite

class StatisticsTest extends Suite {

   def testSimpleAvg {
      val ds = DataSet(1, 2, 3, 4, 5, 6, 7, 8, 9)

      assert(5 === ds.average)
   }

   def testStringAvg {
      val words = Array("eivind", "test", "oslo", "bekk", "scala", "elephant", "cat")

      implicit def str2DS(str: Array[String]) = {
         DataSet(str.map(_.length): _*)
      }

      assert(5 === words.average.round)
      assert(3 === words.min)
      assert(8 === words.max)
   }
}