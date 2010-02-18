package statistics

import org.scalatest.Suite

class StatisticsTest extends Suite {

   def testSimpleAvg {
      val ds = DataSet(1, 2, 3, 4, 5, 6, 7, 8, 9)

      assert(5.0 === ds.average)
      assert(1 === ds.minValue)
      assert(9 === ds.maxValue)
   }

   def testStringAvg {
      val words = Array("eivind", "test", "oslo", "bekk", "scala", "elephant", "cat")

      implicit def str2DS(str: Array[String]) = {
         DataSet(str.map(_.length): _*)
      }

      assert(5 === words.average.round)
      assert(3 === words.minValue)
      assert(8 === words.maxValue)
   }
}