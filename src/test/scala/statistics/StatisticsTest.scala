package statistics

import org.scalatest.Suite

class StatisticsTest extends Suite {

   def testSimpleAvg {
      val ds = DataSet(1, 2, 3, 4, 5, 6, 7, 8, 9)

      assert(5.0 === ds.average)
      assert(1 === ds.minValue)
      assert(9 === ds.maxValue)
   }

   def testSimpleAvg2 {
      val ds = DataSet(1, 2, 3, 4, 5, 6)

      println("Dice average: " + ds.average)
   }

   def testStringAvg {
      val words = Array("eivind", "test", "oslo", "bekk", "scala", "elephant", "cat")

      implicit def str2DS(str: Array[String]) = {
         DataSet(str.map(_.length): _*)
      }

      assert(4.857142857142857 === words.average)
      assert(3 === words.minValue)
      assert(8 === words.maxValue)
   }
}