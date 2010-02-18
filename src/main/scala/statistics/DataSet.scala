package statistics

class DataSet(private val intArr: Array[Int]) {
   def average = intArr.reduceLeft(_ + _) / intArr.size.toDouble

   def minValue = intArr.reduceLeft(_ min _)

   def maxValue = intArr.reduceLeft(_ max _)
}

object DataSet {
   def apply(ints: Int*) = new DataSet(Array(ints: _*))
}