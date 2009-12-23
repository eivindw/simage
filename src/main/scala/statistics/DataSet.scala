package statistics

class DataSet(private val intArr: Array[Int]) {
   def average = intArr.reduceLeft(_ + _) / intArr.size.toDouble

   def min = intArr.reduceLeft(_ min _)

   def max = intArr.reduceLeft(_ max _)
}

object DataSet {
   def apply(ints: Int*) = new DataSet(Array(ints: _*))
}