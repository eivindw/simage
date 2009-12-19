package statistics

class DataSet(private val intArr: Array[Int]) {
   def average = intArr.reduceLeft(_ + _) / intArr.size
}

object DataSet {
   def apply(ints: Int*) = new DataSet(Array(ints: _*))
}