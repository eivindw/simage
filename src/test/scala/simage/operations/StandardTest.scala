package simage.operations

import org.scalatest.Suite
import structs.{Image, Matrix, StrEl}
import structs.StrElType._
import io.SImageIO._
import util.Time

class StandardTest extends Suite {
   val img = Image(Matrix(3, List(
      1, 2, 3,
      4, 5, 6,
      7, 8, 9)))
   val se = StrEl(Square, 3)
   val imgExp = Image(Matrix(3, List(
      3, 3, 4,
      4, 5, 5,
      6, 6, 7)))

   def testAvgFilter {
      assert(imgExp == img.avgSimple(se))
   }

   def testDistributedAvg {
      assert(imgExp == img.avg(se))
   }

   def testAvgLoadSave {
      val img = loadImageCP("/numbers.png")
      Time("Regular avg"){
         img.avgSimple(se)
      }
      //saveImage(avg(img, se), "target/cell_avg.jpg")
   }

   def testDistAvgLoadSave {
      val img = loadImageCP("/numbers.png")
      Time("Distributed avg"){
         img.avg(se)
      }
      //saveImage(img.avg(se), "target/cell_avg.jpg")
   }
}
