package simage.operations

import org.scalatest.Suite
import structs.StrElType._
import io.SImageIO._
import structs.{GrayScaleImage, Image, Matrix, StrEl}
import util.Time

class StandardTest extends Suite {
   val img = Image(Matrix(3, List(
      1, 2, 3,
      4, 5, 6,
      7, 8, 9)))
   val se: StrEl[Int] = StrEl(Square, 3)
   val imgExp = Image(Matrix(3, List(
      3, 3, 4,
      4, 5, 5,
      6, 6, 7)))
   val file = "/numbers.png"

   def testAvgFilter {
      assert(imgExp == img.avgSimple(se))
   }

   def testDistributedAvg {
      assert(imgExp == img.avg(se))
   }

   def testAvgLoadSave {
      val img = loadImageCP(file)
      Time("Regular avg"){
         println(img.avgSimple(se))
      }
      //saveImage(avg(img, se), "target/cell_avg.jpg")
   }

   def testDistAvgLoadSave {
      val img = loadImageCP(file)
      Time("Distributed avg"){
         println(img.avg(se))
      }
      //saveImage(img.avg(se), "target/cell_avg.jpg")
   }
}
