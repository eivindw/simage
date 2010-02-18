package imagestats

import org.scalatest.Suite
import simage.structs.{GrayScaleImage, Image, Matrix}
import statistics.DataSet

class ImageStatisticsTest extends Suite {

   def testStatisticAvgOnImage {
      val img = Image(Matrix(3, List(
         9, 8, 7,
         6, 5, 6,
         7, 8, 9)))

      implicit def img2DS(img: GrayScaleImage) = {
         DataSet(img.data.toArray: _*)
      }

      assert(7 === img.average.round)
      assert(5 === img.minValue)
      assert(9 === img.maxValue)
   }
}