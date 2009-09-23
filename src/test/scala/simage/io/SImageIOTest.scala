package simage.io

import org.scalatest.Suite
import SImageIO._
import structs.{Image, Matrix}

class SImageIOTest extends Suite {

   def testLoadClassPath {
      val m = loadImageCP("/small_grad.png")

      assert(10 == m.width)
      assert(10 == m.height)
   }

   def testSave {
      val m = Image(Matrix(10, List(
         0, 24, 50, 76, 102, 127, 153, 179, 205, 231,
         0, 24, 50, 76, 101, 127, 153, 179, 205, 231,
         0, 24, 50, 76, 102, 127, 153, 179, 205, 230,
         0, 24, 50, 75, 102, 127, 153, 179, 205, 231,
         0, 24, 50, 76, 102, 127, 153, 179, 205, 231,
         0, 24, 50, 75, 102, 128, 154, 179, 205, 231,
         0, 24, 50, 76, 102, 127, 153, 179, 205, 231,
         0, 24, 50, 76, 102, 127, 153, 179, 205, 231,
         0, 24, 50, 76, 102, 127, 154, 179, 205, 231,
         0, 24, 50, 76, 102, 127, 153, 180, 205, 231)))

      saveImage(m, "target/grad_test.png")
   }

   def testSaveLoad {
      val m = Image(Matrix(3, List(0, 128, 0, 128, 255, 128, 0, 128, 0)))
      saveImage(m, "target/test.png")
      val m2 = loadImage("target/test.png")
      
      assert(m == m2)
   }
}
