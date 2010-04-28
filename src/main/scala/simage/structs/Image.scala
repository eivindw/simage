package simage.structs

import parallel.Operations._

trait Image[T] {
   type DataType

   val data: DataType
   val width: Int
   val height: Int
   val min: T
   val max: T

   def apply(x: Int, y: Int): T

   def -(other: Image[T]): Image[T]

   //def +(other: Image): Image

   def hasPoint(x: Int, y: Int) = x >= 0 && x < width && y >= 0 && y < height

   override def toString = "Image " + width + "x" + height
}

case class GrayScaleImage(d: Matrix[Int]) extends Image[Int] {
   type DataType = Matrix[Int]

   val data = d
   val width = data.nCols
   val height = data.nRows
   val min = 0
   val max = 255

   def apply(x: Int, y: Int) = d(x, y)

   def -(other: Image[Int]) = (this, other) match {
      case (GrayScaleImage(d1), GrayScaleImage(d2)) => Image(d1 - d2)
   }

   /*def +(other: Image) = (this, other) match {
      case (GrayScaleImage(d1), GrayScaleImage(d2)) => Image(d1 + d2)
   }*/

   def doParallelSeOp(op: (MatrixWindow) => Matrix[Int]) = {
      Image(parallel(data, op).asInstanceOf[DataType])
   }
}

case class RGBImage(d: Matrix[(Int, Int, Int)]) extends Image[(Int, Int, Int)] {
   type DataType = Matrix[(Int, Int, Int)]

   val data = d
   val width = data.nCols
   val height = data.nRows
   val min = (0, 0, 0)
   val max = (255, 255, 255)

   def apply(x: Int, y: Int) = d(x, y)

   def -(other: Image[(Int, Int, Int)]) = (this, other) match {
      case (RGBImage(d1), RGBImage(d2)) => Image(d1 - d2)
   }
}

object Image {
   import operations.{Morphology, Standard}

   def apply(d: Matrix[Int]) = {
      new GrayScaleImage(d) with Standard with Morphology
   }

   def apply(d: Matrix[(Int, Int, Int)]) = {
      new RGBImage(d)
   }
}
