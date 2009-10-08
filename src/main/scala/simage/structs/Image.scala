package simage.structs

import parallel.Operations._

trait Image {
   type DataType

   val data: DataType
   val width: Int
   val height: Int
   val min: Int
   val max: Int

   def apply(x: Int, y: Int): Int

   def -(other: Image): Image

   //def +(other: Image): Image

   def hasPoint(x: Int, y: Int) = x >= 0 && x < width && y >= 0 && y < height

   override def toString = "Image " + width + "x" + height
}

case class GrayScaleImage(d: Matrix[Int]) extends Image {
   type DataType = Matrix[Int]

   val data = d
   val width = data.nCols
   val height = data.nRows
   val min = 0
   val max = 255

   def apply(x: Int, y: Int) = d(x, y)

   def -(other: Image) = (this, other) match {
      case (GrayScaleImage(d1), GrayScaleImage(d2)) => Image(d1 - d2)
   }

   /*def +(other: Image) = (this, other) match {
      case (GrayScaleImage(d1), GrayScaleImage(d2)) => Image(d1 + d2)
   }*/

   def doParallelSeOp(op: (MatrixWindow) => Matrix[Int]) = {
      Image(parallel(data, op).asInstanceOf[DataType])
   }
}

object Image {
   import operations.{Morphology, Standard}

   def apply(d: Matrix[Int]) = {
      new GrayScaleImage(d) with Standard with Morphology
   }
}
