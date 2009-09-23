package simage.structs

class ImagePart(p1: (Int, Int), p2: (Int, Int)) {
   val win = (p1, p2)
}

abstract class Image extends Splittable[ImagePart] {
   type DataType

   val data: DataType
   val width: Int
   val height: Int
   val min: Int
   val max: Int

   protected lazy val imgWin = new ImagePart((0, 0), (height - 1, width - 1))
   protected val SPLIT_THRESHOLD = 100

   def apply(x: Int, y: Int): Int

   def -(other: Image): Image

   //def +(other: Image): Image

   def seOp(se: StrEl[Int], op: (Seq[Int]) => Int, fill: Int): Image =
      seOp(se, op, true, fill, imgWin)

   def seOp(se: StrEl[Int], op: (Seq[Int]) => Int): Image =
      seOp(se, op, false, 0, imgWin)

   def partialSeOp(se: StrEl[Int], op: (Seq[Int]) => Int, fill: Int, region: ImagePart): Image =
      seOp(se, op, true, fill, region)

   def partialSeOp(se: StrEl[Int], op: (Seq[Int]) => Int, region: ImagePart): Image =
      seOp(se, op, false, 0, region)

   protected def seOp(
      se: StrEl[Int],
      op: (Seq[Int]) => Int,
      fillMissing: Boolean,
      fill: Int,
      region: ImagePart): Image

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

   def merge(other: Splittable[ImagePart]): Splittable[ImagePart] = (this, other) match {
      case (GrayScaleImage(d1), GrayScaleImage(d2)) => Image(d1 appendBelow d2)
   }

   def split = {
      width * height match {
         case x if x < SPLIT_THRESHOLD => Array(imgWin)
         case _ => {
            val splitVals = Array(4, 5, 6, 7, 3, 2)
            val sVal = splitVals.find(height % _ == 0)
            val step = height / sVal.get

            (for(i <- 0 until height by step) yield new ImagePart((i, 0), (i + step - 1, height - 1))).toArray
         }
      }
   }

   protected def seOp(
      se: StrEl[Int],
      op: (Seq[Int]) => Int,
      fillMissing: Boolean,
      fill: Int,
      region: ImagePart) = {
      def pointOp(col: Int, row: Int) = {
         val w = se.wpad
         val h = se.hpad
         val tmp = for {
            x <- -w to w
            cx = col + x
            y <- -h to h
            ry = row + y
            if(fillMissing || hasPoint(cx, ry))
         } yield {
            if (fillMissing && !hasPoint(cx, ry)) fill
            else apply(cx, ry) * se(w + x, h + y)
         }
         op(tmp)
      }
      val win = region.win
      val imgVals = for (row <- win._1._1 to win._2._1; col <- win._1._2 to win._2._2) yield {
         pointOp(col, row)
      }
      Image(Matrix(width, imgVals.toList))
   }
}

object Image {
   def apply(d: Matrix[Int]) = new GrayScaleImage(d)
}
