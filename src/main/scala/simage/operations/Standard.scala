package simage.operations

import structs.{Image, ImagePart, StrEl}
import parallel.Operations._

object Standard {
   
   def avgSimple(img: Image, se: StrEl[Int]) = img.seOp(se, (seq) => seq.reduceLeft(_ + _) / seq.size)

   def avg(img: Image, se: StrEl[Int]): Image = {
      val splittable = parallel(img, img.partialSeOp(se, (seq) => seq.reduceLeft(_ + _) / seq.size, _: ImagePart))
      splittable.asInstanceOf[Image]
   }
}
