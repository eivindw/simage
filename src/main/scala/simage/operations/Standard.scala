package simage.operations

import structs.{GrayScaleImage, MatrixWindow, StrEl, Image}

trait Standard { this: GrayScaleImage =>
   def avg(se: StrEl[Int]) = {
      doParallelSeOp(data.seOp(se, (seq: Seq[Int]) => seq.reduceLeft(_ + _) / seq.size, _: MatrixWindow))
   }

   def avgSimple(se: StrEl[Int]) = {
      Image(data.seOp(se, (seq) => seq.reduceLeft(_ + _) / seq.size))
   }
}
