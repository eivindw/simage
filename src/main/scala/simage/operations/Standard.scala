package simage.operations

import structs.{GrayScaleImage, StrEl, Image}

trait Standard { this: GrayScaleImage =>
   def avg(se: StrEl[Int]) = {
      doParallelSeOp(data.seOpWin(se, (seq: Seq[Int]) => seq.reduceLeft(_ + _) / seq.size) _)
   }

   def avgSimple(se: StrEl[Int]) = {
      Image(data.seOp(se, (seq) => seq.reduceLeft(_ + _) / seq.size))
   }
}
