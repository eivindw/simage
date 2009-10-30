package simage.operations

import structs.{GrayScaleImage, StrEl}

trait Morphology { this: GrayScaleImage =>
   def erode(se: StrEl[Int]) = {
      doParallelSeOp(data.seOpWin(se, (seq: Seq[Int]) => seq.reduceLeft(_ min _), max) _)
   }

   def dilate(se: StrEl[Int]) = {
      doParallelSeOp(data.seOpWin(se, (seq: Seq[Int]) => seq.reduceLeft(_ max _), min) _)
   }

   def open(se: StrEl[Int]) = erode(se).dilate(se)

   def close(se: StrEl[Int]) = dilate(se).erode(se)

   def topHat(se: StrEl[Int]) = this - open(se)

   def botHat(se: StrEl[Int]) = close(se) - this

   // morphGrad = dilate - erode
   // intGrad = img - erode
   // extGrad = dilate - img
   // hitOrMiss
   // watershed
   // skeleton
   // perimeter
}
