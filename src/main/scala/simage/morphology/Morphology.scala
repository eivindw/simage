package simage.morphology

import structs.{StrEl, Image}

object Morphology {
   def erode(img: Image, se: StrEl[Int]) = img.seOp(se, (seq) => seq.reduceLeft(_ min _), img.max)

   def dilate(img: Image, se: StrEl[Int]) = img.seOp(se, (seq) => seq.reduceLeft(_ max _), img.min)

   def open(img: Image, se: StrEl[Int]) = dilate(erode(img, se), se)

   def close(img: Image, se: StrEl[Int]) = erode(dilate(img, se), se)

   def topHat(img: Image, se: StrEl[Int]) = img - open(img, se)

   def botHat(img: Image, se: StrEl[Int]) = close(img, se) - img

   // morphGrad = dilate - erode
   // intGrad = img - erode
   // extGrad = dilate - img
   // hitOrMiss
   // watershed
   // skeleton
   // perimeter
}
