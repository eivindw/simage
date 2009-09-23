package simage.operations

import actors.Futures._
import structs._

object Standard {
   
   def avgSimple(img: Image, se: StrEl[Int]) = img.seOp(se, (seq) => seq.reduceLeft(_ + _) / seq.size)

   def avg(img: Image, se: StrEl[Int]): Image = {
      val splittable = parallel(img, img.partialSeOp(se, (seq) => seq.reduceLeft(_ + _) / seq.size, _: ImagePart))
      splittable.asInstanceOf[Image]
   }

   def parallel[T](obj: Splittable[T], op: (T) => Splittable[T]): Splittable[T] = {
      val regions = obj.split
      regions.size match {
         case 1 => op(regions.first)
         case _ => {
            val futures = for(region <- regions) yield future {
               println("Thread running.." + Thread.currentThread.getId)
               op(region)
            }
            val results = awaitAll(5000, futures: _*)
            val parts = for(result <- results) yield result.get.asInstanceOf[Splittable[T]]
            parts.reduceLeft(_ merge _)
         }
      }
   }
}
