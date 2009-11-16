package simage.parallel

import actors.Futures._

object Operations {

   def parallel[T](obj: Splittable[T], op: (T) => Splittable[T]): Splittable[T] = {
      obj.split match {
         case Array(region) => op(region)
         case regions: Array[T] => {
            val futures = for(region <- regions) yield future {
               //println("Thread running.." + Thread.currentThread.getId)
               op(region)
            }
            val results = awaitAll(5000, futures: _*)
            val parts = for(result <- results) yield result.get.asInstanceOf[Splittable[T]]
            parts.reduceLeft(_ merge _)
         }
      }
   }
}