package simage.structs

trait Splittable[T] {
   def split: Array[T]

   def merge(other: Splittable[T]): Splittable[T]
}
