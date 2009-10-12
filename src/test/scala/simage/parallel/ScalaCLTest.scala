package simage.parallel

import org.scalatest.Suite

import scalacl.{Dim, Program}
import scalacl.ScalaCL._

import util.Time

class ScalaCLTest extends Suite {

   val arr = (1 to 1000000).toArray
   val nCols = 1000
   val arrMtx = new ArrayMatrix(nCols, arr)
   val se = new ArrayMatrix(3, Array(1, 1, 1, 1, 1, 1, 1, 1, 1))

   val prog = new SeOpOCL(Dim(arr.size))

   class SeOpOCL(i: Dim) extends Program(i) {
      val iarr = IntsVar
      val iarr2 = IntsVar

      var output = IntsVar

      content = output := iarr + iarr2
   }

   def testAvg = {
      Time("Brute force implementation"){
         val res = arrMtx + arrMtx
         println(res)
      }

      Time("ScalaCL implementation"){
         val res = arrMtx plusCL arrMtx
         println(res)
      }
   }

   class ArrayMatrix(cols: Int, array: Array[Int]) {
      private val arr = array

      val nRows = arr.size / cols
      val nCols = cols

      def apply(i: Int, j: Int) = arr(i + j * nCols)

      def +(other: ArrayMatrix) = {
         val newArr = Array.make(arr.size, 0)
         var i = 0
         while(i < arr.size) {
            newArr(i) = arr(i) + other.arr(i)
            i = i + 1
         }
         newArr
      }

      def plusCL(other: ArrayMatrix) = {
         Time("Copy in-data"){
            prog.iarr.write(arr)
            prog.iarr2.write(other.arr)
         }
         Time("Run program"){
            prog !
         }
         val newArr = Array.make(arr.size, 0)
         Time("Copy out-data"){
            for(i <- 0 until arr.size) {
               newArr(i) = prog.output.get(i)
            }
         }
         new ArrayMatrix(nCols, newArr)
      }

      override def toString = {
         nRows + "x" + nCols + " matrix"
      }
   }
}