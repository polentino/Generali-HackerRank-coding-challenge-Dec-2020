package it.polentino911.generali.prisonBreak

import java.io.PrintWriter
import scala.io.StdIn

object Result {

  /*
   * Complete the 'prison' function below.
   *
   * The function is expected to return a LONG_INTEGER.
   * The function accepts following parameters:
   *  1. INTEGER n
   *  2. INTEGER m
   *  3. INTEGER_ARRAY h
   *  4. INTEGER_ARRAY v
   */
  def prison(n: Int, m: Int, h: Array[Int], v: Array[Int]): Long = {
    // (1) ensure the bar are sorted
    val hSorted = h.sorted
    val vSorted = v.sorted

    val maxH = maxAdjacent(hSorted)
    val maxV = maxAdjacent(vSorted)

    maxH * maxV
  }


  private def maxAdjacent(array: Array[Int]): Int = {
    array
      // (2) group adjacent bar
      .sliding(2)
      // (3) if distance == 1 store a "1", else "0"
      .map(pair => if(pair.last - pair.head == 1) 1 else 0)
      // (4) create a list that holds the sum of adjacent ones
      .foldLeft(List.empty[Int])(
        (acc, item) => acc match {
          case head :+ tail => if(item == 1) head :+ (tail + item) else head :+ tail :+ item
          case List(elem) => if(item == 1) List(elem + item) else List(elem, item)
          case Nil => List(item)
        }
      )
      // (5) return the max of this list + 2, to get the real dimension
      .max + 2
  }

}

object Solution {
  def main(args: Array[String]) {
    val result = Result.prison(3,2,Array(1,2,3),Array(1,2))
    println(result)
  }
}
