package it.polentino911.generali.officePark

object Result {

  /*
   * Complete the 'findMinDistance' function below.
   *
   * The function is expected to return an INTEGER.
   * The function accepts following parameters:
   *  1. INTEGER w
   *  2. INTEGER h
   *  3. INTEGER n
   */

  def findMinDistance(w: Int, h: Int, n: Int): Int = {
    // bruteforce :/
    def fillArray(n: Int, a: Array[Array[Int]]): List[Array[Array[Int]]] = {
      if (n == 1) {
        val result = for {
          i <- 0 until w
          j <- 0 until h
        } yield {
          val c = a.map(_.clone)
          c(i)(j) = 0
          c
        }
        result.toList
      } else {
        val result = for {
          i <- 0 until w
          j <- 0 until h
        } yield {
          val c = a.map(_.clone)
          c(i)(j) = 0
          fillArray(n - 1, c)
        }
        result.flatten.toList
      }
    }

    def computeDistance(a: Array[Array[Int]]): Int = {
      for {
        i <- 0 until w
        j <- 0 until h
      } yield {
        // set tiles by scanning them as follows
        //          (4)
        //    (3)  (i,j)  (2)
        //          (1)

        // (1)
        a.lift(i).flatMap(_.lift(j + 1)) match {
          case Some(value) if value != -1 && a(i)(j) == -1 => a(i)(j) = value + 1
          case _ =>
        }
        // (2)
        a.lift(i + 1).flatMap(_.lift(j)) match {
          case Some(value) if value != -1 && a(i)(j) == -1 => a(i)(j) = value + 1
          case _ =>
        }
        // (3)
        a.lift(i - 1).flatMap(_.lift(j)) match {
          case Some(value) if value != -1 && a(i)(j) == -1 => a(i)(j) = value + 1
          case _ =>
        }
        // (4)
        a.lift(i).flatMap(_.lift(j - 1)) match {
          case Some(value) if value != -1 && a(i)(j) == -1 => a(i)(j) = value + 1
          case _ =>
        }
        a
      }

      if (a.flatten.contains(-1)) {
        computeDistance(a)
      } else {
        a.flatten.max
      }
    }

    // create initial array
    val a = Array.tabulate(w, h)((x, y) => -1)
    fillArray(n, a)
      .find(_.flatten.count(_ == 0) == n)
      .map(computeDistance)
      .max
  }

}

object Solution {
  def main(args: Array[String]) {
    val result = Result.findMinDistance(4, 4, 3)
    println(result)
  }
}
