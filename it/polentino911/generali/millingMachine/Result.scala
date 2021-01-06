package it.polentino911.generali.millingMachine

import java.io.PrintWriter
import scala.io.StdIn

object Result {

  /*
   * Complete the 'toolchanger' function below.
   *
   * The function is expected to return an INTEGER.
   * The function accepts following parameters:
   *  1. STRING_ARRAY tools
   *  2. INTEGER k
   *  3. STRING q
   */

  def toolchanger(tools: Array[String], k: Int, q: String): Int = {
    // (1) rearrange the tools such that the array starts with the tool at index k
    val rearrangedTools = k match {
      case 0 => tools // no need to shuffle
      case _ =>
        val head = tools.drop(k)
        val tail = tools.take(k)
        head ++ tail
    }

    // (2) split the tools in half; no reason to continue search past halfway
    val (toolsHead, toolsTail) = rearrangedTools.splitAt(math.ceil(rearrangedTools.length / 2.0).toInt)
    val headIndex = toolsHead.indexOf(q)
    val tailIndex = toolsTail.length - toolsTail.lastIndexOf(q)

    // (3) ensure we handle -1 cases
    if(headIndex == -1) {
      return tailIndex
    } else if (tailIndex == -1) {
      return headIndex
    }

    // (4) otherwise, return the min of the two indices
    math.min(headIndex, tailIndex)
  }
}

object Solution {
  def main(args: Array[String]) {
    val printWriter = new PrintWriter(sys.env("OUTPUT_PATH"))

    val toolsCount = StdIn.readLine.trim.toInt

    val tools = Array.ofDim[String](toolsCount)

    for (i <- 0 until toolsCount) {
      val toolsItem = StdIn.readLine
      tools(i) = toolsItem
    }

    val k = StdIn.readLine.trim.toInt

    val q = StdIn.readLine

    val result = Result.toolchanger(tools, k, q)

    printWriter.println(result)

    printWriter.close()
  }
}
