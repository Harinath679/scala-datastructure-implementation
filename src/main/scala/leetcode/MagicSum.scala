package leetcode

import scala.io.StdIn

object MagicSum extends App {

  def sieveSumSquares(n: Int): Vector[Long] = {
    val arr = Array.fill[Long](n + 1)(0L)

    (1 to n).foreach { d =>
      val sq = d.toLong * d
      (d to n by d).foreach { m =>
        arr(m) += sq
      }
    }

    arr.toVector   // immutable outside
  }

  def isPerfectSquare(x: Long): Boolean = {
    val r = math.sqrt(x).toLong
    r * r == x
  }

  def magicSum(n: Int): Long =
    sieveSumSquares(n)
      .drop(1)
      .filter(isPerfectSquare)
      .sum

  val n = StdIn.readInt()
  println(MagicSum.magicSum(n))

}
