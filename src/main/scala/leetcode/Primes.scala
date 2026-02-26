package leetcode

import scala.io.StdIn

object Primes extends App {

  val n = StdIn.readInt()

  val isPrime = Array.fill(n + 1)(true)

  if (n >= 0) isPrime(0) = false
  if (n >= 1) isPrime(1) = false

  var p = 2
  while (p * p <= n) {
    var mul = p * p
    while (mul <= n) {
      isPrime(mul) = false
      mul += p
    }
    p += 1
  }
  println(isPrime.toList)


  def primeSieve(n: Int): Vector[Boolean] = {
    val arr = Array.fill(n + 1)(true)
    if (n >= 0) arr(0) = false
    if (n >= 1) arr(1) = false

    (2 to math.sqrt(n).toInt).foreach { p =>
      if (arr(p)) {
        (p * p to n by p).foreach { m =>
          arr(m) = false
        }
      }
    }

    arr.toVector
  }

  def primeSieveRec(n: Int): Set[Int] = {

    @annotation.tailrec
    def loop(p: Int, composites: Set[Int]): Set[Int] =
      if (p * p > n) composites
      else if (composites(p)) loop(p + 1, composites)
      else {
        val newComposites =
          (p * p to n by p).foldLeft(composites)(_ + _)
        loop(p + 1, newComposites)
      }

    val composites = loop(2, Set(0, 1))
    (2 to n).filterNot(composites).toSet
  }

}
