package leetcode

object Anagrams extends App {

  val str = scala.io.StdIn.readLine().trim.split(" ", 2)
  val map1 = str(0).groupBy(identity).view.mapValues(_.length).toMap
  val map2 = str(1).groupBy(identity).view.mapValues(_.length).toMap

  println(map1 == map2)

}
