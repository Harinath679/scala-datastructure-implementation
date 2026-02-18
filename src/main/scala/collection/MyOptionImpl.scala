package collection

object MyOptionImpl extends App {

  sealed trait MyOption[+A] {
    def isEmpty: Boolean

    def map[B](f: A => B): MyOption[B]

    def flatMap[B](f: A => MyOption[B]): MyOption[B]

    def filter(f: A => Boolean): MyOption[A]
  }

  case object MyNone extends MyOption[Nothing] {
    override def isEmpty: Boolean = true

    override def map[B](f: Nothing => B): MyOption[B] = MyNone

    override def filter(f: Nothing => Boolean): MyOption[Nothing] = MyNone

    override def flatMap[B](f: Nothing => MyOption[B]): MyOption[B] = MyNone
  }

  case class MySome[+A](value: A) extends MyOption[A] {
    override def isEmpty: Boolean = false

    override def map[B](f: A => B): MyOption[B] = MySome(f(value))

    override def filter(f: A => Boolean): MyOption[A] =
      if(f(value)) this else MyNone

    override def flatMap[B](f: A => MyOption[B]): MyOption[B] = f(value)
  }

  val option = MySome(10)
  println(option.isEmpty)
  println(option.filter(_ % 2 == 0))
  println(option.map(_ + 10))
  println(option.flatMap(f => MySome(f + 10)))

  val none: MyOption[Int] = MyNone

  println(none.isEmpty)              // true
  println(none.filter(_ % 2 == 0))   // MyNone
  println(none.map(_ + 10))          // MyNone
  println(none.flatMap(x => MySome(x+10))) // MyNone


}
