package collection

object MyListImpl extends App {

  sealed trait MyList[+A] {
    def isEmpty: Boolean
    def head: A
    def tail: MyList[A]

    def map[B](f: A => B): MyList[B]
    def flatMap[B](f: A => MyList[B]): MyList[B]
    def ++[B >: A](other: MyList[B]): MyList[B]
    def filter(f: A => Boolean): MyList[A]
    def foldLeft[B](init: B)(f: (B, A) => B): B
  }

  case object MyNil extends MyList[Nothing] {
    def isEmpty = true
    def head = throw new NoSuchElementException
    def tail = throw new NoSuchElementException

    def map[B](f: Nothing => B) = MyNil
    def flatMap[B](f: Nothing => MyList[B]) = MyNil
    def ++[B >: Nothing](other: MyList[B]) = other
    override def filter(f: Nothing => Boolean): MyList[Nothing] = MyNil

    override def foldLeft[B](init: B)(f: (B, Nothing) => B): B = init
  }

  case class Cons[+A](h: A, t: MyList[A]) extends MyList[A] {
    def isEmpty = false
    def head = h
    def tail = t

    def map[B](f: A => B): MyList[B] =
      Cons(f(h), t.map(f))

    def ++[B >: A](other: MyList[B]): MyList[B] =
      Cons(h, t ++ other)

    def flatMap[B](f: A => MyList[B]): MyList[B] =
      f(h) ++ t.flatMap(f)

    override def filter(f: A => Boolean): MyList[A] = {
      if (f(h)) Cons(h, t.filter(f))
      else t.filter(f)
    }

    override def foldLeft[B](init: B)(f: (B, A) => B): B =
      t.foldLeft(f(init, h))(f)
  }

  object MyList {
    def apply[A](xs: A*): MyList[A] =
      if (xs.isEmpty) MyNil
      else Cons(xs.head, apply(xs.tail: _*))
  }


  val list = MyList(1,2,3,4)

  println(list)

  println(list.map(_ * 10))
  println(list.flatMap(f => MyList(f * 10)))

  println(list.filter(_ % 2 == 0))

  println(list.foldLeft(0)(_ + _))

}
