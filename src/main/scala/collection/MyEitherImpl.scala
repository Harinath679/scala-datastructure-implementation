package collection

object MyEitherImpl extends App {

  sealed trait MyEither[+L, +R] {
    self =>

    def isRight: Boolean

    def isLeft: Boolean = !isRight

    def map[B](f: R => B): MyEither[L, B] =
      this match {
        case MyRight(value) => MyRight(f(value))
        case MyLeft(err) => MyLeft(err)
      }

    def flatMap[LL >: L, B](f: R => MyEither[LL, B]): MyEither[LL, B] =
      this match {
        case MyRight(value) => f(value)
        case MyLeft(err) => MyLeft(err)
      }

    def fold[B](leftF: L => B, rightF: R => B): B =
      this match {
        case MyLeft(err) => leftF(err)
        case MyRight(value) => rightF(value)
      }

    def getOrElse[RR >: R](default: => RR): RR =
      this match {
        case MyRight(value) => value
        case MyLeft(_) => default
      }

    def orElse[LL >: L, RR >: R](other: => MyEither[LL, RR]): MyEither[LL, RR] =
      this match {
        case MyRight(_) => this
        case MyLeft(_) => other
      }
  }

  final case class MyLeft[+L](value: L) extends MyEither[L, Nothing] {
    override def isRight: Boolean = false
  }

  final case class MyRight[+R](value: R) extends MyEither[Nothing, R] {
    override def isRight: Boolean = true
  }

}
