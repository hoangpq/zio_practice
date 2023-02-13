import cats.Monad
import cats.data.Writer
import cats.implicits._

object MonadTransformer {
  type LoggedT[A] = Writer[List[String], A]

  /*implicit val loggedTMonad: Monad[LoggedT] = new Monad[LoggedT] {
    def pure[A](x: A): LoggedT[A] = Writer(List.empty, x)

    def flatMap[A, B](fa: LoggedT[A])(f: A => LoggedT[B]): LoggedT[B] =
      fa.flatMap(f)

    def tailRecM[A, B](a: A)(f: A => LoggedT[Either[A, B]]): LoggedT[B] =
      f(a).flatMap {
        case Left(nextA) => tailRecM(nextA)(f)
        case Right(b) => pure(b)
      }
  }*/

  def main(args: Array[String]): Unit = {
    // method generally return untransformed type
    val result1 = addAll("1", "2", "3")
    println(result1)
  }

  def parseNumber(str: String): LoggedT[Option[Int]] =
    util.Try(str.toInt).toOption match {
      case Some(num) => Writer(List(s"Read $str"), Some(num))
      case None => Writer(List(s"Failed on $str"), None)
    }

  def addAll(a: String, b: String, c: String): LoggedT[Option[Int]] = {
    import cats.data.OptionT
    val result = for {
      a <- OptionT(parseNumber(a))
      b <- OptionT(parseNumber(b))
      c <- OptionT(parseNumber(c))
    } yield a + b + c
    result.value
  }

}
