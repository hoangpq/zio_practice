import cats.Monad

import cats.syntax.flatMap._
import scala.annotation.tailrec

object CustomMonad {
  var optionMonad = new Monad[Option] {
    def pure[A](opt: A): Option[A] = Some(opt)

    def flatMap[A, B](opt: Option[A])(fn: A => Option[B]): Option[B] =
      opt flatMap fn

    @tailrec
    override def tailRecM[A, B](a: A)(fn: A => Option[Either[A, B]]): Option[B] =
      fn(a) match {
        case None => None
        case Some(Left(a1)) => tailRecM(a1)(fn)
        case Some(Right(b)) => Some(b)
      }
  }
}

object CatsMonad {
  def retry_[F[_] : Monad, A](start: A)(f: A => F[A]): F[A] =
    f(start).flatMap { a =>
      retry_(a)(f)
    }

  import cats.syntax.functor._
  def retryTailRecM[F[_]: Monad, A](start: A)(f: A => F[A]): F[A] =
    Monad[F].tailRecM(start){ a =>
      f(a).map(a2 => Left(a2))
    }

  def main(args: Array[String]): Unit = {
    import cats.instances.option._

    println(retryTailRecM(100000)(a => if (a == 0) None else Some(a - 1)))
  }
}