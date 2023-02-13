import cats.{Applicative, Foldable, Monoid, Semigroupal, Traverse}
import cats.instances.int._
import cats.instances.list._
import cats.instances.invariant._
import cats.instances.future._
import cats.instances.option._
import cats.instances.string._
import cats.instances.either._
import cats.instances.vector._
import cats.instances.lazyList._ // for lazylist

import cats.syntax.apply._ // for tupled
import cats.syntax.applicative._
import cats.syntax.traverse._
import cats.syntax.either._
import cats.syntax.parallel._ // for parTupled
// import cats.implicits._

import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt

object ApplicativeM {

  def parseInt(str: String): Either[String, Int] = {
    Either.catchOnly[NumberFormatException](str.toInt).leftMap(_ => s"Couldn't read $str")
  }

  final case class Cat(name: String, yearOfBirth: Int, favoriteFoods: List[String])

  val tupleToCat: (String, Int, List[String]) => Cat = Cat.apply
  val catToTuple: Cat => (String, Int, List[String]) =
    cat => (cat.name, cat.yearOfBirth, cat.favoriteFoods)

  implicit val catMonoid: Monoid[Cat] = (
    Monoid[String],
    Monoid[Int],
    Monoid[List[String]]
  ).imapN(tupleToCat)(catToTuple)

  def main(args: Array[String]): Unit = {
    println(for {
      a <- parseInt("a")
      b <- parseInt("b")
      c <- parseInt("c")
    } yield a + b + c)

    println(Semigroupal[Option].product(Some(123), None))
    println(Semigroupal.tuple3(None, None, Some(123)))
    println(Semigroupal.map3(Option(1), Option(2), Option(3))(_ + _ + _))
    println(Semigroupal.map2(Option(1), Option.empty[Int])(_ + _))

    println((Option(123), Option("abc")).tupled)

    val add: (Int, Int) => Int = (a, b) => a + b
    println((Option(1), Option(2)).mapN(add))

    // with difference type
    val futurePair = Semigroupal[Future].product(Future("Hello"), Future(123))
    println(Await.result(futurePair, 1.second))

    // zip fixed numbers of Futures
    val futureCat = (
      Future("Garfield"),
      Future(1978),
      Future(List("Lasagne"))
    ).mapN(Cat.apply)
    println(Await.result(futureCat, 1.second))

    // combining Lists with Semigroupal produces some potentially unexpected results
    // we might expect code like the following to zip the list
    // but we actually get the cartesian product of their elements
    println(Semigroupal[List].product(List(1, 2), List(3, 4)))

    type ErrorOr[A] = Either[Vector[String], A]
    println(Semigroupal[ErrorOr].product(
      Left(Vector("Error 1")),
      Left(Vector("Error 2"))
    ))

    val error1: ErrorOr[Int] = Left(Vector("Error 1"))
    val error2: ErrorOr[Int] = Left(Vector("Error 2"))

    println(Semigroupal[ErrorOr].product(error1, error2))
    println((error1, error2).tupled)
    println((error1, error2).parTupled)

    type ErrorOrList[A] = Either[Vector[String], A]
    val errStr1: ErrorOrList[String] = Left(Vector("Error 1"))
    val errStr2: ErrorOrList[String] = Left(Vector("Error 2"))
    println((errStr1, errStr2).parTupled)

    val success1: ErrorOr[Int] = Right(1)
    val success2: ErrorOr[Int] = Right(2)
    val addTwo = (x: Int, y: Int) => x + y
    println((error1, error2).parMapN(addTwo))
    println((success1, success2).parMapN(addTwo))

    println(optionToList(Some(1)))
    println(optionToList(None))

    // parallel list
    println((List(1, 2), List(3, 4)).tupled)
    show(Nil)
    show(List(1, 2, 3))

    def minus(a: Int, b: Int): Int = {
      println(a, b)
      return a - b
    }

    println(List(1, 2, 3).sum)
    println(List(1, 2, 3).foldRight(0)(minus))

    val ints = List(1, 2, 3)
    println(Foldable[List].foldLeft(ints, 0)(_ + _))

    val maybeInt = Option(123)
    println(Foldable[Option].foldLeft(maybeInt, 10)(_ * _))

    def bigData = (1 to 1000000).to(LazyList)
    // println(bigData.foldRight(0L)(_ + _))

    // import cats.instances.lazyList._
    import cats.Eval

    val eval: Eval[Long] = Foldable[LazyList].
      foldRight(bigData, Eval.now(0L)) { (num, eval) =>
        eval.map(_ + num)
      }

    // println(eval.value)
    println(Foldable[Option].nonEmpty(Option(42)))

    // combineAll: combines all elements in the sequence using their Monoid

    val ints_0 = List(Option(1), Option(2))
    println(Foldable[List].combineAll(ints_0))
    // println(Foldable[List].foldMap(List(1, 2, 3))(_.toString))
    // import cats.instances.vector._
    val ints_ = List(Vector(1, 2, 3), Vector(4, 5, 6))

    // composing Foldables to support deep traversal of nested sequences
    (Foldable[List] compose Foldable[Vector]).combineAll(ints_)

    println((Option(List(1, 2, 3)), Option(1)).mapN(_ :+ _))
    println(List(1, 2, 3) :+ 1)

    println(listTraverse(List(1, 2, 3)) { x => Option(x + 1) })
    println(process(List(2, 4, 6)))

    println(List(processErrors(List(1, 2, 3))))

    val total: Future[List[Int]] = Traverse[List].traverse(List(1, 100, 3))(sample)
    println(Await.result(total, 1.second).sum)

    val numbers = List(Future(1), Future(2), Future(3))
    // println(Await.result(numbers.sequence, 1.second))



  }

  def show[A](list: List[A]): String =
    list.foldLeft("")((acc, a) => s"$acc, $a")

  def listTraverse[F[_] : Applicative, A, B](list: List[A])(func: A => F[B]): F[List[B]] =
    list.foldLeft(List.empty[B].pure[F]) { (accum, item) =>
      (accum, func(item)).mapN(_ :+ _)
    }

  import cats.instances.option._

  def process(inputs: List[Int]) =
    listTraverse(inputs)(n => if (n % 2 == 0) Some(n) else None)

  import cats.data.Validated
  import cats.instances.list._

  type ErrorsOr[A] = Validated[List[String], A]

  def processErrors(inputs: List[Int]): ErrorsOr[List[Int]] =
    listTraverse(inputs) { n =>
      if (n % 2 == 0) Validated.valid(n)
      else Validated.invalid(List(s"$n is not even"))
    }

  def sample(number: Int): Future[Int] = Future(number)

  import cats.arrow.FunctionK

  object optionToList extends FunctionK[Option, List] {
    def apply[A](fa: Option[A]): List[A] =
      fa match {
        case None => List.empty[A]
        case Some(a) => List(a)
      }
  }
}