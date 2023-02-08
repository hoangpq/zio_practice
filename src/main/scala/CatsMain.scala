import CatsMain.wrapper.UserNotFound
import cats.{Eq, Functor, MonadError, Monoid, Show}
import cats.data.Writer

import java.util.logging.Logger

object CatsMain {

  import cats.Monad
  import cats.data.OptionT
  import cats.instances.option._
  import cats.instances.list._
  import cats.instances.future._

  val monadList = Monad[List]
  val aSimpleList = monadList.pure(1)

  val list: OptionT[List, Int] = OptionT(List(Option(1), Option(2), Option(3)))

  var listOfDouble: OptionT[List, Int] = for {
    num <- list
  } yield num * 2


  import cats.implicits._
  import cats.data.EitherT
  import cats.instances.either._

  // var testVal = EitherT(Future.successful(Right(120)))

  /*val f = for {
    num <- testVal
  } yield num*/

  import cats.data.Reader

  case class Config(number: Int)

  case class Connection(config: Config) {
    def getConfigNumber(): Int = config.number
  }

  val dbReader: Reader[Config, Connection] = Reader(conf => Connection(conf))
  val intReader: Reader[Config, Int] = for {
    _id <- dbReader.map(_.config.number)
  } yield _id + 1

  val aWriter: Writer[List[String], Int] = Writer(List("Hello"), 1)

  def countAndLog(n: Int): Writer[Vector[String], Int] = {
    if (n <= 0) Writer(Vector("starting!"), 0)
    else countAndLog(n - 1).flatMap(_ => Writer(Vector(s"$n"), n))
  }

  def sumWithLogs(n: Int): Writer[Vector[String], Int] = {
    if (n <= 0) Writer(Vector("Done!"), 0)
    else for {
      _ <- Writer(Vector(s"Now at $n"), n)
      lowerSum <- sumWithLogs(n - 1)
    } yield lowerSum + n
  }

  sealed trait Json

  final case class JsObject(get: Map[String, Json]) extends Json

  final case class JsString(get: String) extends Json

  final case class JsNumber(get: Double) extends Json

  final case object JsNull extends Json

  trait JsonWriter[A] {
    def write(value: A): Json
  }

  final case class Person(name: String, email: String)

  object JsonWriterInstance {
    // implicit values goes here
    implicit val StringWriter: JsonWriter[String] =
      new JsonWriter[String] {
        def write(value: String): Json = JsString(value)
      }

    implicit val personWriter: JsonWriter[Person] =
      new JsonWriter[Person] {
        def write(value: Person): Json =
          JsObject(Map(
            "name" -> JsString(value.name),
            "email" -> JsString(value.email)
          ))
      }
  }

  object Json {
    def toJson[T](value: T)(implicit writer: JsonWriter[T]): Json = {
      writer.write(value)
    }
  }


  object JsonSyntax {
    implicit class JsonWriterOps[T](value: T) {
      def toJson(implicit writer: JsonWriter[T]): Json = {
        writer.write(value)
      }
    }

    // implicit vals vs implicit defs
    implicit def optionWriter[A](implicit writer: JsonWriter[A]): JsonWriter[Option[A]] =
      new JsonWriter[Option[A]] {
        override def write(option: Option[A]): Json =
          option match {
            case Some(aValue) => writer.write(aValue)
            case None => JsNull
          }
      }

  }

  import CatsMain.JsonSyntax._
  import CatsMain.JsonWriterInstance._

  import scala.util.Random
  import scala.concurrent.{Future, Await}
  import scala.concurrent.ExecutionContext.Implicits.global
  import scala.concurrent.duration._

  val future2 = {
    val r = new Random(0L)

    for {
      a <- Future(r.nextInt())
      b <- Future(r.nextInt())
    } yield (a, b)
  }

  def myMethod[F[_]] = {
    // val functor = Functor.apply[F]
  }

  type <=[B, A] = A => B

  implicit val stringPrintable: Printable[String] =
    new Printable[String] {
      override def format(value: String): String = s"'${value}'"
    }

  implicit val booleanPrintable: Printable[Boolean] =
    new Printable[Boolean] {
      override def format(value: Boolean): String =
        if (value) "yes" else "no"
    }

  trait Printable[A] {
    self =>
    def format(value: A): String

    def contramap[B](func: B => A): Printable[B] =
      new Printable[B] {
        def format(value: B): String = self.format(func(value))
      }
  }

  def format[A](value: A)(implicit p: Printable[A]): String =
    p.format(value)

  final case class Box[A](value: A)

  /*implicit def boxPrintable[A](implicit p: Printable[A]): Printable[Box[A]] =
    new Printable[Box[A]] {
      def format(box: Box[A]): String = p.format(box.value)
    }*/

  implicit def boxPrintable[A](implicit p: Printable[A]): Printable[Box[A]] =
    p.contramap[Box[A]](_.value)

  import cats.syntax.contravariant._

  val func1 = (x: Int) => x.toDouble

  trait Codec[A] {
    def encode(value: A): String

    def decode(value: String): A
  }

  implicit val symbolMonoid: Monoid[Symbol] =
    Monoid[String].imap(Symbol.apply)(_.name)


  // traits = type classes
  // implicit values = type class instances
  // implicit parameter = type class use; and
  // implicit class: optional utilities that make type classes easier to use

  def parseInt(str: String): Option[Int] =
    scala.util.Try(str.toInt).toOption

  def divide(a: Int, b: Int): Option[Int] =
    if (b == 0) None else Some(a / b)

  def stringDivideBy(aStr: String, bStr: String): Option[Int] =
    parseInt(aStr).flatMap {
      aNum =>
        parseInt(bStr).flatMap {
          bNum => divide(aNum, bNum)
        }
    }

  object wrapper {
    sealed trait LoginError extends Product with Serializable

    final case class PasswordIncorrect(username: String) extends LoginError

    final case class UserNotFound(username: String) extends LoginError

    case object UnexpectedError extends LoginError
  };

  import wrapper._

  case class User(username: String, password: String)

  type LoginResult = Either[LoginError, User]

  def handleError(error: LoginError): Unit =
    error match {
      case UserNotFound(u) =>
        println(s"User not found: $u")
      case PasswordIncorrect(u) =>
        println(s"Password incorrect: $u")
      case UnexpectedError =>
        println(s"Unexpected error")
    }

  def main(args: Array[String]): Unit = {

    println(intReader.run(Config(10)))
    println(aWriter.map(_ + 1))
    // value stay the same, logs change
    println(aWriter.mapWritten(_ :+ "found something interesting!"))
    println(aWriter.bimap(_ :+ "found something interesting!", _ + 1))
    // println("Hello, world!")
    // println(aSimpleList.map(_ + 1))
    // println(listOfDouble.value.reduce(_ |+| _))
    // println(f.value)

    // countAndLog(10).written.foreach(println)
    // println(sumWithLogs(100).written)
    println(Json.toJson("Hello"))
    println(Json.toJson(Person("John", "vampire@gmail.com")))

    println(Person("Vampire", "vampire@gmail.com").toJson)

    println(implicitly[JsonWriter[String]])
    println(Json.toJson(Option("Hello")))

    implicit val dateShow: Show[java.util.Date] = Show.show(date => s"${date.getTime}ms since the epoch.")

    def showVal[A](value: A)(implicit show: Show[A]): String = {
      show.show(value)
    }

    println(showVal(123))
    println(showVal(new java.util.Date()))

    import cats.instances.int._
    val eqInt = Eq[Int]

    println(eqInt.eqv(123, 123))

    val result1 = Await.result(future2, 1.second)
    println(result1)

    // F[_]
    // type constructor, higher kinded types

    // List is a type constructor, takes one parameter
    // List[A] is a type, produced by applying a type parameter
    // Function are "value constructors"

    // math.abs // function, takes one parameter
    // math.abs // value, produced by applying a value parameter

    import cats.instances.list._

    val list1 = List(1, 2, 3)
    val list2 = Functor[List].map(list1)(_ * 2)
    println(list2)
    import cats.instances.option._

    val func = (x: Int) => x + 1
    val liftedFunc = Functor[Option].lift(func)
    println(liftedFunc(Option(1)))
    println(Functor[Option].as(Some(2), 1))

    val func1 = (a: Int) => a + 1
    val result = Functor[Option].lift(func1.map(_ + 1))(Option(1))
    println(result)

    def doMath[F[_]](start: F[Int])(implicit functor: Functor[F]): F[Int] = {
      start.map(n => n + 1 * 2)
    }

    println(doMath(Option(2)))
    println(format("hello"))
    println(format(Box(true)))
    println(func1(20))

    // pure (Haskell) = return (Scala)
    // >>= (Haskell) = flatMap (Scala)
    // Left identity: calling pure and transforming the result
    // with func is the same as just calling func
    // pure(a).flatMap(func) = func(a)
    // Right identity
    // passing pure to flatMap is the same as doing nothing
    // m.flatMap(pure) == m
    // associativity
    // flatMap over two functions f and g is the same as
    // flatMapping over f and then flatMapping over g
    // m.flatMap(f).flatMap(g) == m.flatMap(x => f(x).flatMap(g))

    import cats.Monad

    println(Monad[Option].flatMap(Monad[Option].pure(3))(Some(_)))
    println(Monad[List].flatMap(List(1, 2, 3))(a => List(a, a * 10)))

    import cats.instances.future._
    val fm = Monad[Future]
    val future = fm.flatMap(fm.pure(1))(x => fm.pure(x + 2))
    println(Await.result(future, 1.second))

    println(sq(Option(3), Option(4)))
    println(countPositive(List(1, 2, 3)))
    println(countPositive(List(-1, -2, 3)))

    println(Either.catchOnly[NumberFormatException]("foo".toInt))
    println(Either.catchNonFatal(sys.error("Badness")))
    println(Either.fromTry(scala.util.Try("foo".toInt)))
    println(Either.fromOption[String, Int](None, "Badness"))

    import cats.syntax.either._
    println("Error".asLeft[Int].getOrElse(0))

    // take the last argument as function to check the value
    println((-1).asRight[String].ensure("Must be non-negative!")(_ > 0))

    // recover
    println("error".asLeft[Int].recover {
      case _: String => -1
    })
    println("error".asLeft[Int].recoverWith {
      case _: String => Right(-1)
    })

    // mapping
    println("foo".asLeft[Int].leftMap(_.reverse))
    println((6).asRight[String].bimap(_.reverse, _ * 7))

    println((123).asRight[String])
    println((123).asRight[String].swap)

    // error handling
    val res = for {
      a <- 1.asRight[String]
      b <- 0.asRight[String]
      c <- if (b == 0) "DIVO".asLeft[Int]
      else (a / b).asRight[String]
    } yield c * 100
    println(res)

    val res1: LoginResult = UserNotFound("dave").asLeft
    res1.fold(handleError, println)

    import cats.MonadError
    import cats.instances.either._

    type ErrorOr[A] = Either[String, A]
    val monadError = MonadError[ErrorOr, String]
    val success = monadError.pure(42)
    val failure = monadError.raiseError[Int]("Badness")
    println(success, failure)
    println(monadError.ensure(success)("Number to low!")(_ > 1000))

    import cats.syntax.applicative._
    import cats.syntax.applicativeError._
    import cats.syntax.monadError._

    val success1 = 42.pure[ErrorOr]
    val failure1 = "Badness".raiseError[ErrorOr, Int]
    println(failure1.handleErrorWith {
      case "Badness" => 256.pure[ErrorOr]
      case _ => monadError.raiseError("Something else")
    })

    import scala.util.Try
    import cats.instances.try_._

    val exn: Throwable = new RuntimeException("It's all gone wrong")
    println(exn.raiseError[Try, Int])

    println(validateAdult[Try](18))
    println(validateAdult[ExceptionOr](18))

    // eager, lazy, memoized
    // call-by-value which is eager and memoized
    val x = {
      println("Computing x")
      math.random()
    }

    // re-run on every access
    // call-by-name which is lazy and not memoized
    def y = {
      println("Computing y")
      math.random()
    }

    // call-by-need which is lazy and memoized
    // not compute z until it is used and memoize the result
    lazy val z = {
      println("Computing z")
      math.random()
    }

    println(z)

    import cats.Eval
    val now = Eval.now(math.random() + 1000)
    // println(now.map(_ + 1).value)
    val always = Eval.always(math.random() + 3000)
    println(always)
    val later = Eval.later(math.random() + 2000)
    println(later)

    val greeting = Eval.always {
      println("Step 1");
      "Hello"
    }
      .map { str => println("Step 2"); s"$str world" }
    println(greeting.value)

    // Eval as a Monad
    val ans = for {
      a <- Eval.now {
        println("Calculating A");
        40
      }
      b <- Eval.always {
        println("Calculating B");
        2
      }
    } yield {
      println("Adding A and B")
      a + b
    }
    println(ans.value)
    println(ans.value)

    println(factorial(50).value)
    println(foldRight((1 to 100000).toList, 0L)(_ + _))

    // writer
    import cats.data.Writer
    import cats.instances.vector._
    println(Writer(Vector(
      "It was the best of times",
      "It was the worst of times"
    ), 1859))
    println(Vector(
      "It was the best of times",
      "It was the worst of times"
    ).tell.written)

    type Logged[A] = Writer[Vector[String], A]
    val writer1 = for {
      a <- 10.pure[Logged]
      _ <- Vector("a", "b", "c").tell
      b <- 32.writer(Vector("x", "y", "z"))
    } yield a + b
    println(writer1.written)
    // println(Vector(1).combine(Vector(2, 3)))
    val writer2 = writer1.mapWritten(_.map(_.toUpperCase))
    println(writer2.written)

    val writer3 = writer2.mapBoth { (log, res) =>
      val log2 = log.map(_ + "!")
      val res2 = res * 1000
      (log2, res2)
    }
    println(writer3.run)
    writer1.reset
    println(writer1.run)
    println(writer1.swap.run)

    /*Await.result(Future.sequence(Vector(
      Future(f1(5)),
      Future(f1(5))
    )), 5.second)*/

    import cats.syntax.apply._

    val opt1: Option[Int] = Some(2)
    val opt2: Option[Int] = Some(4)
    val s: Option[Int] = (opt1, opt2, opt2).mapN(_ + _ + _)
    println(s)

    // println(f1WithLogs(5).written)
    // with sequence
    val res_await = Await.result(Future.sequence(
      Vector(
        Future(f1WithLogs(5)),
        Future(f1WithLogs(5))
      )).map(_.map(_.written)), 5.second)
    println(res_await)

    final case class Cat(name: String, favouriteFood: String)
    type CatReader = Reader[Cat, String]

    val fFood: CatReader = for {
      name <- Reader((cat: Cat) => cat.name)
      food <- Reader((cat: Cat) => cat.favouriteFood)
    } yield s"$name loves $food"

    println(fFood(Cat("Garfield", "lasagne")))

    val users = Map(
      1 -> "dade",
      2 -> "kate",
      3 -> "margo"
    )

    val passwords = Map(
      "dade" -> "zerocool",
      "kate" -> "acidburn",
      "margo" -> "secret"
    )

    val db = Db(users, passwords)
    println(checkLogin(2, "zerocool").run(db))

    import cats.data.State
    val a = State[Int, String] { state =>
      (state, s"The state is $state")
    }
    // get the state and the result
    println(a.run(10).value)
    println(a.runS(10).value)
    println(a.runA(10).value)

    val step1 = State[Int, String] { num =>
      val ans = num + 1
      (ans, s"Result of step1: $ans")
    }
    val step2 = State[Int, String] { num =>
      val ans = num * 2
      (ans, s"Result of step2: $ans")
    }
    val both = for {
      a <- step1
      b <- State.modify[Int](_ + 1)
    } yield (a, b)

    println(both.run(20).value)

    import cats.data.State
    import State._

    val program: State[Int, (Int, Int, Int)] = for {
      a <- get[Int]
      _ <- set[Int](a + 1)
      b <- get[Int]
      _ <- modify[Int](_ + 1)
      c <- inspect[Int, Int](_ * 1000)
    } yield (a, b, c)
    println(program.run(1).value)


    /*State[List[Int], Int] { oldStack =>
      val newStack = someTransformation(oldStack)
      val result = someCalculation
      (newStack, result)
    }*/
    // val multiStateProgram = evalAll(List("1", "2", "+", "3", "*"))
    // println(multiStateProgram.runA(Nil).value)

    println(evalInput("10 20 + 3 *"))

  }

  import cats.data.State

  type CalcState[A] = State[List[Int], A]

  def operand(num: Int): CalcState[Int] =
    State[List[Int], Int] { stack =>
      (num :: stack, num)
    }

  def operator(func: (Int, Int) => Int): CalcState[Int] =
    State[List[Int], Int] {
      case b :: a :: tail =>
        val ans = func(a, b)
        (ans :: tail, ans)
      case _ =>
        sys.error("Fail!")
    }

  def evalOne(sym: String): CalcState[Int] =
    sym match {
      case "+" => operator(_ + _)
      case "-" => operator(_ - _)
      case "*" => operator(_ * _)
      case "/" => operator(_ / _)
      case num => operand(num.toInt)
    }

  def evalAll(input: List[String]): CalcState[Int] =
    input.foldLeft(0.pure[CalcState]) { (a, b) =>
      a.flatMap(_ => evalOne(b))
    }

  def evalInput(input: String): Int =
    evalAll(input.split(" ").toList).runA(Nil).value

  final case class Db(usernames: Map[Int, String], passwords: Map[String, String])

  type DBReader[A] = Reader[Db, A]

  def findUserName(userId: Int): DBReader[Option[String]] =
    Reader(db => db.usernames.get(userId))

  def checkPassword(username: String, password: String): DBReader[Boolean] =
    Reader(db => db.passwords.get(username).contains(password))

  def checkLogin(userId: Int, password: String): DBReader[Boolean] =
    for {
      username <- findUserName(userId)
      passwordOk <- username.map {
        username => checkPassword(username, password)
      }.getOrElse {
        false.pure[DBReader]
      }
    } yield passwordOk

  def slowly[A](body: => A) =
    try body finally Thread.sleep(100)

  def f1(n: Int): Int = {
    val ans = slowly(if (n == 0) 1 else n * f1(n - 1))
    println(s"f1($n) = $ans")
    ans
  }

  type Logged[A] = Writer[Vector[String], A]

  def f1WithLogs(n: Int): Logged[Int] =
    for {
      ans <- if (n == 0) {
        1.pure[Logged]
      } else {
        slowly(f1WithLogs(n - 1).map(_ * n))
      }
      _ <- Vector(s"f1($n) = $ans").tell
    } yield ans

  import cats.Eval;

  // factorial without stack overflow
  def factorial(n: BigInt): Eval[BigInt] =
    if (n == 1) {
      Eval.now(n)
    } else {
      Eval.defer(factorial(n - 1).map(_ * n))
    }

  def foldRightEval[A, B](as: List[A], acc: Eval[B])(fn: (A, Eval[B]) => Eval[B]): Eval[B] =
    as match {
      case head :: tail => Eval.defer(fn(head, foldRightEval(tail, acc)(fn)))
      case Nil => acc
    }

  def foldRight[A, B](as: List[A], acc: B)(fn: (A, B) => B): B =
    foldRightEval(as, Eval.now(acc)) { (a, b) =>
      b.map(fn(a, _))
    }.value

  type ExceptionOr[A] = Either[Throwable, A]

  def validateAdult[F[_]](age: Int)(implicit me: MonadError[F, Throwable]): F[Int] =
    if (age >= 18) age.pure[F] else new IllegalArgumentException("Age must be greater than 18").raiseError[F, Int]

  def countPositive(nums: List[Int]): Either[String, Int] =
    nums.foldLeft(0.asRight[String]) { (accumulator, num) =>
      if (num > 0) {
        accumulator.map(_ + 1)
      } else {
        Left("Negative. Stopping")
      }
    }

  def sq[F[_] : Monad](a: F[Int], b: F[Int]): F[Int] =
    a.flatMap(x => b.map(y => x * x + y * y))
}