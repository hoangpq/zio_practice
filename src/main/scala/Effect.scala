import cats.Parallel
import cats.effect.{IO, IOApp, ParallelF}
import cats.instances.either._

object Effect extends IOApp.Simple {

  import cats.ApplicativeError

  type ErrorOr[A] = Either[String, A]
  val appErrorEither = ApplicativeError[ErrorOr, String]
  val desirableValue = appErrorEither.pure(42)
  val failedValue: ErrorOr[Int] = appErrorEither.raiseError("Something failed")

  import cats.syntax.applicativeError._ // for syntax

  val failedValue_v2: ErrorOr[Int] = "Something failed".raiseError[ErrorOr, Int]

  case class MyIO[A](unsafeRun: () => A) {
    def map[B](f: A => B): MyIO[B] = MyIO(() => f(unsafeRun()))

    def flatMap[B](f: A => MyIO[B]): MyIO[B] = MyIO(() => f(unsafeRun()).unsafeRun())
  }

  val anIO: MyIO[Int] = MyIO(() => {
    println("I'm writing something...")
    42
  })

  val clock: MyIO[Long] = MyIO(() => System.currentTimeMillis())

  def measure[A](computation: MyIO[A]): MyIO[Long] = for {
    start <- clock
    _ <- computation
    end <- clock
  } yield end - start

  def testTimeIO(): Unit = {
    val test = measure(MyIO(() => Thread.sleep(1000)))
    println(test.unsafeRun())
  }

  def putStrLn(line: String): MyIO[Unit] = MyIO(() => println(line))

  def read: MyIO[String] = MyIO(() => scala.io.StdIn.readLine())

  def testConsole(): Unit = {
    val program = for {
      _ <- putStrLn("What's your name?")
      name <- read
      _ <- putStrLn(s"Hello, $name")
    } yield ()
    program.unsafeRun()
  }

  val aDelayIO: IO[Int] = IO.delay {
    println("I'm writing something...")
    42
  }

  def fibonacci(n: Int): IO[BigInt] =
    if (n < 2) IO(1)
    else for {
      last <- IO.defer(fibonacci(n - 1)) // same as .delay(...).flatten
      prev <- IO.defer(fibonacci(n - 2))
    } yield last + prev

  import tuts.utils._
  import cats.syntax.apply._

  val meaningOfLife: IO[Int] = IO.delay(42)
  val favLang: IO[String] = IO.delay("JS")
  val goalInLife = (meaningOfLife.debug_cats, favLang.debug_cats).mapN((num, string) => s"my goal in life is $num and $string")

  // parallelism on IOs
  val parIO1: IO.Par[Int] = Parallel[IO].parallel(meaningOfLife.debug_cats)
  val parIO2: IO.Par[String] = Parallel[IO].parallel(favLang.debug_cats)

  import cats.effect.implicits._

  val goalInLifeParallel: IO.Par[String] = (parIO1, parIO2).mapN((num, string) => s"my goal in life is $num and $string")
  val goalInLife_v2: IO[String] = Parallel[IO].sequential(goalInLifeParallel)

  // shorthand for parallel sequential

  import cats.syntax.parallel._

  val goalInLife_v3: IO[String] = (meaningOfLife.debug_cats, favLang.debug_cats).parMapN((num, string) => s"my goal in life is $num and $string")

  override def run: IO[Unit] = goalInLife_v3.map(println)
}