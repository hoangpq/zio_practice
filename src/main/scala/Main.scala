import zio._
import zio.Console.{printLine, readLine}

import java.io.IOException

object Main extends ZIOAppDefault {

  val aFailedZIO = ZIO.fail("Something went wrong")

  // attempt: run an effect that might throw and exception
  val badZIO = ZIO.succeed {
    println("Trying something")
    val string: String = null
    string.length
  }

  // use attempt if you're evee unsure whether your code might throw an exception
  val betterZIO: ZIO[Any, Throwable, Int] = ZIO.attempt {
    println("Trying something")
    val string: String = null
    string.length
  }

  val catchError = betterZIO.catchAll(e => ZIO.attempt(s"Returning a difference value because $e"))

  // catchAll
  // catchSome (pattern matching), orElse

  // Errors, Defects

  val zippedFibers = for {
    fib1 <- ZIO.succeed(42).fork
    fib2 <- ZIO.succeed(100).fork
    fiber = fib1.zip(fib2)
    tuple <- fiber.join
  } yield tuple


  var effects: Seq[ZIO[Any, Nothing, Int]] = (1 to 10).map(i => ZIO.succeed(i))
  var sumPar = ZIO.reduceAllPar(ZIO.succeed(0), effects)(_ + _)

  var printParallel = ZIO.foreachPar((1 to 10).toList)(i => ZIO.succeed(println(i)))

  def run = printParallel
}