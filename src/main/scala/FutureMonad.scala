import jdk.javadoc.doclet.Reporter

import scala.concurrent.Await
import scala.concurrent.duration.DurationInt

object FutureMonad {

  import cats.data.EitherT
  import scala.concurrent.Future
  import scala.concurrent.ExecutionContext.Implicits.global
  import cats.implicits._

  val powerLevel = Map(
    "Jazz" -> 6,
    "Bumblebee" -> 8,
    "Hot Rod" -> 10
  )

  type Response[A] = EitherT[Future, String, A]

  def getPowerLevel(ally: String): Response[Int] = {
    powerLevel.get(ally) match {
      case Some(power) => EitherT.right(Future(power))
      case None => EitherT.left(Future(s"$ally unreachable"))
    }
  }

  def canSpecialMove(ally1: String, ally2: String): Response[Boolean] = {
    for {
      power1 <- getPowerLevel(ally1)
      power2 <- getPowerLevel(ally2)
    } yield (power1 + power2) > 15
  }

  def tacticalReport(ally1: String, ally2: String): String = {
    val result = canSpecialMove(ally1, ally2).value
    Await.result(result, 1.second) match {
      case Left(msg) => s"Comms error: $msg"
      case Right(true) => s"$ally1 and $ally2 are ready to roll out!"
      case Right(false) => s"$ally1 and $ally2 need a recharge."
    }
  }

  def main(args: Array[String]): Unit = {
    println(tacticalReport("Hot Rod", "Bumblebee"))
  }
}