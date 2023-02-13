import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt
// instance of traverse
import cats.instances.future._
import cats.instances.list._
import cats.syntax.traverse._
import cats.instances.int._
import cats.{Monad, Monoid}
import cats.syntax.semigroup._

object FMain {
  def main(args: Array[String]): Unit = {
    val future1 = Future {
      (1 to 100).toList.foldLeft(0)(_ + _)
    }
    println(future1.map(_.toString))
    println(Await.result(future1, 1.second))
    val fut = Future.sequence(List(Future(1), Future(2), Future(3)))
    Await.result(fut, 1.second).foreach(println)
    val fut2 = List(Future(1), Future(2), Future(3)).sequence
    println(Await.result(fut2, 1.second))
    println(Monad[Future].pure(42))
    val fm = Monoid[Future[Int]].combine(Future(1), Future(2))
    println(Await.result(fm, 1.second))
    // available processor
    println(Runtime.getRuntime.availableProcessors())
    println((1 to 10).toList.grouped(3).toList)
    val result: Future[Int] = parallelFoldMap((1 to 1000000).toVector)(identity)
    println(Await.result(result, 1.second))

    import cats.instances.option._
    import cats.syntax.apply._

    println((Option(1), Option(2)).tupled)

    import cats.data.Kleisli
    val step1: Kleisli[List, Int, Int] =
      Kleisli(x => List(x + 1, x - 1))
    val step2: Kleisli[List, Int, Int] =
      Kleisli(x => List(x, -x))
    val step3: Kleisli[List, Int, Int] =
      Kleisli(x => List(x * 2, x / 2))
    val pipeline = step1 andThen step2 andThen step3
    println(pipeline.run(20))
  }

  def parallelFoldMap[A, B: Monoid](values: Vector[A])(func: A => B): Future[B] = {

    import cats.implicits._

    val numCores = Runtime.getRuntime.availableProcessors()
    val groupSize = (1.0 * values.size / numCores).ceil.toInt

    // create one group for each CPU
    val groups: Iterator[Vector[A]] = values.grouped(groupSize)
    val futures: Iterator[Future[B]] =
      groups.map(group => Future(group.foldMap(func)))

    Future.sequence(futures).map { iterable =>
      iterable.foldLeft(Monoid[B].empty)(_ |+| _)
    }


  }
}