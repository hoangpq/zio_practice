import cats.syntax.functor._

sealed trait Tree[+A]

final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

final case class Leaf[A](value: A) extends Tree[A]


object MonadTree {

  import cats.Monad
  import scala.annotation.tailrec

  def leaf[A](value: A): Tree[A] = Leaf(value)

  def branch[A](left: Tree[A], right: Tree[A]): Tree[A] =
    Branch(left, right)

  implicit val treeMonad = new Monad[Tree] {
    def pure[A](value: A): Tree[A] = Leaf(value)

    def flatMap[A, B](tree: Tree[A])
                     (f: A => Tree[B]): Tree[B] = {
      tree match {
        case Branch(l, r) =>
          Branch(flatMap(l)(f), flatMap(r)(f))
        case Leaf(value) =>
          f(value)
      }
    }

    def tailRecM[A, B](arg: A)
                      (f: A => Tree[Either[A, B]]): Tree[B] = {
      @tailrec
      def loop(
                open: List[Tree[Either[A, B]]],
                closed: List[Option[Tree[B]]]): List[Tree[B]] =
        open match {
          case Branch(l, r) :: next =>
            loop(l :: r :: next, None :: closed)

          case Leaf(Left(value)) :: next =>
            loop(f(value) :: next, closed)

          case Leaf(Right(value)) :: next =>
            loop(next, Some(pure(value)) :: closed)

          case Nil =>
            closed.foldLeft(Nil: List[Tree[B]]) { (acc, maybeTree) =>
              maybeTree.map(_ :: acc).getOrElse {
                val left :: right :: tail = acc
                branch(left, right) :: tail
              }
            }
        }

      loop(List(f(arg)), Nil).head
    }
  }

}

object TreeMonad {

  import MonadTree._
  import cats.syntax.flatMap._ // for flatMap

  def main(args: Array[String]): Unit = {
    val tree = branch(leaf(1), branch(leaf(2), leaf(3)))
    println(tree.map(_ + 1))

    val newTree = for {
      t1 <- branch(leaf(1), leaf(2))
      t2 <- branch(leaf(200), leaf(4))
      t3 <- branch(leaf(t1 + 2), leaf(t2 + 3))
    } yield t3
    println(newTree)
  }
}