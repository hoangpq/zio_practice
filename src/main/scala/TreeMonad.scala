sealed trait Tree[+A]

final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

final case class Leaf[A](value: A) extends Tree[A]


object MonadTree {

  import cats.Monad

  def leaf[A](value: A): Tree[A] = Leaf(value)

  def branch[A](left: Tree[A], right: Tree[A]): Tree[A] =
    Branch(left, right)

  implicit val treeMonad = new Monad[Tree] {
    def pure[A](value: A): Tree[A] = Leaf(value)

    def flatMap[A, B](tree: Tree[A])(f: A => Tree[B]): Tree[B] = {
      tree match {
        case Branch(l, r) =>
          Branch(flatMap(l)(f), flatMap(r)(f))
        case Leaf(value) => f(value)
      }
    }

    def tailRecM[A, B](a: A)(f: A => Tree[Either[A, B]]): Tree[B] = ???

  }
}

object TreeMonad {

  import MonadTree._
  import cats.syntax.functor._ // for map
  import cats.syntax.flatMap._ // for flatMap

  def main(args: Array[String]): Unit = {
    val tree = branch(leaf(1), branch(leaf(2), leaf(3)))
    println(tree.flatMap(x => branch(leaf(x + 1), leaf(x + 1))))

    val newTree = for {
      t1 <- branch(leaf(1), leaf(2))
      t2 <- branch(leaf(200), leaf(4))
      t3 <- branch(leaf(t1 + 2), leaf(t2 + 3))
    } yield t3
    println(newTree)
  }
}