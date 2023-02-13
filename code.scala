import cats.data.Writer
type Logged[A] = Writer[List[String], A]
// Methods generally return untransformed stacks:
def parseNumber(str: String): Logged[Option[Int]] = util.Try(str.toInt).toOption match {
  case Some(num) => Writer(List(s"Read $str"), Some(num))
  case None      => Writer(List(s"Failed on $str"), None)
}
// Consumers use monad transformers locally to simplify composition:
def addAll(a: String, b: String, c: String): Logged[Option[Int]] = { import cats.data.OptionT
  val result = for {
    a <- OptionT(parseNumber(a))
    b <- OptionT(parseNumber(b))
    c <- OptionT(parseNumber(c))
  } yield a + b + c
  result.value
}
// This approach doesn't force OptionT on other users' code:
val result1 = addAll("1", "2", "3")
// result1: Logged[Option[Int]] = WriterT(
//   (List("Read 1", "Read 2", "Read 3"), Some(6))
// )
val result2 = addAll("1", "a", "3")
// result2: Logged[Option[Int]] = WriterT(
//   (List("Read 1", "Failed on a"), None)
// )