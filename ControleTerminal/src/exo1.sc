import scala.annotation.tailrec

@tailrec
def rotation[T](n: Int, l: List[T]): List[T] =
    if (n > 0) rotation(n - 1, l.tail ::: (l.head :: Nil))
    else l

rotation(3, List(1, 2, 3, 4, 5, 6, 7))

rotation(6, List("aa", "bb", "cc", "dd"))