def flip[T](list: List[T]): List[T] = list match {
    case Nil => Nil
    case a :: Nil => list
    case a :: b :: c => b :: a :: flip(c)
}

flip(List(1, 2, 3, 4, 5, 6))

flip(List(1, 2, 3, 4, 5, 6, 7))