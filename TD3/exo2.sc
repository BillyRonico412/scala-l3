def inverse[T](list1: List[T], list2: List[T]): List[(T, T)] = list1 match {
    case a :: b => list2 match {
        case c :: d => (a, c) :: inverse(b, d)
        case Nil => Nil
    }
    case Nil => Nil
}

println(inverse(2 :: 3 :: 4 :: Nil, 5 :: 6 :: 7 :: 8 :: 9 :: Nil))