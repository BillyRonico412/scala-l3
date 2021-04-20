def aplatir(list: List[Any]): List[Any] = list match {
    case (a: List[Any]):: b => aplatir(a) ::: aplatir(b)
    case a :: b => a :: aplatir(b)
    case Nil => Nil
}

aplatir(List(List(2, 3, 5, List(true, false)), List(2, 5), 'B', 24, 3))