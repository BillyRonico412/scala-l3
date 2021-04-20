def estDans2(x: Any, list: List[Any]): Boolean = list match {
    case (a: List[Any]) :: b => if (estDans2(x, a)) true else estDans2(x, b)
    case a :: b => if (a == x) true else estDans2(x, b)
    case Nil => false
}

println(estDans2(true, List(List(2, 3, 5, List(556, false)), List(2, 5), 'B', 24, 3)))