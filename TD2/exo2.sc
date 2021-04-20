def estDans(x: Int, list: List[Int]): Boolean = list match {
    case Nil => false
    case a::b => if (a == x) true else estDans(x, b)
}

val list = 5 :: 3 :: 1 :: 2 :: Nil

println(estDans(7, list))