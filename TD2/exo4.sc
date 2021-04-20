def estDans(x: Int, list: List[Int]): Boolean = list match {
    case Nil => false
    case a::b => if (a == x) true else estDans(x, b)
}

def toutDif(list: List[Int]): Boolean = list match {
    case a :: b => if (estDans(a, b)) false else toutDif(b)
    case Nil => true
}

println(toutDif(2 :: 5 :: 3 :: 2 :: Nil))