def estDans[T](x: T, list: List[T]): Boolean = list match {
    case a :: b => if (x == a) true else estDans(x, b)
    case Nil => false
}

println(estDans("Billy", List("Tutu", "Billy")))
println(estDans("Billy", List("Tutu", "Wendy", "Regis")))
