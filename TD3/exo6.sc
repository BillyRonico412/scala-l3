def sauf[T](x: T, list: List[T]): List[T] = list match {
    case Nil => Nil
    case a :: b => 
        if (a == x) sauf(x, b)
        else a :: sauf(x, b)
}

def enleve[T](list1: List[T], list2: List[T]): List[T] = list2 match {
    case Nil => list1
    case a :: b => enleve(sauf(a, list1), b)
}

println(sauf(2, List(1, 2, 3)))
println(enleve(List(1, 2, 3, 4, 5), List(2, 3, 4)))