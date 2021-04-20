def estDans[T](x: T, list: List[T]): Boolean = list match {
    case Nil => false
    case a::b => if (a == x) true else estDans(x, b)
}

def estEnsemble[T](list: List[T]): Boolean = list match {
    case a :: b => !estDans(a, b) && estEnsemble(b)
    case Nil => true
}


def union[T](ensemble1: List[T], ensemble2: List[T]): List[T] = ensemble1 match {
    case Nil => ensemble2
    case a :: b => 
        if (estDans(a, ensemble2)) union(b, ensemble2)
        else a :: union(b, ensemble2)
}

def inter[T](ensemble1: List[T], ensemble2: List[T]): List[T] = ensemble1 match {
    case Nil => Nil
    case a :: b => 
        if (estDans(a, ensemble2)) a :: inter(b, ensemble2)
        else inter(b, ensemble2)
}

println(estEnsemble(List(1, 2, 3, 4, 3)))

println(union(List(1, 2, 3), List(3, 4, 5, 6)))

println(inter(List(1, 2, 3), List(3, 4, 5, 6)))
