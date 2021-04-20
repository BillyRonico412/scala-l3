def len(list: List[Any]): Int =
    list.foldRight(z = 0)(op = (_: Any, b: Int) => 1 + b)

def estPremier(a: Int): Boolean = a match {
    case 1 => true
    case _ => len(List.range(1, a).filter((x: Int) => a % x == 0)) == 1
}

def estPremierBis(a: Int) = a match {
    case 1 => true
    case _ => List.range(2, a).forall((x: Int) => a % x != 0)
}

estPremier(1)

estPremier(5)

estPremier(18)

estPremier(97)

estPremierBis(1)

estPremierBis(5)

estPremierBis(18)

estPremierBis(97)

List.range(1, 1001).filter(estPremierBis)

List.range(1, 1001).filter(estPremier)