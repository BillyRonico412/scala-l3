def len(list: List[Any]): Int =
    list.foldRight(z = 0)(op = (_: Any, b: Int) => 1 + b)

def estPremier(a: Int): Boolean = a match {
    case 1 => true
    case _ => len(List.range(1, a).filter((x: Int) => a % x == 0)) == 1
}

val listNbrePremierInf100 = for (x <- List.range(1, 101); if estPremier(x)) yield x

val listPairPremierInf10 =
    for (x <- List.range(1, 11); y <- List.range(x, 11); if estPremier(x + y)) yield (x, y)
