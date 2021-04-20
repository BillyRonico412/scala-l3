def listePairesTcheater(n: Int): List[(Int, Int)] =
    for (x <- List.range(1, n); y <- List.range(x+1, n)) yield (x, y)

def concat[T](list1: List[T], list2: List[T]): List[T] =
    list1.foldRight(z = list2)(op = (a: T, b: List[T]) => a :: b)

def flat[T](list: List[List[T]]): List[T] =
    list.foldRight(z = List[T]())(op = (a: List[T], b: List[T]) => concat(a, b))

def estPremierBis(a: Int) = a match {
    case 1 => true
    case _ => List.range(2, a).forall((x: Int) => a % x != 0)
}

def listePaires(n: Int): List[(Int, Int)] =
    List.range(1, n).map((x: Int) => (x, n))

def listePair2(n: Int): List[(Int, Int)] =
    flat(List.range(1, n).map((x: Int) => listePaires(x)))

def pairPremier(n: Int) =
    listePair2(n).filter((paire: (Int, Int)) => estPremierBis(paire._1 + paire._2))

pairPremier(10)