def partage[T](list: List[T], critere: (T) => Boolean): (List[T], List[T]) = list match {
    case a :: b => 
        if (critere(a)) (a :: partage(b, critere)._1, partage(b, critere)._2)
        else (partage(b, critere)._1, a :: partage(b, critere)._2)
    case Nil => (Nil, Nil)
}

def partagePairImpair(list: List[Int]): (List[Int], List[Int]) = partage(list, (x: Int) => x % 2 == 0)

def partageHibou(list: List[String]): (List[String], List[String]) = partage(list, (x: String) => x <= "hibou")

println(partage(List(1, 2, 3, 4, 5, 6), (x: Int) => x % 3 == 0))

println(partagePairImpair(List(1, 2, 3, 4, 5, 6)))

println(partageHibou(List("billy", "wendy", "chaggy", "kobby", "brandy")))

