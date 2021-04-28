def saufDernier(list: List[Int]): List[Int] = list match {
    case _ :: Nil => Nil
    case a :: b => a :: saufDernier(b)
    case Nil => Nil
}

saufDernier(List(1, 2, 3, 4))