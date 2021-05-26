def posneg(list: List[Int]): (List[Int], List[Int]) = list match {
    case Nil => (List(), List())
    case a :: b =>
        if (a < 0) (posneg(b)._1, a :: posneg(b)._2)
        else (a :: posneg(b)._1, posneg(b)._2)
}

posneg(List(2, 4, -3, 6, -1, 0))