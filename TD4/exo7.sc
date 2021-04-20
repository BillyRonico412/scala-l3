def triRapide(list: List[Int]): List[Int] = list match {
    case Nil => Nil
    case a :: _ =>
        triRapide(list.filter((x: Int) => x < a)) :::
        list.filter((x: Int) => x == a) :::
        triRapide(list.filter((x: Int) => x > a))
}

triRapide(List(2, 9, 7, 8, 7, 9, 2, 9, 12, 4, 1, 12))