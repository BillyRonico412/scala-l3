def pairImpair(list: List[Int]): (List[Int], List[Int]) = {

    def pair(list: List[Int]): List[Int] = list match {
        case a :: b => if (a % 2 == 0) a :: pair(b) else pair(b)
        case Nil => Nil
    }

    def imPair(list: List[Int]): List[Int] = list match {
        case a :: b => if (a % 2 == 1) a :: pair(b) else pair(b)
        case Nil => Nil
    }

    (pair(list), imPair(list))

}

println(pairImpair(0 :: 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: 8 :: 9 :: Nil))