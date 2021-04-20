def concat(list1: List[Int], list2: List[Int]): List[Int] = list1 match {
        case Nil => list2
        case a :: b => a :: concat(b, list2)
    }


println(concat(1 :: Nil, 2 :: 5 :: Nil))