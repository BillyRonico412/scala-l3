def saufPair(list: List[Int]): List[Int] = list match {
    case a :: b => if (a % 2 == 0) a :: saufPair(b) else saufPair(b)
    case Nil => Nil
}

println(saufPair(0 :: 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: 8 :: 9 :: Nil))