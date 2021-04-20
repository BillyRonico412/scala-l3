def eclateListCouple(list: List[(Int, Int)]): (List[Int], List[Int]) = {

    def getFirstElement(list: List[(Int, Int)]): List[Int] = list match {
        case (a, _) :: c => a :: getFirstElement(c)
        case Nil => Nil
    }

    def getSecondElement(list: List[(Int, Int)]): List[Int] = list match {
        case (_, b) :: c => b :: getFirstElement(c)
        case Nil => Nil
    }

    (getFirstElement(list), getSecondElement(list))

}

val listTest = 11 :: -5 :: -7 :: 20 :: Nil

println(eclateListCouple((2, 5) :: (-3, 4) :: (8, -20) :: Nil))