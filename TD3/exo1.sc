
def eclateListCouple[A, B] (list: List[(A, B)]): (List[A], List[B]) = {

    def getFirstElement(list: List[(A, A)]): List[A] = list match {
        case (a, _) :: c => a :: getFirstElement(c)
        case Nil => Nil
    }

    def getSecondElement(list: List[(A, A)]): List[A] = list match {
        case (_, b) :: c => b :: getFirstElement(c)
        case Nil => Nil
    }

    (getFirstElement(list), getSecondElement(list))

}

println(eclateListCouple((2, 5) :: (-3, 4) :: (8, -20) :: Nil))