def elementDindicePairImpair[T](l: List[T], n: Int = 0): (List[T], List[T]) = l match {
    case Nil => (Nil, Nil)
    case a :: b =>
        if (n % 2 == 0) (a :: elementDindicePairImpair(b, n + 1)._1, elementDindicePairImpair(b, n + 1)._2)
        else (elementDindicePairImpair(b, n + 1)._1, a :: elementDindicePairImpair(b, n + 1)._2)
}

def reverse[T](l: List[T]): List[T] = l match {
    case Nil => Nil
    case a :: b => reverse(b) ::: (a :: Nil)
}

def addextr[T](l1: List[T], l2: List[T]): List[T] =
    reverse(elementDindicePairImpair(l1)._1) ::: l2 ::: elementDindicePairImpair(l1)._2

addextr(List(1), List(9,9,9))

addextr(List(1,2), List(9,9,9))

addextr(List(1,2,3,4,5), List(9,9,9))