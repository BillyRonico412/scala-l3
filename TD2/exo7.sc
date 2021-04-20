def tousFois2(list: List[Int]): List[Int] = list match {
    case a :: b => a * 2 :: tousFois2(b)
    case Nil => Nil
}

def tousPlus1(list: List[Int]): List[Int] = list match {
    case a :: b => a + 1 :: tousPlus1(b)
    case Nil => Nil
}

def appliqueATous(f: (Int) => Int): (List[Int]) => List[Int] = {
    def func(list: List[Int]): List[Int] = list match {
        case a :: b => f(a) :: func(b)
        case Nil => Nil
    }
    func
}

def tousAbsolue = appliqueATous((x: Int) => if (x < 0) -1 * x else x)

def tousPositif = appliqueATous((x: Int) => if (x < 0) 0 else x)

val listTest = 11 :: -5 :: -7 :: 20 :: Nil

println(tousFois2(listTest))

println(tousPlus1(listTest))

println(tousAbsolue(listTest))

println(tousPositif(listTest))
