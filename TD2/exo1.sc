def saufDernier(list: List[Int]): List[Int] = list match{
    case _ :: Nil => Nil
    case a :: b => a :: saufDernier(b)
    case Nil => Nil
}

val list = 2 :: 5 :: 3 :: Nil

saufDernier(list)