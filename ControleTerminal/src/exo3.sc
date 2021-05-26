def appartient[T](x: T, l: List[T]): Boolean =
    !l.forall((a: T) => a != x)

appartient(2, List(3,4,5,8,5,3,0))

appartient("toto", List("titi", "toto", "tata"))