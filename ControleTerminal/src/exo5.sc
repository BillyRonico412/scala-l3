def quiSontDans[T](l1: List[T], l2: List[T]): List[T] =
    l1.filter((x: T) => !l2.forall((a: T) => a != x))

quiSontDans( List(1,5,2,8,3,2,7,8,1), List(9,3,5,11,20,4,8) )
quiSontDans( List(1,2,3), List(10,20,30,40) )