def appartient[T](x: T, l: List[T]): Boolean =
    !l.forall((a: T) => a != x)

def simplifie[T](l: List[T]): List[T] =
    if (l.isEmpty) Nil
    else
        if (appartient(l.head, l.tail)) simplifie(l.tail)
        else l.head :: simplifie(l.tail)

simplifie(List(2,1,3,4,3,2))

def simplifie2[T](l: List[T]) =
    l.filter((x: T) => l.map((a: T) => a == x).length > 1)
