def toutVrai[T](list: List[T], test: T => Boolean): Boolean =
    list.foldRight(z  = true)(op = (a: T, b: Boolean) => test(a) && b)

toutVrai(List(2, 4, 24, -32), (x: Int) => x % 2 == 0)

toutVrai(List(2, 4, 25, -32), (x: Int) => x % 2 == 0)