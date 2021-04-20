def multiple(a: Int, b: Int): List[Int] =
    List.range(0, b).filter((x: Int) => x % a == 0)

multiple(5, 200)

multiple(-3, 10)