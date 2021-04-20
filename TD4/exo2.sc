def len(list: List[Any]): Int =
    list.foldRight(z = 0)(op = (_: Any, b: Int) => 1 + b)

len(List((1, "b"), ("Billy", true), (false, true), 5))