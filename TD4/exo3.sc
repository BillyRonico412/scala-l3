def concat(list1: List[Any], list2: List[Any]): List[Any] =
    list1.foldRight(z = list2)(
        op = (a: Any, b: List[Any]) => a :: b
    )

concat(List(1, 2, 3, 4), List('a', 'b', 'c', 'd'))