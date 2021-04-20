def concat(list1: List[Any], list2: List[Any]): List[Any] =
    list1.foldRight(z = list2)(
        op = (a: Any, b: List[Any]) => a :: b
    )

def flat(list: List[List[Any]]): List[Any] =
    list.foldRight(z = List[Any]())(
        op = (a: List[Any], b: List[Any]) => concat(a, b)
    )

flat(List(List("a", true, 5), List("b", false, -5, 2.36)))