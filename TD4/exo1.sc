def twist(list: List[(Any, Any)]): List[(Any, Any)] =
    list.map((tuple: (Any, Any)) => (tuple._2, tuple._1))

twist(List((1, "b"), ("Billy", true), (false, true)))