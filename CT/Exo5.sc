import scala.annotation.tailrec

@tailrec
def forall2[T](p: (T, T) => Boolean, l1: List[T], l2: List[T]): Boolean =
    (l1.isEmpty && l2.isEmpty) || ((l1.nonEmpty && l2.nonEmpty) && (p(l1.head, l2.head) && forall2(p, l1.tail, l2.tail)))

forall2((x:Int,y:Int)=>x==y, List(1,2,3,4), List(1,2))

forall2((x:Int,y:Int)=>x==y, List(1,2,3,4), List(1,2,3,4))

forall2((x:String, y:String)=>x<y, List("aa", "bb", "ccc"), List("ccc", "gg", "z"))