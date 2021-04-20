// Fonction permettant de supprimer les n premiers elements de la list 
def supn[T](n: Int, list: List[T]): List[T] = 
    if (n == 0) list
    else list match {
        case a :: b => supn(n-1, b)
        case Nil => Nil
    }

// Fonction permettant de tester si list1 est péfixe list2
def estPrefixe[T](list1: List[T], list2: List[T]): Boolean = list1 match {
    case a :: b => list2 match {
        case c :: d => {
            if (a == c) estPrefixe(b, d)
            else false
        }
        case Nil => false
    }
    case Nil => true
}

def concat[T](list1: List[T], list2: List[T]): List[T] = list1 match {
        case Nil => list2
        case a :: b => a :: concat(b, list2)
    }

def substitue[T](list: List[T], list1: List[T], list2: List[T]): List[T] = 
    if (list1.isEmpty) list
    else if (estPrefixe(list1, list)) concat(list2, substitue(supn(list1.length, list), list1, list2))
    else list match {
        case a :: b => a :: substitue(b, list1, list2)
        case Nil => Nil
    }

// println(supn(2, "Billy" :: "Wendy" :: "Chaggy" :: "Kobby" :: "Brandy" :: Nil))

// println(supn(7, "Billy" :: "Wendy" :: "Chaggy" :: "Kobby" :: "Brandy" :: Nil))

// println(estPrefixe("Billy" :: "Wendy" :: Nil, "Billy" :: "Wendy" :: "Chaggy" :: "Kobby" :: "Brandy" :: Nil))

// println(estPrefixe("Olivier" :: "Dilane" :: Nil, "Billy" :: "Wendy" :: "Chaggy" :: "Kobby" :: "Brandy" :: Nil))

// println(estPrefixe("Billy" :: "Wendy" :: "Chaggy" :: "Kobby" :: "Brandy" :: Nil, "Billy" :: "Wendy" :: "Chaggy" :: "Kobby" :: "Brandy" :: Nil))

// println(estPrefixe("Billy" :: "Wendy" :: "Chaggy" :: "Kobby" :: "Brandy" :: "Dorine" :: Nil, "Billy" :: "Wendy" :: "Chaggy" :: "Kobby" :: "Brandy" :: Nil))

println(substitue(1 :: 2 :: 4 :: 4 :: 5 :: 2 :: 4 :: Nil, Nil, 0 :: 10 :: 20 :: Nil))

