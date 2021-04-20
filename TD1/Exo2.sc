import scala.annotation.tailrec

def factorielle(x: Int): Int = {
    @tailrec
    def factoTerm(x: Int, accu: Int): Int = {
        if (x == 0) accu
        else factoTerm(x - 1, x * accu)
    }
    factoTerm(x, 1)
}

println(factorielle(6))