import scala.annotation.tailrec

@tailrec
def puiss(x: Double, n: Int, accu: Double = 1): Double =
    if (n == 0) accu
    else puiss(x, n - 1, accu * x)

def somme1(a: Int, b: Int, n: Int): Double =
    if (n == 0) 1
    else puiss(a, n) + b * somme1(a, b, n-1)

@tailrec
def somme2(a: Int, b: Int, n: Int, accu: Double = 1): Double =
    if (n == 0) accu
    else somme2(a, b, n - 1, puiss(a, n) + b * accu)

puiss(12, 5)

somme1(2, 3, 2)
somme2(2, 3, 2)


