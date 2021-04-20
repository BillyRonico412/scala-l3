// La fonction de Leibniz permet de faire une approximation de PI / 8

def leibniz1(n: Int): Double = {
    if (n == 0) 1.0 / 3.0
    else (1 / ((4 * n.toDouble + 1) * (4 * n.toDouble + 3))) + leibniz1(n - 1)
}

// Leibniz avec une fonction reccursive terminale

def leibniz2(n: Int): Double = {
    def leibnizTerm(n: Int, accu: Double): Double = {
        if (n == 0) accu + 1.0 / 3.0
        else leibnizTerm(n - 1, accu + (1 / ((4 * n.toDouble + 1) * (4 * n.toDouble + 3))))
    }
    leibnizTerm(n, 0)
}

def PI(n: Int): Double = leibniz2(n) * 8

println(PI(1000))