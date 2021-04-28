// CALCUL DE COEFICIENT BINOMIAUX
// Calcul de p parmi n
def cooefBinomiaux(p: Int, n: Int): Int =
    if (p > n) 0
    else if (p == 0) 1
    else cooefBinomiaux(p - 1, n - 1) + cooefBinomiaux(p, n - 1)

cooefBinomiaux(1, 2)