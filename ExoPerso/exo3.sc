// ALGORITHME D'EUCLIDE

// pgcd(a, b) en supposant que a et b positif ou nul

def euclide(a: Int, b: Int): Int =
    if (b == 0) a
    else euclide(b, a % b)

euclide(225, 125)
