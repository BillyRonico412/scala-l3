// Cas de base: a = b: a
// Cas inductif: a * produit(a + 1, b)

def produit(a: Int, b: Int): Int = {
    if (a == b)  a
    else a * produit(a + 1, b)
}

println(produit(5, 8))