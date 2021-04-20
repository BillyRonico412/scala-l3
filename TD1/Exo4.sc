// Cas de base: a = b return a
// Cas inductif: a * produit(a + 1, b)

def produit(a: Int, b: Int): Int = {
    def produitTerm(a: Int, b: Int, accu: Int): Int = {
        if (a == b) accu * a
        else produitTerm(a + 1, b, a * accu)
    }
    produitTerm(a, b, 1)
}

println(produit(5, 8))