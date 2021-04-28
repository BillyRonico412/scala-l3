// Creer une fonction recursif permettant de calculer la somme de deux entiers
// Les seuls opérations de base sont:
// => L'ajout de 2 entiers: a + b
// => Le retrait de 1 à un entier a: a - 1
// => Les comparaisons de à 0 d'un entiers: a = 0

def produit(a: Int, b: Int): Int =
    if (a == 0) 0
    else b + produit(a - 1, b)

produit(3, 7)
