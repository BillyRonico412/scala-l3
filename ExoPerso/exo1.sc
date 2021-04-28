// Creer une fonction recursif permettant de calculer la somme de deux entiers
// Les seuls opérations de base sont:
// => L'ajout de 1 à un entier a
// => Le retrait de 1 à un entier a
// => Les comparaisons de à 0 d'un entiers

def somme(a: Int, b: Int): Int = {
    if (a == 0) b
    else if (a < 0) somme(a + 1, b) - 1
    else somme(a - 1, b) + 1
}

somme(-20, 3)