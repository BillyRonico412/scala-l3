// Factorielle
// Cas de base: 0! = 1
// Cas inductive: pour x > 0, x! = x * (x - 1)!

def factorielle(x: Int): Int = {
    if (x == 0) 1
    else x * factorielle(x - 1)
}

println(factorielle(5))