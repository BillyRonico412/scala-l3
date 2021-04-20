// Fonction fab permettant de generaliser la somme et le produit

// a <= b
def fab(f: (Int, Int) => Int): (Int, Int) => Int = {
    def func(a: Int, b: Int): Int = {
        if (a == b) a 
        else f(a, func(a + 1, b))
    }
    func
}

def somme = fab((x: Int, y: Int) => x + y)

def produit = fab((x: Int, y: Int) => x * y)