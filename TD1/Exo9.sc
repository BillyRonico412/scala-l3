def generec(
    base: Int => Boolean, 
    valBase: Int => Double, 
    compose: (Int, Double) => Double, 
    decrois: Int => Int
): Int => Double = {
    
    def f(x: Int): Double = {
        if (base(x)) valBase(x)
        else compose(x, f(decrois(x)))
    }

    f

}

def factorielle = generec(
    (x: Int) => x == 1,
    (x: Int) => x,
    (x: Int, y: Double) => x * y,
    (x: Int) => x - 1
)

println(factorielle(5))