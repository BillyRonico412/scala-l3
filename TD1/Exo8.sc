import scala.math.sqrt

// f : la fonction a calculer l'integrale, n: le nombre de rectangle

def integrale(f: Double => Double, n: Int): (Double, Double) => Double = {
    def func(a: Double, b: Double): Double = {
        def calculeRectangle(k: Int): Double = {
            if (k == 0) 0
            else {
                val milieu = a + ((k - 1) * (b - a) / n) + (b - a) / (n * 2)
                val aire = f(milieu) * (b - a).toDouble / n
                aire + calculeRectangle(k-1)
            }
        }
        calculeRectangle(n)
    }
    func
}

def integraleRacine = integrale(sqrt, 100)

def integraleRacine1aT = integraleRacine(1, _: Double)

println(integraleRacine1aT(10))
