import scala.math.sin

// Definir un fonction permettant de composé deux operations mathematiques

def compose(f: Double => Double, g: Double => Double) = (x: Double) => f(g(x))

def fonction = compose(sin, (x: Double) => 2 * x + 1)

println(fonction(3))