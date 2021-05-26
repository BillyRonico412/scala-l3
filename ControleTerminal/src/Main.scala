object Main extends App {

    trait Arbre {

        def valeur: Double

        def fg: Arbre

        def fd: Arbre

        def collecte(p: Double => Boolean): List[Double] = this match {
            case ArbreVide => Nil
            case Noeud(valeur, fg, fd) =>
                if (p(valeur)) valeur :: (fg.collecte(p) ::: fd.collecte(p))
                else fg.collecte(p) ::: fd.collecte(p)
        }

        def reduce[A, B](op: Double => A, agrege: (A, B, B) => B, neutre: B): B = this match {
            case ArbreVide => neutre
            case Noeud(valeur, fg, fd) => agrege(op(valeur), fg.reduce(op, agrege, neutre), fd.reduce(op, agrege, neutre))
        }

        def listVal(): List[Double] =
            reduce(
                op = (x: Double) => x,
                agrege = (valCourant: Double, valFg: List[Double], valFd: List[Double]) => valCourant :: (valFg ::: valFd),
                neutre = Nil
            )

    }

    case class Noeud(valeur: Double, fg: Arbre, fd: Arbre) extends Arbre

    case object ArbreVide extends Arbre {
        // valeur, fg et fd ne sont pas définies pour l'arbre vide
        // et provoque donc une erreur si on tente d'appliquer ces méthodes
        def valeur = sys.error("operation non définie")

        def fg = sys.error("operation non définie")

        def fd = sys.error("operation non définie")
    }

    def main(): Unit = {
        val arbreEx = Noeud(
            valeur = 3,
            fg = Noeud(
                valeur = 2,
                fg = ArbreVide,
                fd = ArbreVide
            ),
            fd = Noeud(
                valeur = 12,
                fg = Noeud(
                    valeur = 10,
                    fg = ArbreVide,
                    fd = ArbreVide
                ),
                fd = Noeud(
                    valeur = 7,
                    fg = ArbreVide,
                    fd = ArbreVide
                )
            )
        )

        println(arbreEx.collecte((x: Double) => x % 2 == 0))
        println(arbreEx.listVal())

    }

    main()

}
