trait Arbre {

    def valeur: Double

    def fg: Arbre

    def fd: Arbre

    def collecte(p: Double => Boolean): List[Double] = this match {
        case ArbreVide => Nil
        case Noeud(valeur, fg, fd) =>
            if (p(valeur)) valeur :: (fg.collecte(p) ::: fd.collecte(p))
            else fg.collecte(p) ::: fd.collecte(p)
        case _ => sys.error("operation non définie")
    }

    def reduce[A, B](op: Double => A, agrege: (A, B, B) => B, neutre: B): B = this match {
        case ArbreVide => neutre
        case Noeud(valeur, fg, fd) =>agrege(op(valeur), fg.reduce(op, agrege, neutre), fd.reduce(op, agrege, neutre))
    }

}

case class Noeud(valeur: Double, fg: Arbre, fd: Arbre) extends Arbre

case object ArbreVide extends Arbre {
    // valeur, fg et fd ne sont pas définies pour l'arbre vide
    // et provoque donc une erreur si on tente d'appliquer ces méthodes
    def valeur = sys.error("operation non définie")

    def fg = sys.error("operation non définie")

    def fd = sys.error("operation non définie")
}

