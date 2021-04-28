import scala.annotation.tailrec

object Feuille6 {

    trait Etat {

        // id de l'état
        val id: String

        // transitions: Listes des transitions depuis l'état
        val transitions: List[(Char, Etat)]

        // terminal: Vrai si l'état est accéptant
        val terminal: Boolean

        def delta(c: Char): Etat = {
            @tailrec
            def deltaForce(c: Char, lesTransitions: List[(Char, Etat)]): Etat = lesTransitions match {
                case Nil => this
                case (char, etat) :: reste =>
                    if (char == c) etat
                    else deltaForce(c, reste)
            }

            deltaForce(c, transitions)
        }

        def affiche(): Unit = {
            println()
            println(id + " : ")
            transitions.foreach(
                (transition: (Char, Etat)) => println(transition._1 + " -> " + transition._2)
            )
            println()
        }

    }

    case object Qs extends Etat {
        val id: String = "Qs"
        val transitions: List[(Char, Etat)] = List(
            ('a', Q1), ('b', Qp), ('c', Qp)
        )
        val terminal = false
    }

    case object Qp extends Etat {
        val id: String = "Qp"
        val transitions: List[(Char, Etat)] = List(
            ('a', Qp), ('b', Qp), ('c', Qp)
        )
        val terminal = false
    }

    case object Q1 extends Etat {
        val id: String = "Q1"
        val transitions: List[(Char, Etat)] = List(
            ('a', Qs), ('b', Q1), ('c', Q2)
        )
        val terminal = false
    }

    case object Q2 extends Etat {
        val id: String = "Q2"
        val transitions: List[(Char, Etat)] = List(
            ('a', Qs), ('b', Q1), ('c', Qp)
        )
        val terminal = true
    }

    class Automate(val lesEtats: List[Etat], val alpha: String, val entree: Etat) {

        def lesTerminaux(): List[Etat] = lesEtats filter ((etat: Etat) => etat.terminal)

        def lesNonTerminaux(): List[Etat] = lesEtats filter ((etat: Etat) => !etat.terminal)

        def verifMot(mot: String): Boolean = mot forall ((c: Char) => alpha contains c)

        def accepte(mot: String): Boolean = if (verifMot(mot))
            lesTerminaux() contains mot.toList.reverse.foldRight(z = entree)(
                op = (char: Char, etat: Etat) => etat.delta(char)
            )
        else false

        def affiche(): Unit = {
            println("Alphabet: " + alpha)
            println("Etat Initial " + entree.id)
            println("Etats Terminaux : " + lesTerminaux().foldRight("!")(
                (etat: Etat, c: String) => etat.id + " " + c)
            )
            println("Transitions: ")
            lesEtats foreach ((etat: Etat) => etat.affiche())
        }

    }

    def main(args: Array[String]): Unit = {

        val auto = new Automate(List(Qs, Q1, Q2, Qp), "abc", Qs)

        auto.affiche()

        println("Mot aaabbc accépté ? : " + auto.accepte("aaabbc"))

        println("Mot aabc accépté ? : " + auto.accepte("aabc"))

        println("Mot wbda accépté ? : " + auto.accepte("wbda"))

    }

}
