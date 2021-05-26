/*
* Billy Ronico 38007483
* */

import scala.annotation.tailrec

/*

Cette classe permet de représenté un fait.

* id: Numéro du fait
* intitule: Enoncé du fait en clair

* */

case class Fait(id: Int, intitule: String)

/*

La classe permettant de représenté une règle
Une regle est une implication entre 2 list de fait (hypotheses et conclusions)

* id : numéro de la règle
* hypothese : liste des faits qui constituent l’hypothèese
* conclusion : liste des faits qui constituent la conclusion

*/

case class Regle(id: Int, hypothese: List[Fait], conclusion: List[Fait]) {

    // Permet l'affichage suivant: SI hyp1 ET hyp2 ET ... ALORS conc1 ET conc2 ET ...
    override def toString: String =
        "Si " + hypothese.foldRight(" ALORS ")(
            (fait: Fait, string: String) =>
                if (string != " ALORS ") fait.intitule + " ET " + string
                else fait.intitule + string
        ) + conclusion.foldRight("")(
            (fait: Fait, string: String) =>
                if (string != "") fait.intitule + " ET " + string
                else fait.intitule + string
        )

}

object Main extends App {

    // Permet d'afficher une base de fait
    def afficheBDFait(bdf: List[Fait]): Unit = {
        println("*** Base de faits ***")
        bdf.foreach((fait: Fait) => println(fait.intitule))
    }

    // Permet d'afficher une base de règles
    def afficheBDRegle(bdr: List[Regle]): Unit = {
        println("*** Base de règles ***")
        bdr.foreach((regle: Regle) => println(regle))
    }

    /*
      Teste si une règle est applicable.

      => Condition:
        * Pas déjà utilisé
        * Hypothèses sont connu

      => Paramètres:
        * regle: La règle
        * bdfConnus: Base de fait connus
        * reglesUtils: Liste des règles déjà utilisé

    */

    def estApplicable(regle: Regle, bdfConnus: List[Fait], reglesUtils: List[Regle] = List()): Boolean =
        !reglesUtils.exists((r: Regle) => r.id == regle.id) &&
            regle.hypothese.forall(
                (faitHyp: Fait) => bdfConnus.exists(
                    (faitConnue: Fait) => faitConnue.id == faitHyp.id
                )
            )

    /*

       Retourne couple (FaitConnus, ReglesUtils) mis à jour après application de la règle

       => Paramètres:
        * regle: La règle
        * bdfConnus: Base de fait connus
        * reglesUtils: Liste des règles déjà utilisé

    */

    def appliqueRegle(regle: Regle, bdfConnus: List[Fait], reglesUtils: List[Regle] = List()): (List[Fait], List[Regle]) = {
        println("<-- Application de la regle : " + regle)
        (bdfConnus ::: regle.conclusion, reglesUtils ::: regle :: Nil)
    }

    // Teste si un element est dans une liste
    @tailrec
    def estDans[T](x: T, list: List[T]): Boolean = list match {
        case Nil => false
        case a :: b => if (a == x) true else estDans(x, b)
    }

    // Retourne une liste qui est l'union de deux listes (dans le sens ensemblistes)
    @tailrec
    def union[T](x: List[T], y: List[T]): List[T] = x match {
        case a :: b =>
            if (!estDans(a, y)) union(b, a :: y)
            else union(b, y)
        case Nil => y
    }

    // Retourne une liste qui est la différence entre la liste x et y
    def diff[T](x: List[T], y: List[T]): List[T] = x match {
        case Nil => Nil
        case a :: b =>
            if (!estDans(a, y)) a :: diff(b, y)
            else diff(b, y)
    }
    /*

            Entrée: la base de règles (bdr), une base de faits connus (bdfConnus), les regles qui sont déja utilisé

            Sortie:

                - la base de faits terminale, c’est à dire bdfConnus dans laquelle
                on a ajouté tous les nouveaux faits qui on été déduits au cours du raisonnement,

                - la liste des regles utilisées dans le raisonnement

            Cas de base: Aucune règle n'est applicable, on retourne les bdfConnus et les regles utilisés

            Cas de recursion: On applique les regles qui sont applicables et on le rajoute dans la base de fait connus puis
                on recommence

        */

    @tailrec
    def chainageAvant(bdr: List[Regle], bdfConnus: List[Fait], reglesUtils: List[Regle] = List[Regle]()): (List[Fait], List[Regle]) = {

        val reglesApplicable = bdr.filter((regle: Regle) => estApplicable(regle, bdfConnus, reglesUtils))

        if (reglesApplicable.isEmpty) (bdfConnus, reglesUtils)
        else chainageAvant(
            bdr,
            bdfConnus ::: reglesApplicable.foldRight(List[Fait]())(
                (regle: Regle, listFaitConnus: List[Fait]) => appliqueRegle(regle, bdfConnus)._1 ::: listFaitConnus
            ).filter((f: Fait) => !estDans(f, bdfConnus)),
            reglesUtils ::: reglesApplicable
        )

    }

    // Permet d'afficher proprement le chainage avant

    def afficheChainageAvant(bdr: List[Regle], bdf: List[Fait]): Unit = {

        val resultChainageAvant = chainageAvant(bdr, bdf)

        println()

        println("Les nouveaux faits sont : ")

        resultChainageAvant._1.filter((fait: Fait) => !estDans(fait, bdf)).foreach((fait: Fait) => println(fait.intitule))

        println()
    }


    /*

    Permet de réaliser le chainage arrière

     Entrée: Une base de regles, une base de fait connus, le fait à démontrer et les regles déja utilisés
     Sortie:
        - Vrai si le fait est démontrable, fauxSinon
        - la base de fait connus dans laquelle ont été rajoutés tous les nouveaux faits
        - la liste des regles utilisés pendant le raisonnement

     Principe:
        - Cas de base: Si le fait est dans la base de fait connus, on retourne vrai, les bases de faits connus, regles utilisés
        - Cas inductif: On recupere tout les regles qui ont comme conclusion le fait à démontrer et on teste si y en a un qui est valide

    * */

    def chainageArriere(bdr: List[Regle], bdf: List[Fait], fait: Fait, reglesUtils: List[Regle] = List[Regle]()): (Boolean, List[Fait], List[Regle]) = {

        println("==> Je cherche à démontrer : " + fait.intitule)

        if (estDans(fait, bdf)) {
            println("(.) " + fait.intitule + " est dans la base des faits connus")
            (true, bdf, reglesUtils)
        }
        else {

            val regleDeduisantFait = bdr.filter(
                (regle: Regle) => !reglesUtils.contains(regle)
            ).filter(
                (regle: Regle) => regle.conclusion.contains(fait)
            )
            chainageArriereRegle(bdr, bdf, fait, regleDeduisantFait, reglesUtils)

        }

    }

    /*

    Permet de réaliser le chainage arrière sur un ensemble de regle

     Entrée: Une base de regles, une base de fait connus, le fait à démontrer, les regles deduisants le fait et les regles déja utilisés
     Sortie:
        - Vrai si le fait est démontrable, fauxSinon
        - la base de fait connus dans laquelle ont été rajoutés tous les nouveaux faits
        - la liste des regles utilisés pendant le raisonnement

     Principe:
        - Cas de base: Si les regles déduisant le fait est vide, on dira que le fait est indémontrable
        - Cas Inductif: On recupere tout les hypothèses de la tête, et on y applique le chainageArriereHyp,
            - Si tout les hypothèses sont demontrable, On arrete, la règle est donc applicable
            - Sinon on recommence avec la queue comme ensemble de regle

    * */

    @tailrec
    def chainageArriereRegle(bdr: List[Regle], bdf: List[Fait], fait: Fait, regleDeduisantFait: List[Regle], reglesUtils: List[Regle]): (Boolean, List[Fait], List[Regle]) =
        regleDeduisantFait match {
            case Nil =>
                println("(x) " + fait.intitule + " n'est pas démontrable")
                (false, bdf, reglesUtils)
            case a :: b =>
                println("En utilisant la règle (" + a.id + ") " + a)
                val chainageArriereHypRes = chainageArriereHyp(bdr, bdf, a.hypothese, reglesUtils)
                if (chainageArriereHypRes._1) {
                    println("<-- Application de la règle (" + a.id + ") " + a)
                    println("(+) " + fait.intitule + " est démontrer")
                    (
                        true,
                        chainageArriereHypRes._2,
                        a :: chainageArriereHypRes._3
                    )

                } else chainageArriereRegle(bdr, union(bdf, chainageArriereHypRes._2), fait, b, reglesUtils)
        }

    /*

    Permet de réaliser le chainage arrière sur un ensemble de fait

     Entrée: Une base de regles, une base de fait connus, les hypotheses et les regles déja utilisés
     Sortie:
        - Vrai si le fait est démontrable, fauxSinon
        - la base de fait connus dans laquelle ont été rajoutés tous les nouveaux faits
        - la liste des regles utilisés pendant le raisonnement

     Principe:
        - Cas de base: Si on a plus d'hypothèses, alors la regle
        - Cas Inductif: On recupere la premiere hypothese (tete), on applique la chainage arriere
            - Si elle est démontrable, alors on recommence avec la queue comme ensemble d'hypothèses
            - Sinon la regle qui contient ces hypothèses est inapplicable

    * */

    def chainageArriereHyp(bdr: List[Regle], bdf: List[Fait], hypotheses: List[Fait], reglesUtils: List[Regle]): (Boolean, List[Fait], List[Regle]) = {
        hypotheses match {
            case Nil => (true, bdf, reglesUtils)
            case a :: b =>

                val chainageArriereFait = chainageArriere(bdr, bdf, a, reglesUtils)
                if (chainageArriereFait._1) {
                    val chainageArriereHypSuivant =
                        if (estDans(a, bdf)) chainageArriereHyp(bdr, bdf, b, reglesUtils)
                        else chainageArriereHyp(bdr, a :: bdf, b, reglesUtils)
                    (
                        chainageArriereHypSuivant._1,
                        union(chainageArriereFait._2, chainageArriereHypSuivant._2),
                        union(chainageArriereFait._3, chainageArriereHypSuivant._3)
                    )
                } else chainageArriereFait
        }
    }

    def afficheChainageArriere(bdr: List[Regle], bdf: List[Fait], fait: Fait): Unit = {

        println("===> Lancement du chainage avant pour démontrer : est un guépard")

        println()

        val chainageArriereRes = chainageArriere(bdr, bdf, fait)

        println()

        println("+++ Les nouveaux faits sont : ")

        val nouveauxFait = diff(chainageArriereRes._2, bdf)
        nouveauxFait.foreach(
            (fait: Fait) => println(fait.intitule)
        )

        if (chainageArriereRes._1)
            println("==> Résultat : " + fait.intitule + " est démontrable !!! ")
        else println("==> Résultat : " + fait.intitule + " n'est pas démontrable !!!")

    }

    def main(): Unit = {
        // Initialisation de tout les faits

        val faitADesPoils = Fait(1, "a des poils")
        val faitEstUnMammifere = Fait(2, "est un mammifère")
        val faitDonneDuLait = Fait(3, "donne du lait")
        val faitMangeDeLaViande = Fait(4, "mange de la viande")
        val faitEstUnCarnivore = Fait(5, "est un carnivore")
        val faitADesDentsPointus = Fait(6, "a des dents pointus")
        val faitADesGriffes = Fait(7, "a des griffes")
        val faitALesYeuxVersAvant = Fait(8, "a les yeux vers avant")
        val faitADesSabots = Fait(9, "a des sabots")
        val faitEstUnOngule = Fait(10, "est un ongulet")
        val faitEstPasCarnivore = Fait(11, "n'est pas carnivore")
        val faitEstDeCouleurBrune = Fait(12, "est de couleur brune")
        val faitADesTachesSombres = Fait(13, "a des taches sombres")
        val faitEstUnGueppard = Fait(14, "est un guéppard")
        val faitADesRaiesNoirs = Fait(15, "a des raies noires")
        val faitEstUnTigre = Fait(16, "est un tigre")
        val faitAUnLongCoup = Fait(17, "a un long cou")
        val faitADesLonguesPattes = Fait(18, "a des longues pattes")
        val faitEstUnGirafe = Fait(19, "est un girafe")
        val faitEstUnZebre = Fait(20, "est un zebre")
        val faitADesPlumes = Fait(21, "a des plumes")
        val faitEstUnOiseau = Fait(22, "est un oiseau")
        val faitVole = Fait(23, "vol")
        val faitPondDesOeufs = Fait(24, "ponds des oeufs")
        val faitNeVolePas = Fait(25, "ne vole pas")
        val faitEstNoirEtBlanc = Fait(26, "est noir et blanc")
        val faitEstUnAutruche = Fait(27, "est un autruche")
        val faitNage = Fait(28, "nage")
        val faitEstUnPingouin = Fait(29, "est un pingouin")
        val faitEstUnAigle = Fait(30, "est un aigle")

        // Initialisation des règles

        val regle1 = Regle(
            1,
            List(faitADesPoils),
            List(faitEstUnMammifere)
        )

        val regle2 = Regle(
            2,
            List(faitDonneDuLait),
            List(faitEstUnMammifere)
        )

        val regle3 = Regle(
            3,
            List(faitMangeDeLaViande),
            List(faitEstUnCarnivore)
        )

        val regle4 = Regle(
            4,
            List(faitADesDentsPointus, faitADesGriffes, faitALesYeuxVersAvant),
            List(faitEstUnCarnivore)
        )

        val regle5 = Regle(
            5,
            List(faitEstUnMammifere, faitADesSabots),
            List(faitEstUnOngule)
        )
        val regle6 = Regle(
            6,
            List(faitEstUnMammifere, faitEstPasCarnivore),
            List(faitEstUnOngule)
        )
        val regle7 = Regle(
            7,
            List(faitEstUnMammifere, faitEstUnCarnivore, faitEstDeCouleurBrune, faitADesTachesSombres),
            List(faitEstUnGueppard)
        )
        val regle8 = Regle(
            8,
            List(faitEstUnMammifere, faitEstUnCarnivore, faitEstDeCouleurBrune, faitADesRaiesNoirs),
            List(faitEstUnTigre)
        )

        val regle9 = Regle(
            9,
            List(faitEstUnOngule, faitAUnLongCoup, faitADesLonguesPattes, faitADesTachesSombres),
            List(faitEstUnGirafe)
        )
        val regle10 = Regle(
            10,
            List(faitEstUnOngule, faitADesRaiesNoirs),
            List(faitEstUnZebre)
        )
        val regle11 = Regle(
            11,
            List(faitADesPlumes),
            List(faitEstUnOiseau)
        )

        val regle12 = Regle(
            12,
            List(faitVole, faitPondDesOeufs),
            List(faitEstUnOiseau)
        )

        val regle13 = Regle(
            13,
            List(faitEstUnOiseau, faitNeVolePas, faitAUnLongCoup, faitADesLonguesPattes, faitEstNoirEtBlanc),
            List(faitEstUnAutruche)
        )

        val regle14 = Regle(
            14,
            List(faitEstUnOiseau, faitNeVolePas, faitNage, faitEstNoirEtBlanc),
            List(faitEstUnPingouin)
        )
        val regle15 = Regle(
            15,
            List(faitEstUnOiseau, faitEstUnCarnivore),
            List(faitEstUnAigle)
        )


        // Initialisation de la base de regle
        val bdr = List(
            regle1, regle2, regle3, regle4, regle5, regle6, regle7, regle8, regle9,
            regle10, regle11, regle12, regle13, regle14, regle15
        )

        // Initialisation des bases de fait connus
        val bdf = List(
            faitDonneDuLait, faitMangeDeLaViande, faitADesDentsPointus, faitEstDeCouleurBrune, faitADesRaiesNoirs
        )

        // Affichage de la base de regle

        afficheBDRegle(bdr)
        println()
        afficheBDFait(bdf)

        println()

        afficheChainageAvant(bdr, bdf)

        println("******************************************************************")

        afficheChainageArriere(bdr, bdf, faitEstUnGueppard)

    }

    main()

}