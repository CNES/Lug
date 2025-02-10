# Lug
Lug est un prouveur automatique qui permet la spécification et le raisonnement dans les logiques finiment valuées.

## Fonctionnement
L'ensemble des théorèmes et de leur preuve s'écrit dans un fichier .lug qui pourra ensuite être exécuté par la commande
```dune exec lug <nom>```

La structure pour la preuve d'un théorème est la suivante.

```
Theorem <nom> : <formule>.
Proof.
    apply R_1.
    apply R_2.
    ...
    apply R_n.
    Qed.
```

Le mot clef ``Qed`` marque la fin de la preuve. Aussi, une liste des objectifs à prouver est indiquée après l'application d'une règle.

Les règles structurelles sont indépendantes de la logique manipulée et sont identifiées par les noms ``Cont_k``, ``Perm_k`` et ``Weak_k`` pour les règles respectives de la contraction, de la permutation et de l'affaiblissement en position k dans le séquent.

Il est possible d'entrer en mode automatique pour la preuve d'un objectif en spécifiant ``apply auto``.

La déclaration de la sémantique de la logique se fait dans un fichier .log qui pourra être appelé avant une preuve à l'aide de ```Use <nom>```.

## Exemple
On prouve dans la logique de Lukasiewicz à trois valeurs de vérité que A /\ B -> B \\/ A.
```
Use Luk3.

Theorem T : Imp(And(A,B),Or(B,A)).
Proof.
    apply Imp_2.
    apply Or_2.
    apply Or_1.
    apply axiom.
    apply And_0.
    apply axiom.
    apply Or_2.
    apply And_1.
    apply And_0.
    apply axiom.
    apply And_0.
    apply axiom.
    Qed.
```

## Copyright

Vladislas de Haldat, Space Transportation Directorate, CNES

August 2024

Released under the terms of CeCILL license (see file LICENSE)
