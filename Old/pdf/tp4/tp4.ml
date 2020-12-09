(* Corrigé TP 4 *)


(* Question 1 *)

let rec rendu_monnaie liste_devises prix = match liste_devises with
      [] -> if n = 0 then [] else failwith "impossible"
    | d::suite -> if d <= prix 
      then d::(boucle liste_devises (prix-d))
      else boucle suite prix;;

(* rendu_monnaie [1;2;5;10;20;50;100;200;500] 542 *)

(* preuve d'optimalité pour les euros. On montre l'optimalité pour le système [1;2;5;10].
Remarquons que toute solution optimale utilise :
 (a) au plus une pièce de 5 (sinon on remplace deux pièces de 5 par une pièce de 10),
 (b) au plus deux pièces de 2 (sinon on remplace trois pièces de 2 par une de 5 et une de 1),
 (c) au plus une pièce de 1 (sinon on remplace deux pièces de 1 par une pièce de 2).

Montrons que le nombre de billets de 10 utilisés par toute solution optimale est E(n / 10) (partie entière), c'est-à-dire le nombre calculé par l'algorithme glouton.
La somme payée avec les pièces de 1, 2, 5 est d’au plus 1 + (2 * 2) + 5 = 10. 
Elle ne vaut pas 10, sinon on remplace par un billet de 10, elle vaut donc au plus 9. 
Le nombre de pièces de 10 utilisées est donc égal à E(n / 10).

On procède de même pour le nombre de pièces de 5.
Pour payer le reste, c’est à dire n mod 10, toute solution optimale payera en pièces de 1 et 2 au plus 1 + 2 × 2 = 5. 
Elle ne vaut pas 5, sinon on remplace par une pièce de 5.
Le nombre de pièces de 5 utilisées est donc égal à E(n / 5).

De même pour le nombre pièces de 2.
On a donc montré que toute solution optimale utilise E(n / k) devises de valeurs k, il y a donc une unique solution optimale, et elle est donnée par l'algorithme glouton.

Pour prouver la même chose dans le cas du système complet des euros, on prend son courage à deux mains
et on itère ce raisonnement depuis la devise 500, en descendant.
*)

(* Question 2 *)

(* [1;2;3;4] 6 : il y a deux solutions optimales : 3 + 3 = 4 + 2 = 6. *)

(* Question 3 *)

(* rendu_monnaie [1;3;4] 6 *)

(* Question 6 *)

(* on suppose f triée par valeurs décroissantes *)

let ordonnancement d f =
  let n = Array.length f in
  let rec boucle k fin_planifiee =
    if k = n 
    then 0
    else 
      if d.(k) >= fin_planifiee
      then 1 + boucle (k+1) f.(k)
      else boucle (k+1) fin_planifiee
  in boucle 0 0 ;;

