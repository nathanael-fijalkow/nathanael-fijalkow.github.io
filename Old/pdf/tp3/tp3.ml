(* TP 3 informatique MP/MP* LLG, 2011/2012 *)
(* Nathanaël FIJALKOW *)

(* Question 1 *)

(*
La transposée de M représente le graphe où les flèches ont été inversées.
M = transposée(M) ssi M représente un graphe non-orienté
*)

(* Question 2 *)

(* Pour définir la puissance, il faut vérifier que le produit de matrice est bien associatif,
ce qui est le cas.
D(i,j) = 1 ssi il existe un chemin de longueur au plus 1 entre i et j 
(si i = j, il y a un chemin de longueur 0);
D^k(i,j) = 1 ssi il existe un chemin de longueur au plus k entre i et j *)

(* Question 3 *)

let de_succ_a_pred succ =
  let n = vect_length succ in
  let pred = make_vect n [] in
  let rec boucle i = 
    if i = n 
    then pred
    else 
      begin
	do_list (fun j -> pred.(j) <- i::pred.(j)) succ.(i) ;
	boucle (i+1)
      end
  in boucle 0 ;;

let de_pred_a_succ pred =
  let n = vect_length pred in
  let succ = make_vect n [] in
  let rec boucle j =
    if j = n 
    then succ
    else 
      begin
      do_list (fun i -> succ.(i) <- j::succ.(i)) pred.(j);
      boucle (j+1)
      end
  in boucle 0 ;;

(* Question 6 *)

(* c'est un parcours en profondeur *)

let parcours_profondeur n tab_succ sommet_depart =
  let tab_non_parcourus = make_vect n true in
  let rec boucle liste_a_traiter = match liste_a_traiter with
    | [] -> []
    | sommet_courant::suite
	when tab_non_parcourus.(sommet_courant)->
      tab_non_parcourus.(sommet_courant) <- false ;
	  sommet_courant::(boucle (tab_succ.(sommet_courant)@suite))
    | _::suite -> boucle suite
  in boucle [sommet_depart] ;;

(* parcours_profondeur 8 [|[1;2;3;4];[5];[];[4];[6];[7];[];[]|] 0 ;; *)

let parcours_largeur n tab_succ sommet_depart =
  let tab_non_parcourus = make_vect n true in
  let rec boucle liste_a_traiter = match liste_a_traiter with
    | [] -> []
    | sommet_courant::suite
	when tab_non_parcourus.(sommet_courant)->
      tab_non_parcourus.(sommet_courant) <- false ;
	  sommet_courant::(boucle (suite@(tab_succ.(sommet_courant))))
    | _::suite -> boucle suite
  in boucle [sommet_depart] ;;

(* parcours_largeur 8 [|[1;2;3;4];[5];[];[4];[6];[7];[];[]|] 0 ;; *)

(* Question 7 *)

(* On ne traite chaque sommet qu'une fois, donc un sommet est considéré par
la fonction boucle exactement k fois, où k est le nombre de voisins qu'il a,
quantité que l'on appelle le degré du sommet. *)
 
(* Question 8 *)

(* rappelons que le coût de la primitive @ est la taille de la liste de gauche.
Dans le cas du parcours en profondeur, on concatène la liste des successeurs du sommet traité.
Chaque sommet étant traité une seule fois, le coût de @ est exactement m, le nombre d'arêtes 
du graphe.
Pour le cas du parcours en largeur, on a écrit "suite @ tab_succ.(sommet_courant)",
ce qui est une très mauvaise idée : il est beaucoup plus intéressant de gèrer 
la liste à traiter avec une file LIFO (Last In First Out), pour que le coût de concatèner 
suite à la file des successeurs soit le nombre de successeurs, et obtenir le même résultat :
le coût de maintenir la liste à traiter est exactement m, le nombre d'arêtes du graphe. *)

(* Question 9 *)

let calcul_composantes_connexes n tab_succ =
  let tab_non_traites = make_vect n true in
  let rec boucle sommet_courant =
    if sommet_courant = n 
    then []
    else
      if tab_non_traites.(sommet_courant)
      then 
	let composante = parcours_profondeur n tab_succ sommet_courant in
	do_list (fun i -> tab_non_traites.(i) <- false) composante ;
	composante::(boucle (sommet_courant + 1))
      else boucle (sommet_courant + 1)
  in boucle 0 ;;

(* calcul_composantes_connexes 8 [|[1;2;3;4];[5];[];[4];[];[7];[];[]|] ;; *)

(* Question 14 *)

(* http://chris.com/ascii/art/html/animals_js.html
à lire dans un éditeur avec une police correcte, sinon ça ne marche pas.
        _    _
       ( \__//) 
       .'     )
    __/b d  .  )
   (_Y_`,     .)
    `--'-,-'  )
         (.  )
         (   )
        (   )
       ( . )         .---.
      (    )        (     )
      (   . )      (  .    )
      (      )    (      .  ),
      ( .     `"'`  .       `)\
       (      .              .)\
       ((  .      .   (   .   )\\
       ((       .    (        ) \\
        ((     )     _( .   . )  \\
        ( ( .   )"'"`(.(     )   ( ;
        ( (    )      ( ( . )     \'
         |~(  )        |~(  )
         | ||~|        | ||~|
    jgs  | || |        | || |    
        _| || |       _| || |
       /___(| |      /___(| |
          /___(         /___(
*)
