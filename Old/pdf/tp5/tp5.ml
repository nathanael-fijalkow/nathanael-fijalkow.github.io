(* Corrige TP 5 Jeux sur les graphes *)

(* Question 5 *)

(* Première solution : calculer la puissance n ème de M puis en déduire W_Eve *)

(* multiplication de matrices de booleens pour les lois || (OU) et && (ET) *)
let prod a b = 
  let n = vect_length a in
  let produit = make_matrix n n false in
  let rec valeur i j k = (k < n) && (((a.(i).(k) && b.(k).(j))) || (valeur i j (k+1))) in
  for i = 0 to n-1 do 
    for j = 0 to n-1 do 
      produit.(i).(j) <- valeur i j 0;
    done; 
  done;
  produit;;

let rec puissance matrice = function
  | 1 -> matrice
  | n -> let m = puissance matrice (n/2) in 
	 if n mod 2 = 0 
      then prod m m
      else prod m (prod m matrice);;

(* la fonction suivante retourne le tableau de booléen weve représentant W_Eve *)
(* test_intersection teste s'il existe v dans f tel que tab.(v) *)
let resoudre_accessibilite_unjoueur_matrice matrice f =
  let n = vect_length matrice in
  let matrice_n = puissance matrice n in
  let rec test_intersection tab_accessible f = match f with 
    | [] -> false
  	| v::suite -> tab_accessible.(v) || test_intersection tab_accessible suite
  in init_vect n (fun u -> test_intersection matrice_n.(u) f) ;;

(* Seconde solution, avec trois boucles for imbriquées *)
let resoudre_accessibilite_unjoueur_matrice matrice f =
  let n = vect_length matrice and weve = f in
  for i = 1 to n-1 do
      for u = 0 to n-1 do
        for v = 0 to n-1 do
	  if matrice.(u).(v) && weve.(v) then weve.(u) <- true ;
	done ;
      done;
    done;
  weve;;

(* Question 7 *)

let resoudre_accessibilite_unjoueur_liste pred f =
  let n = vect_length pred in
  let weve = make_vect n false in
  let rec marquer = function
    | [] -> ()
    | u::suite when weve.(u) -> marquer suite
    | u::suite -> 
      begin 
	  weve.(u) <- true;
	  marquer pred.(u);
	  marquer suite;
	end
  in marquer f; weve;;

(* Question 12 *)

(* on peut calculer en temps linéaire le tableau succ à l'aide du tableau pred.
   On supposera donc avoir les deux *)

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

(* la fonction suivante calcule l attracteur de f *)
(* veve represente V_Eve, sous la forme d'un tableau de booleens *)
(* Invariant : pour u un sommet, tab_coups_surs.(u) est au plus le nombre d'aretes depuis u 
qui ne vont pas dans weve, dites aretes "sures" *)

let resoudre_accessibilite_deuxjoueurs_liste pred succ f veve =
  let n = vect_length pred in
  let weve = make_vect n false in
  let tab_coups_surs = init_vect n (function u -> list_length succ.(u)) in
  let rec marquer u = 
    begin
      weve.(u) <- true;
      (* on traite tous les précédesseurs de u, on décrémente leur nombre d'arêtes sûres.
	 On marque ensuite ceux qui appartiennent à Eve ou n'ont plus d'arêtes sûres *)
      do_list (function v -> 
	tab_coups_surs.(v) <- tab_coups_surs.(v)-1;
	if (tab_coups_surs.(v) <= 0 || veve.(v)) && not weve.(v)
	then marquer v;) 
	pred.(u);
    end
  in do_list marquer f; weve;;

let radl = resoudre_accessibilite_deuxjoueurs_liste ;;

(* Question 21 *)

(* la fonction suivante calcule l'attracteur strict de f *)
let resoudre_accessibilite_stricte_deuxjoueurs_liste pred succ f veve =
  let n = vect_length pred in
  let weve = make_vect n false in
  (* on a besoin de deux copies de tab_coups_surs : une pour l'initialisation, une pour les itérations *)
  let tab_coups_surs = init_vect n (function u -> list_length succ.(u)) in
  let tab_coups_surs_init = copy_vect tab_coups_surs in
  let rec marquer u = 
    begin
      weve.(u) <- true;
      do_list (function v -> 
	tab_coups_surs.(v) <- tab_coups_surs.(v)-1;
	if (tab_coups_surs.(v) <= 0 || veve.(v)) && not weve.(v)
	then marquer v;) 
	pred.(u);
    end
  in 
  (* initialisation : on calcule Attr^+_0(F), en utilisant tab_coups_surs_init.
       On marque les sommets ajoutés à Attr^+_0(F) *)
  do_list (function u ->
    do_list (function v -> 
      tab_coups_surs_init.(v) <- tab_coups_surs_init.(v)-1;
      if (veve.(v) || tab_coups_surs_init.(v) <= 0) && not weve.(v) 
      then marquer v)
      pred.(u) ;
    tab_coups_surs.(u) <- tab_coups_surs_init.(u) - 1)
    f;
  weve;;

let rasdl = resoudre_accessibilite_stricte_deuxjoueurs_liste ;;

(* Question 23 *)

(* intersection de deux tableaux de booleens *)
let intersection tab_1 tab_2 = 
  let n = vect_length tab_1 in
  init_vect n (function i -> tab_1.(i) && tab_2.(i)) ;;

(* d'une liste de sommets vers un tableau de booléens *)
let list_to_tab liste n =
  let tab = make_vect n false in
  do_list (function u -> tab.(u) <- true ;) liste ;
  tab ;;

(* d'un tableau de booléens vers une liste de sommets *)
let tab_to_list tab n =
  let rec boucle u =
    if u = n then [] else
      if tab.(u) then u::(boucle (u+1)) else boucle (u+1)
  in boucle 0 ;;

(* calcul_z k retourne Z_k *)
let resoudre_buchi_deuxjoueurs_liste pred succ f veve =
  let n = vect_length pred in
  let f_tab = list_to_tab f n in
  let rec calcul_z = function
    | 1 -> f_tab
    | k -> let z_k_moins_1 = tab_to_list (calcul_z (k-1)) n in
	   let attr_strict_z_k_moins_1 = rasdl pred succ z_k_moins_1 veve in
	   intersection f_tab attr_strict_z_k_moins_1
  in radl pred succ (tab_to_list (calcul_z (n+1)) n) veve;;

let rbdl = resoudre_buchi_deuxjoueurs_liste ;;


(* Exemple *)

(*
let m = [|[|false;false;true;true;false|];
	  [|true;false;false;true;false|];
	  [|false;false;false;true;true|];
	  [|false;true;false;false;true|];
	  [|false;false;false;false;true|]|];;
let pred = [|[1];
	     [3];
	     [0];
	     [0;1;2];
	     [2;3;4]|];;
let succ = [|[2;3];
	     [0;3];
	     [3;4];
	     [1;4];
	     [4]|];;
let f = [4];;
let veve = [|false;true;true;false;false|];;

  radl pred succ f veve;;
  rasdl pred succ f veve;;
  rbdl pred succ f veve;;
  *)
