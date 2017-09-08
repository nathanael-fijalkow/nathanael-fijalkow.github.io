(* TP 2 informatique MP* Louis-le-grand, 2011/2012 *)
(* Nathanaël FIJALKOW *)

(* Question 1 *)

(* l'ordre d'evaluation des arguments *)

(* Question 2 *)

(* int : 6 *)

(* Question 3 *)

exception Trouve ;;
let existe elt tableau =
try 
  for i = 0 to vect_length tableau - 1 do
    if tableau.(i) = elt then raise Trouve ;
  done ; false
with Trouve -> true ;;

(* Question 5 *)

let nouvelle_matrice n p x =
  let mat = make_vect n [||] in
    for i = 0 to n-1 do
      mat.(i) <- make_vect p x ;
 done ; mat ;;

(* let nouvelle_matrice n p x =
init_vect n (fun _ -> make_vect p x) ;; *)

(* Question 6 *)

exception Fini;; 

let prochain_zero nc grille i j =
let rec boucle i' j' =
if i' < nc
  then 
    if j' < nc
      then (if grille.(i').(j') = 0 then (i',j') else boucle i' (j'+1))
      else boucle (i'+1) 0
  else raise Fini
in boucle i j;;

let choix_suivant nc grille liste_choix = match liste_choix with
  | [] -> let (k,l) = prochain_zero nc grille 0 0 in [(k,l,1)]
  | (i,j,_)::_ -> let (k,l) = prochain_zero nc grille i (j+1) in (k,l,1)::liste_choix ;;

(* Question 7 *)

exception Echec;;

let rec changer_choix nc grille liste_choix = match liste_choix with
  | [] -> raise Echec
  | (i,j,k)::suite -> if k < nc then (i,j,k+1)::suite else changer_choix nc grille suite ;;

(* Question 8 *)

exception Erreur;;

let verifier_ligne nc grille i j k =
let rec boucle j' =
j' = nc || ((j' = j || grille.(i).(j') <> k) && boucle (j'+1))
in boucle 0 ;;

let verifier_colonne nc grille i j k =
let rec boucle i' =
i' = nc || ((i' = i || grille.(i').(j) <> k) && boucle (i'+1))
in boucle 0 ;;

let verifier_carre n grille i j k =
let rec boucle i' j' idivn jdivn =
    i' = idivn + n
  || j' = jdivn + n && boucle (i'+1) jdivn idivn jdivn
  || (j' < jdivn + n && 
	((i,j) = (i',j') || grille.(i').(j') <> k) && boucle i' (j'+1) idivn jdivn)
in
let idivn = n*(i/n) and jdivn = n*(j/n) in
 boucle idivn jdivn idivn jdivn ;;

let remplir_grille nc grille liste_choix =
let grille_remplie = make_matrix nc nc 0 in
for i = 0 to nc-1 do for j = 0 to nc-1 do grille_remplie.(i).(j) <- grille.(i).(j); done; done;
do_list (fun (i,j,k) -> grille_remplie.(i).(j) <- k) liste_choix ;
grille_remplie;;

let correct n nc grille liste_choix =
let grille_remplie = remplir_grille nc grille liste_choix in
try
  match liste_choix with
    | [] -> true
    | (i,j,k)::_ -> 
	verifier_ligne nc grille_remplie i j k &&
	verifier_colonne nc grille_remplie i j k &&
	verifier_carre n grille_remplie i j k
with Erreur -> false ;;


let resoudre_sudoku n grille =
let nc = n*n in
let rec boucle liste_choix =
try
  if correct n nc grille liste_choix
    then let nv_liste_choix = choix_suivant nc grille liste_choix in boucle nv_liste_choix
    else let nv_liste_choix = changer_choix nc grille liste_choix in boucle nv_liste_choix
with | Fini -> remplir_grille nc grille liste_choix
     | Echec -> failwith "pas de solution"
in boucle [] ;;

(* let mat_vide = make_matrix 9 9 0;;
resoudre_sudoku 3 mat_vide;; *)
