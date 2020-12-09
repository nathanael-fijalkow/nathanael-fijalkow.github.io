(* Le mot minimal *)

type expr = Vide | Epsilon | Lettre of bool | Union of expr * expr 
| Concat of expr * expr | Etoile of expr ;;

let rec est_vide = function 
 | Vide -> true
 | Epsilon | Lettre _ -> false 
 | Union(e,e') -> est_vide e && est_vide e' 
 | Concat(e,e') -> est_vide e || est_vide e' 
 | Etoile _ -> false ;;

let rec longueur_mot_min = function 
 | Vide -> None 
 | Epsilon ->  Some 0 
 | Lettre _ -> Some 1 
 | Union(e,e') -> 
     begin
       match (longueur_mot_min e,longueur_mot_min e') with 
       | None,None -> None 
       | None,Some x' -> Some x' 
       | Some x,None -> Some x 
       | Some x,Some x' -> Some (min x x')
     end
 | Concat(e,e') -> 
     begin
       match (longueur_mot_min e,longueur_mot_min e') with 
	 | None,_ | _,None -> None 
	 | Some x,Some x' -> Some (x+x')
     end
 | Etoile _ -> Some 0 ;;

(* Eleusis *)

type automate = 
    { taille : int ; 
      initial : int ; 
      transitions : (char * int) list vect ; 
    } ;;

let jouer auto =
  let rec boucle etat_courant = 
    print_string "Proposer une carte (0 ou 1)  " ;
    try 
      let carte = (read_line ()).[0] in
      let nv_etat = assoc carte auto.transitions.(etat_courant) in 
      print_string "Accepté\n" ; print_newline () ; boucle nv_etat
    with 
      | Not_found -> begin print_string "Rejeté\n" ; print_newline(); boucle etat_courant end
      | End_of_file -> print_string "Fini\n" ;
  in boucle auto.initial ;;

let a = { 
  taille = 5 ; 
  initial = 0 ;
  transitions = [| 
    [(`0`,1);(`1`,2)] ;
    [(`0`,1)] ;
    [(`0`,3);(`1`,4)] ;
    [(`1`,4)] ;
    [(`0`,3)] |] } ;; 

(* jouer a ;; *)
