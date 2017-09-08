(* Corrige TP 6 *)

type automate = { 
  taille : int ; 
  initial : int ;
  transitions : (char * int) list vect ;
  final : bool vect} ;;

let calcul_det a u =
  let n = string_length u in
  let rec avance q i =
    if i = n then q
    else avance (assoc u.[i] a.transitions.(q)) (i+1)
  in
  try a.final.(avance a.initial 0)
  with Not_found -> false ;;

(*
let a = { 
  taille = 5 ; 
  initial = 0 ;
  transitions = [| 
    [(`a`,1);(`b`,2)] ;
    [(`a`,1)] ;
    [(`a`,3);(`b`,4)] ;
    [(`b`,4)] ;
    [(`a`,3)] |] ; 
  final = [|false; true; false; true; false|]} ;;
calcul_det a "aa" ;;
calcul_det a "aba" ;;
calcul_det a "bab" ;;
*)

let accessible a =
  let n = a.taille in 
  let tab_conv = make_vect n (-1) and tab_inv = make_vect n (-1) in
  let rec boucle k = function
    | [] -> k
    | q::suite when tab_conv.(q) = -1 ->
      tab_conv.(q) <- k ; tab_inv.(k) <- q ; boucle (k+1) ((map snd a.transitions.(q))@suite)
    | _::suite -> boucle k suite
  in 
  let nb = boucle 0 [a.initial] in
  { taille = nb ;
    initial = 0 ;
    transitions = init_vect nb (fun i -> map (fun (c,j) -> (c,tab_conv.(j))) 
      a.transitions.(tab_inv.(i))) ;
    final = init_vect nb (fun i -> a.final.(tab_inv.(i))) } ;;  

(*
let b = { taille = 7 ; 
	  initial = 0 ;
	  transitions = [| [(`a`,1);(`b`,2)] ; 
			   [(`a`,1)] ; 
			   [(`a`,3);(`b`,4)] ; 
			   [(`b`,4)] ; 
			   [(`a`,3)]  ; 
			   [(`a`,1) ; (`b`,0)] ; 
			   [(`b`,6)]|] ; 
	  final = [|false; true; false; true; false; true ; true|]} ;;
accessible b ;;
*)

let rec suivant lettre transitions tab_etat =
  let n = vect_length transitions in 
  let tab_nv_etat = make_vect n false in
  for i = 0 to n-1 do
    if tab_etat.(i) then 
      do_list 
	(fun (c,j) -> if c = lettre then tab_nv_etat.(j) <- true) 
	transitions.(i) ;
  done ;
  tab_nv_etat ;;

let int_non_vide tab1 tab2 = 
  let n = vect_length tab1 in
  let rec boucle k =
    (k < n) && ((tab1.(k) && tab2.(k)) || boucle (k+1)) 
  in boucle 0 ;;

let calcul_nondet a u =
  let n = string_length u in
  let rec avance tab_etat i =
    if i = n then tab_etat
    else avance (suivant u.[i] a.transitions tab_etat) (i + 1)
  in let etat_initial = init_vect a.taille (fun k -> k = a.initial) in
     int_non_vide a.final (avance etat_initial 0) ;;

let a = { taille = 5 ; 
	  initial = 0 ;
	  transitions = [| 
	    [(`a`,1) ; (`a`,2) ; (`b`,2)] ; 
	    [(`a`,1)] ; 
	    [(`a`,3) ; (`b`,4)] ; 
	    [(`b`,4)] ; 
	    [(`a`,3)] |] ; 
	  final = [|false; true; false; true; false|]} ;;
calcul_nondet a "aa" ;;
calcul_nondet a "aba" ;;
calcul_nondet a "bab" ;;

let tab2int t =
  let n = vect_length t in
  let rec boucle k = 
    if k = n then 0
    else if t.(k) then 1 + 2*(boucle (k+1))
    else 2*(boucle (k+1))
  in boucle 0 ;;

let int2tab k n =
  let t = make_vect n false in
  let rec boucle i k =
    if k <> 0 then 
      if k mod 2 = 0 then boucle (i+1) (k/2)
      else (t.(i) <- true ; boucle (i+1) (k/2))
    else t
  in boucle 0 k ;;

let rec puiss2 n =
  if n = 0 then 1
  else if n mod 2 = 0 then let x = puiss2 (n/2) in x*x
  else let x = puiss2 (n/2) in 2*x*x ;;

let determinise a =
  let n = a.taille in
  let nv_taille = puiss2 n in
  let tab_transitions = make_vect nv_taille [] in

  let rec add c j = function
    | [] -> [(c,[j])]
    | (lettre,l)::suite -> 
      if lettre = c then (lettre,j::l)::suite
      else (lettre,l)::(add c j suite)
  in
  
  for i = 0 to n-1 do
    for k = 0 to nv_taille-1 do
      let t_k = int2tab k n in if t_k.(i) then 
	  do_list 
	    (fun (c,j) -> 
	      let l = add c j tab_transitions.(k) in
	      tab_transitions.(k) <- l)
	    a.transitions.(i)
    done ;
  done ;
  
  let tab_t = make_vect nv_taille [] in
  for k = 0 to nv_taille-1 do
    do_list (fun (c,l) -> 
      let t_k_c = make_vect n false in
      do_list (fun i -> t_k_c.(i) <- true) l ; 
      tab_t.(k) <- (c,tab2int t_k_c)::tab_t.(k))
      tab_transitions.(k)
  done ;
  
  { taille = nv_taille ;
    initial = tab2int (init_vect n (fun i -> i = a.initial)) ;
    transitions = tab_t ;
    final = init_vect nv_taille (fun k -> let t_k = int2tab k n in int_non_vide t_k a.final) } 
;;

(*
let a n =
  { taille = n+1 ;
    initial = 0 ;
    transitions = init_vect (n+1) 
      (fun i-> 
	if i = 0 
	then [(`a`,0);(`a`,1);(`b`,0)]
	else 
   if i < n
   then [(`a`,i+1);(`b`,i+1)]
   else []) ;
    final = init_vect (n+1) (fun i -> i = n) } ;;
   
let a_det = accessible (determinise (a 15)) ;;
*)

let rec classe tab i =
  if tab.(i) = i 
  then i 
  else let j = classe tab tab.(i) in tab.(i) <- j ; j;;

let equ tab i j =
  classe tab i = classe tab j ;;

let init_pred tab =
  let n = vect_length tab in
  let rec aux k booleen =
    if tab.(k) = booleen then aux (k+1) booleen else
      if booleen then (0,k) else (k,0)
  in aux 1 tab.(0) ;;

let minimise a =
  let n = a.taille in
  let min_f,min_nonf = init_pred a.final in
  let tab_partition = init_vect n (fun i -> if a.final.(i) then min_f else min_nonf) in
  let nb_classes = ref 2 in
  for k = 0 to n-1 do
    for i = 0 to n-2 do
      for j = i+1 to n-1 do
	let rec test_equ l1 = function
	  | [] -> true
	  | (c,q)::suite -> let q' = assoc c l1 in
			    (equ tab_partition q q') && (test_equ l1 suite)
	in if (equ tab_partition i j) && not (test_equ a.transitions.(i) a.transitions.(j)) 
	  then (tab_partition.(j) <- j ; incr nb_classes)
      done ;
    done ;
  done ;

  let a_int = 
    { taille = n ;
      transitions = init_vect n
	(fun i -> map 
	  (fun (c,j) -> (c,classe tab_partition j))
	  a.transitions.(i))  ;
      initial = classe tab_partition a.initial ;
      final = init_vect n (fun i -> a.final.(i)) } in
  accessible a_int;;

(*
let a = { taille = 4 ; 
	  initial = 0 ;
	  transitions = [| 
	    [(`a`,1) ; (`b`,2)] ; 
	    [(`a`,1) ; (`b`,3)] ;
	    [(`a`,2) ; (`b`,3)] ; 
	    [(`a`,3) ; (`b`,3)] |] ; 
	  final = [|false; true; true; false |]} ;;
minimise a ;; 
*)
