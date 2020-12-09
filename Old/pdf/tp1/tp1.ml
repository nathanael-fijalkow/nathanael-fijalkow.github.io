(* Tp 1 informatique MP/MP* LLG, 2011/2012 *)
(* Nathanaël FIJALKOW *)

(* Question 1 *)

let rec fibo n = match n with
  | 0 -> 0
  | 1 -> 1
  | _ -> fibo (n-1) + fibo (n-2) ;;

(* Question 2 *)

(* fibo n se calcule en exactement fibo n appels de fibo! *)

let fibo_couple n = 
  let rec aux_couple (u,v) n = match n with
    | 0 -> u
    | _ -> aux_couple (v,u + v) (n-1)
in aux_couple (0,1) n ;;

(* Le nombre d'appels de fibo_couple est lineaire en n*)

(* Question 3 *)

let rec exponentiation x n = match n with
  | 0 -> 1
  | _ -> let y = exponentiation x (n/2) in
  if n mod 2 = 0 then y * y else y * y * x ;;

(* Question 4 *)

(* la complexite verifie 
c(0) = 0 et c(n) <= c(n/2) + 2
donc c(n) = O(log n)
*)

(* Question 5 *)

(* la fonction est_ce_moi renvoie toujours 'true'.
En effet, le motif (Mail mon_mail) ne teste *pas* l'egalite du mail donne
avec la variable mon_mail : il declare une nouvelle variable mon_mail
correspondant a la valeur donnee en parametre, en ecrasant la variable
precedente. En particulier, il reussit toujours (quand on donne un mail). *)

(* Question 6 *)

let diviseurs_premiers n =
  let rec boucle_testd d n =
    if n = 1 
      then []
      else if n mod d <> 0 
        then boucle_testd (d+1) n
        else d :: boucle_testd d (n / d)
in boucle_testd 2 n ;;

(* Question 7 *)

let rec map f liste = match liste with
  | [] -> []
  | h::t -> (f h) :: map f t ;;

(* Question 8 *)

let rec rev liste =
  let rec rev_append liste accu = match liste with
    | [] -> accu
    | h::t -> rev_append t (h::accu)
in rev_append liste [] ;;

(* Question 9 *)

let rec insertion_tri liste elt = match liste with
  | [] -> [elt]
  | h::q -> if h < elt then h :: insertion_tri q elt else elt :: liste ;;

let tri_insertion liste =
  let rec boucle liste liste_triee = match liste with
    | [] -> liste_triee
    | h::q -> boucle q (insertion_tri liste_triee h)
in boucle liste [] ;;

(* Question 10 *)

let rec pivot a liste = match liste with
  | [] -> [],[]
  | h::q -> let linf,lsup = pivot a liste in
  if h < a then (h::linf,lsup) else (linf,h::lsup) ;;

let rec tri_rapide = function
  | [] -> []
  | a::liste -> let linf,lsup = pivot a liste in
  tri_rapide linf @ (a :: tri_rapide lsup) ;;
