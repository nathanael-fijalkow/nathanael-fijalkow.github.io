exception fini ;;

let rec atteint gauche droite r = match r with
	| [] -> false
	| a::s -> (a = gauche) || (a = droite) || atteint (gauche -1) (droite +1) s ;;

(* bonne_position teste si ajouter k en position (i,j) à la grille ne cree pas d erreur*)
let bonne_position echiquier  = match echiquier with
	| [] -> true
	| a::r -> not (mem a r) && not (atteint (a-1) (a+1) r) ;;


let nouveau_choix echiquier = 1::echiquier ;;

let rec changer_choix echiquier n = match echiquier with
	| [] -> raise fini
	| a::r -> if a < n then (a+1)::r else changer_choix r n ;;

let rec reines_aux echiquier liste_solutions n =
try
if bonne_position echiquier
	then if list_length echiquier = n 
		then reines_aux (changer_choix echiquier n) (echiquier::liste_solutions) n
		else reines_aux (nouveau_choix echiquier) liste_solutions n
	else reines_aux (changer_choix echiquier n) liste_solutions n
with	fini -> liste_solutions ;;

let reines n = reines_aux [1] [] n ;;

reines 8 ;;
