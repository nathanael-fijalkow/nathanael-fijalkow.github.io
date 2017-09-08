(* question 1 *)

let initialise n =
  let v = make_vect n 0 in
    for i = 0 to n-1 do v.(i) <- i done ;
    v ;;

let compte v =
  let n = vect_length v
  and compteur = ref 0
  in 
    for i = 0 to n - 1 do if v.(i) = i then incr compteur done ;
    !compteur ;;

let teste v x y =
  v.(x) = v.(y) ;;

let regroupe v x y =
  let j = v.(y) and k = v.(x) in
  let j' = min j k and k' = max j k in
    for i = 0 to vect_length v - 1 do 
      if v.(i) = k' then v.(i) <- j' done ;;

(* première approche *)

type structure = { mutable taille : int ; pere : int vect } ;;

let initialise n =
  let f = { taille = n ; pere = make_vect n 0 } in
    for i = 0 to n-1 do f.pere.(i) <- i done ;
    f ;;

let rec racine f x =
  if f.pere.(x) = x then x else racine f f.pere.(x) ;;

let compte f = f.taille ;;

let test f x y =
  (racine f x) = (racine f y) ;;

let regroupe f x y =
  let px = racine f x and py = racine f y in
    if px <> py then
      begin
	f.taille <- f.taille - 1 ;
	f.pere.(py) <- px
      end ;;

(* utilisation d'une table des poids *)

type structure = { mutable taille : int ; pere : int vect ; poids : int vect } ;;

let initialise n =
  let f = { taille = n ; pere = make_vect n 0 ; poids = make_vect n 1 } in
    for i = 0 to n-1 do f.pere.(i) <- i done ;
    f ;;

let rec racine f x =
  if f.pere.(x) = x then x else racine f f.pere.(x) ;;

let compte f = f.taille ;;

let teste f x y =
  (racine f x) = (racine f y) ;;

let regroupe f x y =
  let px = racine f x and py = racine f y in
    if px <> py then 
      let wx = f.poids.(px) and wy = f.poids.(py) in
	f.taille <- f.taille - 1 ;
	if wy < wx then
	  begin
	    f.pere.(py) <- px ;
	    f.poids.(px) <- wx + wy
	  end  
	else 
	  begin
	    f.pere.(px) <- py ;
	    f.poids.(py) <- wx + wy
	  end ;;

(* avec compression des chemins *)

type structure = { mutable taille : int ; pere : int vect ; poids : int vect } ;;

let initialise n =
  let f = { taille = n ; pere = make_vect n 0 ; poids = make_vect n 1 } in
    for i = 0 to n-1 do f.pere.(i) <- i done ;
    f ;;

let racine f x =
  let rec ancetre x =
    if x = f.pere.(x) then x else ancetre f.pere.(x)
  in
  let p = ancetre x in
  let rec compression x =
    let y = f.pere.(x) in
      if x <> y then (f.pere.(x) <- p ; compression y)
  in
    compression x ;
    f.pere.(x) ;;

let compte f = f.taille ;;

let teste f x y =
  (racine f x) = (racine f y) ;;

let regroupe f x y =
  let px = racine f x and py = racine f y in
    if px <> py then 
      let wx = f.poids.(px) and wy = f.poids.(py) in
	f.taille <- f.taille - 1 ;
	if wy < wx then
	  begin
	    f.pere.(py) <- px ;
	    f.poids.(px) <- wx + wy
	  end  
	else 
	  begin
	    f.pere.(px) <- py ;
	    f.poids.(py) <- wx + wy
	  end ;;
