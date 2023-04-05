(*----------------------------------- Exercice 1 -----------------------------------*)
(*
let puiss x n =
  let rec aux x n acc =
    if n = 0 then acc
    else aux x (n-1) (acc*x)
  in aux x n 1
;;

  let fast_puiss x n = 
    let rec aux x n acc =
      if n = 0 then acc
      else if n mod 2 = 0 then aux (x*x) (n/2) acc
      else aux (x*x) (n/2) (acc*x)
    in aux x n 1
  ;;

*)
(*----------------------------------- Exercice 2 -----------------------------------*)
(*
(*1*)
let est_triee t =
  let rec aux l =
    match l with
    | [] -> true
    | [x] -> true
    | x::y::l -> if x <= y then aux (y::l) else false
  in aux t
;;

Printf.printf "1 : %b\n" (est_triee [1;2;3;4;6;5;6;7;8;9;10]);;


(*2*)
let insere x l = 
  let rec aux x l =
    match l with
    [] -> [x]
    | y :: s -> if(x <= y) then x::y::s else y::aux x s
  in aux x l
;;

Printf.printf "2 : ";; List.iter (Printf.printf "%d ") (insere (15) [1;2;3;4;6;6;7;8;9;10]);; Printf.printf "\n";;


(*3*)
let tri_insertion l =
  let rec aux l acc =
    match l with
    [] -> acc
    | e :: s -> aux s (insere e acc)
  in aux l []
;;

Printf.printf "3 : ";;List.iter (Printf.printf "%d ") (tri_insertion [4;6;1;87;-4;3;49]);; Printf.printf "\n";;


(*5*)
let separe l =
  let rec aux l l1 l2 =
    match l with
    [] -> (l1, l2)
    | e :: s -> if List.length l1 < List.length l2 then aux s (e::l1) l2 else aux s l1 (e::l2)
  in aux l [] []
;;

let (l1, l2) = separe [2;4;6;8;10];;
Printf.printf "5 : ";;
List.iter (Printf.printf "%d ") l1;; Printf.printf "\n";;
List.iter (Printf.printf "%d ") l2;; Printf.printf "\n";;


(*6*)
let fusion l1 l2 =
  let rec aux l1 l2 = 
    match l1, l2 with
    [], [] -> []
    | e1::s1, [] -> e1::s1
    | [], e2::s2 -> e2::s2
    | e1::s1, e2::s2 -> if e1<=e2 then e1::aux s1 l2 else e2::aux l1 s2
  in aux l1 l2
;;

Printf.printf "6 : ";; List.iter (Printf.printf "%d ") (fusion [1;3;5;7;9] [2;4;6;8;10]);; Printf.printf "\n";;


(*7*)
let tri_fusion l =
  let rec aux l =
    match l with
    [] -> []
    | [x] -> [x]
    | _ -> let (l1, l2) = separe l in fusion (aux l1) (aux l2)
  in aux l
;;

Printf.printf "7 : ";; List.iter (Printf.printf "%d ") (tri_fusion [4;6;1;87;-4;3;49]);; Printf.printf "\n";;


(*8*)
let min_list l =
  let rec aux l curr_min =
    match l with
    [] -> curr_min
    | e::s -> if e<curr_min then aux s e else aux s curr_min
  in aux l (List.hd l)
;;

Printf.printf "8 : %d\n" (min_list [4;6;1;87;-4;3;49]);;


(*9*)
let remove x l =
  let rec aux x l =
    match l with
    [] -> []
    | e :: s -> if e=x then s else e::aux x s
  in aux x l
;;

Printf.printf "9 : ";; List.iter (Printf.printf "%d ") (remove 3 [1;2;3;4;5;6;7;8;9;10]);; Printf.printf "\n";;


(*10*)
let tri_selection l =
  let rec aux l acc =
    match l with
    [] -> List.rev acc
    | s -> aux (remove (min_list s) s) (min_list s::acc)
  in aux l []
;;

Printf.printf "10 : ";; List.iter (Printf.printf "%d ") (tri_selection [4;6;1;87;-4;3;49]);; Printf.printf "\n";;

*)
(*----------------------------------- Exercice 3 -----------------------------------*)

type arbre = V | N of arbre*arbre;;

(*1*)
let rec taille a = match a with V->0 | N(l,r) -> 1+ taille l + taille r;;
Printf.printf "1 : %d\n" (taille (N(N(V,V),N(N(V,V),V))));;


(*2*)
let rec hauteur a = match a with V -> 0 | N(l,r) -> 1 + max (hauteur l) (hauteur r);;
Printf.printf "2 : %d\n" (hauteur (N(N(V,V),N(N(V,V),V))));;


(*3*)
let rec equilibre a = match a with V -> true | N(l,r) -> if(abs (hauteur l)-(hauteur r) )<=1 then ((equilibre l) && (equilibre r)) else false;;
Printf.printf "3 : %b\n" (equilibre (N(N(V,V),N(N(V,V),V))));;


(*4*)
let rec equilibre' a = match a with V -> Some 0 | N(l,r) -> if(abs (hauteur l)-(hauteur r) )>1 then None else Some (max (hauteur l) (hauteur r));;
Printf.printf "4 : %b\n" (equilibre' (N(N(V,V),N(N(V,V),V))) != None);;

(*5*)
let rec complet a = match a with V -> true | N(l,r) -> if(l=V && r=V) then true else if(l=V || r=V) then false else (complet l) && (complet r);;
Printf.printf "5 : %b\n" (complet (N(N(V,V),N(N(V,V),V))));;


(*5*)
let rec parfait a = complet a &&  match a with V -> true | N(l,r) -> if(hauteur l = hauteur r) then true else false;;
Printf.printf "6 : %b\n" (parfait (N(N(V,V),N(N(V,V),V))));;
Printf.printf "%b\n" (parfait (N(N(N(V,V),N(V,V)),N(N(V,V),N(V,V)))));;


(*7*)
let rec genere_parfait n = match n with 0 -> V | _ -> N(genere_parfait (n-1), genere_parfait (n-1));;
Printf.printf "7 : %b\n" (parfait (genere_parfait 3));;