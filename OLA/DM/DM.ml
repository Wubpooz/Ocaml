type arbre = C of char | N of arbre*arbre;;

let rec print_abr a = match a with C(c) -> Printf.printf " %c " c | N(l,r) -> print_abr l; Printf.printf " | "; print_abr r;;
let print_abr a = print_abr a; Printf.printf "\n";;

type mot = int list;;


(*1*)
let t = N(N(N(C('n'),C('f')),C('a')),N(C('s'),N(C('i'),C('t'))));;
print_abr t;;

(*2*)
let rec taille a = match a with C(c) -> 1 | N(l,r) -> 1 + taille l + taille r;;
Printf.printf "taille : %d\n" (taille t);;

(*3*)
let rec contient c a = 
  match a with 
  C(car) -> car=c 
  | N(l,r) -> contient c l || contient c r
;;
Printf.printf "contient f : %b\n" (contient 'f' t);;

(*4*)
let code_char c a = if not(contient c a) then failwith "not in this tree" 
  else 
    let rec loop c a code =
      match a with
        | C(car) when car=c -> code
        | N(l,r) -> if(contient c l) then loop c l (code@[0]) else loop c r (code@[1])
        | _ -> failwith "wtf"
    in 
      loop c a []
;;
List.iter (fun x->Printf.printf "%d" x) (code_char 'i' t);Printf.printf "\n";;

(*5*)
(*
La complexité de la fonction code_char est de O(loop) + O(contient). 
La complexité de contient est de O(n) car on parcourt l'arbre entier.
loop appelle contient à chaque recursion et "tourne" au max la hauteur de l'arbre fois donc est en O(h)*O(contient) = O(h*n). 
Au final, on a : O(code_char) = O(loop) + O(contient) = O(h*n) + O(n) = O(h*n). (h est au pire = n-1 mais on considérera que h<<n, cas d'un arbre équilibré)
**)

(*6*)
let rec reconnait m a =
  match m with
    | [] -> true
    | 0::q -> (match a with N(l,_) -> reconnait q l | _ -> false)
    | 1::q -> (match a with N(_,r) -> reconnait q r | _ -> false)
    | _ -> false
;;

Printf.printf "reconnait [0;1;1] : %b\n" (reconnait [1;1;0] t);;

(*7*)
let rec decode_mot_simple m a = 
  if reconnait m a then
    let rec loop m a =
      match m, a with 
        | [], C(c) -> Some c
        | 0::q, N(l,_) -> loop q l
        | 1::q, N(_,r) -> loop q r
        | _ -> None
    in
      loop m a
  else None
;;

Printf.printf "decode_mot_simple [1;0;1] : %s \n" (match decode_mot_simple [1;0;0] t with Some(c) -> String.make 1 c | None -> "None");;

(*8*)
(*
La complexité de decode_mot_simple est de O(reconnait) + O(loop).
reconnait est de complexité pire cas la hauteur de l'arbre, O(h).
loop est de complexité O(h) aussi assez clairement.
Au final, on a : O(decode_mot_simple) = O(reconnait) + O(loop) = O(h) + O(h) = O(h).
**)




(*9*)
let code_texte clist a =
  let rec loop clist a code =
    match clist with
      | [] -> code
      | c::q -> loop q a (code@(code_char c a))
  in
    loop clist a []
;;
Printf.printf "code_texte [n;a;s;i;t] : "; List.iter (fun x->Printf.printf "%d" x) (code_texte ['n';'a';'s';'i';'t'] t);Printf.printf "\n";;


(*10*)
let decode_mot m a =
  let rec loop m a car =
    if m=[] then None else
    let c = car@([List.hd m]) in
    if reconnait c a then Some (decode_mot_simple c a, List.tl m)
    else loop (List.tl m) a c
  in 
    loop m a []
;;

Printf.printf "%c" (match decode_mot [0;0;1] t with None -> 'N' | Some((c,_)) -> (match c with None -> 'N' | Some(c) -> c));Printf.printf "\n";;

(*11*)