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
let rec contient c a = match a with C(car) -> car=c | N(l,r) -> contient c l || contient c r;;
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
