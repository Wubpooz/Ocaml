(*TODO : refaire qu.10 et MIEUX GERER CE FAILWITH qu.12*)

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
  match m, a with
    [], C(_) -> true
    | 0::q, N(l,_) -> reconnait q l
    | 1::q, N(_,r) -> reconnait q r
    | _ -> false
;;

Printf.printf "reconnait [0;1;1] : %b\n" (reconnait [1;1;0] t);;

(*7*)
let rec decode_mot_simple m a = 
  if reconnait m a then
    let rec loop m a =
      match m, a with 
        [], C(c) -> Some c
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
(*
Soit m un mot binaire et Human t un arbre de préfixe binaire. 
Chaque nœud de l'arbre représente un préfixe de mots binaires. 
En descendant de la racine de l'arbre vers une feuille, on ajoute des bits au préfixe représenté par le nœud.
Supposons que 𝑘 et 𝑙 soient deux entiers tels que les 𝑘 premiers bits de m soient le code valide d'un caractère selon l'arbre Human t et les 𝑙 premiers bits de m soient le code valide d'un autre caractère. 
Sans perte de généralité, supposons que 𝑘 ≤ 𝑙.
Cela signifie que le préfixe représenté par le nœud correspondant à 𝑘 bits de l'arbre Human t est une feuille et représente un caractère valide. 
Cependant, le préfixe représenté par le nœud correspondant aux 𝑙 bits de l'arbre n'est pas une feuille, sinon cela signifierait que les 𝑘 premiers bits de m ne représenteraient pas un caractère valide selon l'arbre.
Comme l'arbre de préfixe binaire ne peut pas avoir deux feuilles identiques avec des préfixes différents, cela signifie que les 𝑙 bits de m ne peuvent pas représenter un autre caractère valide que celui représenté par les 𝑘 premiers bits de m. 
Par conséquent, il ne peut exister qu'un seul entier 𝑘 tel que les 𝑘 premiers bits de m soient le code valide d'un caractère selon l'arbre Human t.
**)


(*11*)
let decode_mot m a =
  let rec loop m a car =
    if m=[] then None else
    let c = car@([List.hd m]) in
    if reconnait c a then Some (decode_mot_simple c a, List.tl m)
    else loop (List.tl m) a c
  in 
    loop m a []
;;
Printf.printf "%c" (match decode_mot [0;0;1;0;1;0] t with None -> 'N' | Some((c,_)) -> (match c with None -> 'N' | Some(c) -> c));Printf.printf "\n";;


(*12*)
let decode_texte m a =
  let rec loop m a txt =
    match m with
    [] -> txt
    | _ -> let tmp = (match decode_mot m a with None -> failwith "Word unrecognized" | Some(c,w)->(c,w)) in loop (snd tmp) a (txt@[match (fst tmp) with None -> 'N' | Some(c)->c])
  in loop m a []
;;

Printf.printf "decode_texte [0;0;1;0;1;0] : "; List.iter (fun x->Printf.printf "%c" x) (decode_texte [0;0;1;1;1;0] t);Printf.printf "\n";;




(*13*)
(*
On va d'abaord avoir A1 :  |    car d'occurences 1 chacunes, cela donne un sous-arbre de poids 2.
                          / \  
                         f  n

On va ensuite avoir A2 :  |    car d'occurences 2 chacunes, cela donne un sous-arbre de poids 4.
                         / \
                        i  A1 

Puis on va avoir A3 :  |    car de poids les plus faibles 2,3<4 => sous-arbre de poids 5.
                      / \
                     t  a

On continue avec A4 :  |    car de poids les plus faibles 3,4<5 => sous-arbre de poids 7.
                      / \
                     s  A2

Finalement, on a A5 :  |   puisque A3 et A4 sont seuls restants. L'arbre final est de poids 12.
                      / \
                     A3  A4


A5 : 
                     |
                    / \
                  / \  \
                 t  a   \
                       / \
                      s   \
                         / \
                        i   \
                           / \
                          f  n
**)


(*14*)
let rec poids a m =
  