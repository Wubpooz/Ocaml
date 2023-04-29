(*-------------------------------------------- I. Arbre de Huffman -------------------------------------------- *)

type arbre = C of char | N of arbre*arbre;;
type mot = int list;;
let rec print_abr a = match a with C(c) -> Printf.printf " %c " c | N(l,r) -> print_abr l; Printf.printf " | "; print_abr r;;
let print_abr a = print_abr a; Printf.printf "\n";;


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
let code_char c a = if not(contient c a) then failwith "caractère non présent dans l'arbre" 
  else 
    let rec loop c a code =
      match a with
        | C(car) when car=c -> code
        | N(l,r) -> if(contient c l) then loop c l (code@[0]) else loop c r (code@[1])
        | _ -> failwith "caractère non présent dans l'arbre, erreur du code de la fonction contient"
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


(*-------------------------------------------- II. Codage d'un texte entier --------------------------------------------*)


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
Soient un mot m et un arbre de Huffman t.
Analyse : (unicité)
Supposons qu'il existe un entier k, maximal, tel que les k premiers bits de m soient le code valide d'un caractère, c, de t.
On peut noter alors m' le mot m privé de ses k premiers bits. Si m' contient un caractère valide de t, on note k' le nombre de bits de ce caractère.
On a alors que les k'+k premiers bits de m sont le code valide d'un caractère de t. Or k'+k>k, ce qui est absurde.
Donc il n'existe pas de k tel que les k premiers bits de m soient le code valide d'un caractère de t. 

Synthèse : (existence faible)
On a que soit m n'est pas un mot valide (ne commence par aucun caractère de t) et donc k n'existe par définition (d'où le 'au plus').
Soit que m est un mot valide et donc qu'il y a au moins un caractère c de t qui commence par les k premiers bits de m.

Ainsi, on a qu'il existe au plus un k tel que les k premiers bits de m soient le code valide d'un caractère de t.
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
    | _ -> let tmp = (match decode_mot m a with None -> (None,[]) (*ou failwith "Mot inconnu"*) | Some(c,w)->(c,w)) in loop (snd tmp) a (txt@[match (fst tmp) with None -> 'N' | Some(c)->c])
  in loop m a []
;;

Printf.printf "decode_texte [0;0;1;0;1;0] : "; List.iter (fun x->Printf.printf "%c" x) (decode_texte [0;0;1;1;1;0] t);Printf.printf "\n";;


(*-------------------------------------------- III. Construction de l'arbre de Huffman --------------------------------------------*)


(*13*)
(*
On va d'abaord avoir A1 :  |    car d'occurences 1 chacunes, cela donne un sous-arbre de poids 2.
                          / \  
                         n  f

On va ensuite avoir A2 :  |    car d'occurences 2 chacunes, cela donne un sous-arbre de poids 4.
                         / \
                        i  t 

Puis on va avoir A3 :  |    car de poids les plus faibles 2,3<=3 => sous-arbre de poids 5.
                      / \
                     A1  a

On continue avec A4 :  |    car de poids les plus faibles 3,4<5 => sous-arbre de poids 7.
                      / \
                     s  A2

Finalement, on a A5 :  |   puisque A3 et A4 sont seuls restants. L'arbre final est de poids 12.
                      / \
                     A3  A4


A5 : 
                      |
                     / \ 
                    /   \
                  /      \
                / \      /\
               /   \    /  \
              /    a   s    \
            / \            / \
           n  f           i  t
**)


(*14*)
(*
let rec poids a stats = 
  match a with
    C(c) -> stats.(Char.code(c))
    | N(l,r) -> (poids l stats) + (poids r stats)
;;

Printf.printf "poids t : %d\n" (poids t [|3;1;2;1;3;2|]);;
**)


(*15*)
(*
Posons $var := le type de var dans ce qui suit :

huffman : $stats -> snd $(extract_min $(file)) 
        : $occ array -> snd $(extract_min 'a prio)     (car occ comp 0 et $0 : int)
        : int array -> snd 'a
huffman : int array -> 'c                              (où 'a = 'c * 'd)

On peut supposer que 'c = arbre.
**)


(*16*)
(*
Ça peut faire une erreur si file is empty ou si 'a n'est pas un type 'paire' (en plus des erreurs potentielles des fonctions impliquées)
**)



(*-------------------------------------------- IV. Tas de Braun --------------------------------------------*)


type 'a tas = E | N of 'a * 'a tas * 'a tas;;
let rec print_tas t = match t with E -> Printf.printf "E" | N(n,t1,t2) -> Printf.printf "N(%d," n; print_tas t1; Printf.printf ","; print_tas t2; Printf.printf ")";;
let rec taille t = match t with E -> 0 | N(_,t1,t2) -> 1 + (taille t1) + (taille t2);;
let ta = N(3, N(4,N(8,E,E),N(5,E,E)),N(6,N(7,E,E),E));;


(*17*)
(*
L'élément minimal d'un tas de Braun est à la racine (conséquence directe de sa définition).
L'élément maximal, quant à lui, n'a pas de position prédéfinie, mais on sait que c'est l'une des feuilles de l'arbre.
**)


(*18*)
(*
Il y a 2 tas de Braun de taille 2 :  N(n,m,E)
Il y a 5 tas de Braun de taille 3 :  N(n,m,N(p,E,E))
Il y a 14 tas de Braun de taille 4 : N(n,m,N(p,N(q,E,E),E))
Il y a 42 tas de Braun de taille 5 : N(n,m,N(p,N(q,N(r,E,E),E),E))
Il y a 132 tas de Braun de taille 6 : N(n,m,N(p,N(q,N(r,N(s,E,E),E),E),E))
IL y a 429 tas de Braun de taille 7 : N(n,m,N(p,N(q,N(r,N(s,N(t,E,E),E),E),E),E))
(C'est les nombres de Catalan, OEIS A000108)
**)


(*19*)
(*
La hauteur d'un arbre de Braun de taille n est égale à ⌊log2(n+1)⌋ puisque permanentement équilibré.
**)


(*20*)
let rec ajoute e tas = match tas with E -> N(e,E,E) | N(n,t1,t2) -> if e<=n then N(e,ajoute n t2, t1) else N(n,ajoute e t2, t1);;

Printf.printf "ajoute 2 ta : "; print_tas (ajoute 2 ta); Printf.printf "\n";;


(*21*)
let ta2 = N(2, N(4,N(8,E,E),E),N(6,N(7,E,E),E));;
Printf.printf "ajoute 3 ta2 : "; print_tas (ajoute 3 ta2); Printf.printf "\n";;


(*22*)
(*
Soit n la taille d'un tas de Braun, t = N(y,t1,t2) et soit x un élément quelconque de type 'a.
On pose donc H(n) : "t'=ajoute(x,t) est de taille n+1"
Initialisation : n=0 -> t=E, ajoute(x,E) = N(x,E,E) qui est de taille 1, donc H(0) est vraie.

Héréditée : Prenons n et supposons H(n) vraie. Prouvons H(n+1).
On pose t'=ajoute(x,t) et on a soit :
  - t'=N(x,t1',t2), x<=y, avec t2 de taille n-1 (car t de taille n) et t1' de taille n par hypothèse de récurrence (on fait N(x,ajoute y t2,t1)) . On a donc taille(t') = taille(t1')+1 = n+1.
  - t'=N(y,t1',t2) avec t2 de taille n-1 (car t de taille n) et t1' de taille n par hypothèse de récurrence (on fait N(y,ajoute x t2, t1)). On a donc taille(t') = taille(t1')+1 = n+1.
Donc H(n+1) est vraie.

Ainsi, t'=ajoute(_,t) renvoie bien un arbre de taille n+1.

Maintenant, on va prouver que t' est aussi un tas de Braun.
On veut que t' soit un arbre binaire, équilibré et ordonné.
 - Arbre binaire : évident (ajoute ne renvoie que des éléments sous la forme N(x,t1,t2), forme d'un arbre binaire)
 - Ordonné : puisque l'on compare l'élément à ajouter avec l'élement actuellement présent dans le tas à cette position et on l'ajoute à cet endroit ssi il est plus petit, sinon on continue, ll sera donc inséré au bon endroit. Et puisque l'arbre de départ est ordonnée, on le reste.
 - Équilibré : d'après l'héréditée, t'=N(_,t1',t2) où t1' est de taille n et t2 de taille n-1. Leur différence de taille n'est que de 1 donc t' reste équilibré.
On a vérifié toutes les critères de la définition, donc t' est un tas de Braun. 

Ainsi, ajoute nous renvoie bien un tas de Braun de taille n+1.
**)


(*23*)
let rec extrait_gauche (tas: 'a tas) : 'a option * 'a tas =
  match tas with
  | E -> (None, E)
  | N(n,E,t2) -> (Some(n), t2)
  | N(n,t1,t2) -> let tmp = extrait_gauche t1 in (fst tmp, N(n,t2,snd tmp))
;;

let ex = extrait_gauche ta in Printf.printf "extrait_gauche ta : %d " (match fst ex with None -> -1 | Some(n) -> n); print_tas (snd ex); Printf.printf "\n";;


(*24*)
(*
- arbre binaire : par définition (N(_,t1,t2) est un arbre binaire), fusion renvoie un arbre binaire.
- ordonné : 
  • fusion(a,E) -> renvoie a, n'est ordonné que ssi a est ordonné (vrai car a tas de Braun, précondition)
  • fusion(N(n_a,t_a1,t_a2),N(n_b,t_b1,t_b2)) -> renvoie N(n_a,N(n_b,t_b1,t_b2),fusion t_a1 t_a2) si n_a<=n_b, ordonné ssi fusion t_a1 t_a2 est ordonné
  • N(n_b,fusion t_b1 t_b2,N(n_a,t_a1,t_a2)) sinon, ordonné ssi fusion t_b1 t_b2 est ordonné
  On pourrait facilement prouver par une récurrence structurelle que fusion est ordonné (on a notre cas de bas et notre héréditée déjà faites)
- équilibré : On choisis A de taille n et B de taille n ou n-1
  • fusion(a,E) -> renvoie a, ordonné ssi a est ordonné (vrai car a tas de Braun, précondition)
  • fusion(N(n_a,t_a1,t_a2),N(n_b,t_b1,t_b2)) -> renvoie N(n_a,N(n_b,t_b1,t_b2),fusion t_a1 t_a2) si n_a<=n_b, N(n_b,t_b1,t_b2) est de taille n ou n-1 car B de taille n-1, fusion t_a1 t_a2 est de taille n-1 par récurrence => équilibré
  • N(n_b,fusion t_b1 t_b2,N(n_a,t_a1,t_a2)) sinon, fusion t_b1 t_b2 est de taille n-1 ou n-2 par récurrence, N(n_a,t_a1,t_a2) est de taille n => PROBLEME car n-2 est à plus de 1 de n

Cette définition de la fusion ne marche donc pas forcément.


Exemple de fusion de deux tas de Braun
t1 = N(1,N(2,E,E),N(3,N(4,E,E),E))
t2 = N(5,N(6,E,E),N(7,E,E))

1) 1<5 -> N(1, N(5,N(6,E,E),N(7,E,E)), fusion N(2,E,E) N(3,N(4,E,E),E))
2) 2<3 -> N(2,N(3,N(4,E,E),E),E)
fusion t1 t2 = N(1,N(5,N(6,E,E),N(7,E,E)),N(2,N(3,N(4,E,E),E),E))
**)


(*25*)
let rec fusion t1 t2 =
  match t1, t2 with
  t1, E -> t1
  | N(n1,t11,t12),N(n2,t21,t22) when n1<=n2 -> N(n1, N(n2,t21,t22),fusion t11 t22) 
  | N(n1,t11,t12),N(n2,t21,t22) -> let f = fusion t21 t22 in let tt = N(n1,t11,t12) in if taille tt>taille f +1 then let tt' = extrait_gauche tt in  N(n2,ajoute (match fst tt' with None -> failwith "fusion : tas vide"| Some(c)-> c) f ,snd tt') else N(n2,f,tt)
  | _ -> failwith "fusion : les deux tas doivent être de taille proche"
;;

let rec fusion2 t1 t2 = (* devrait être la même fonction mais un peu plus lisible et performante car pas d'instruction de branchement et pas d'appel à taille*)
  match t1, t2 with
  t1, E -> t1
  | N(n1,t11,t12),N(n2,t21,t22) when n1<=n2 -> N(n1, N(n2,t21,t22),fusion t11 t22) 
  | N(n1,t11,t12),N(n2,t21,t22) -> let f = fusion t21 t22 in let tt = extrait_gauche (N(n1,t11,t12)) in  N(n2,ajoute (match fst tt with None -> failwith "fusion : tas vide"| Some(c)-> c) f , snd tt)
  | _ -> failwith "fusion : les deux tas doivent être de taille proche"
;;

let ta2 = N(1, N(7,N(34,E,E),E),N(18,N(21,E,E),E));;
let ta3 = N(12,N(13,E,E),N(14,E,E));;
Printf.printf "fusion ta et ta2 : "; print_tas (fusion ta ta3); Printf.printf "\n";;
Printf.printf "fusion ta et ta3 : "; print_tas (fusion2 ta ta3); Printf.printf "\n";;