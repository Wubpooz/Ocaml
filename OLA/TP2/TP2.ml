(*------------------------------------- Exercice 1 -------------------------------------*)
(*
type binop = And | Or | Imp
type fmla = True | False | Var of int | Not of fmla | Bin of binop*fmla*fmla

let rec print_binop op = match op with And -> Printf.printf "And " | Or -> Printf.printf "Or " | Imp -> Printf.printf "=> ";; 
let rec print_fmla f = 
  match f with 
  True -> Printf.printf "true "
  | False -> Printf.printf "false "
  | Var(x) -> Printf.printf "%d " x
  | Not(nf) -> Printf.printf "Not( "; print_fmla nf; Printf.printf ") ";
  | Bin(op,f1,f2) -> Printf.printf "( "; print_fmla f1; print_binop op; print_fmla f2; Printf.printf") ";
;;


(*1*)
let rec contains i f = 
  match f with  
    True | False -> false
    | Var(x) -> x=i
    | Not(nf) -> contains i nf
    | Bin(_,f1,f2) -> contains i f1 || contains i f2
;;

Printf.printf "%b\n" (contains 5 ( Bin(And,Bin(Imp,Var(5),Var(3)),False) ) );;


(*2*)
let rec elim_imp f =
  match f with
    True -> True
    | False -> False
    | Var(x) -> Var(x)
    | Not(nf) -> Not(elim_imp nf)
    | Bin(op,f1,f2) -> if op=Imp then Bin(Or,Not(elim_imp f1),elim_imp f2) else Bin(op,elim_imp f1,elim_imp f2)
;;

print_fmla (elim_imp (Bin(And,Bin(Imp,Var(5),Var(3)),False)));; Printf.printf "\n";;


(*3*)
let rec is_nnf f =
  match f with
    True | False | Var(_) -> true
  | Not(nf) -> (match nf with Var(_) -> true | _ -> false)
  | Bin(_,f1,f2) -> is_nnf f1 && is_nnf f2
;;

Printf.printf "%b\n" (is_nnf (Not(Bin(And,Bin(Imp,Var(5),Var(3)),False))));;


(*4*)
let rec neg_nnf f =
  match f with
    True -> False
    | False -> True
    | Var(x) -> Var(x)
    | Not(nf) -> nf
    | Bin(op,f1,f2) -> (match op with 
                        | And -> Bin(Or,neg_nnf f1,neg_nnf f2) 
                        | Or -> Bin(And,neg_nnf f1, neg_nnf f2) 
                        | Imp -> Bin(Imp,f1,f2) (*SHOULD NEVER HAPPEN*))
;;

print_fmla (neg_nnf (Bin(And,Bin(Or,Not(Var(5)),Var(3)),False)));; Printf.printf "\n";;


(*5*)
let rec nnf f = neg_nnf (neg_nnf (elim_imp f));;

Printf.printf "%b\n" (is_nnf (nnf(Not(Bin(And,Bin(Imp,Var(5),Var(3)),False)))));;


(*6*)
(*complexité en 2*neg_nnf + elim_imp = 2*O(n) + O(n) = O(n) *)
*)










(*------------------------------------- Exercice 4 -------------------------------------*)

type regexp = Eps | Char of char | Alt of regexp*regexp | Concat of regexp*regexp | Star of regexp
let rec print_regex e = 
  match e with 
    Eps -> Printf.printf "ɛ" 
    | Char(c) -> Printf.printf"%c" c 
    | Alt(e1,e2) -> Printf.printf "("; print_regex e1; Printf.printf "|";print_regex e2; Printf.printf ")";
    | Concat(e1,e2) -> print_regex e1; print_regex e2;
    | Star(e) -> print_regex e; Printf.printf "*";
;;

(*1*)
let rec espilon exp=
  match exp with
    Eps -> true
    | Char(_) -> false
    | Star(_) -> true
    | Alt(e1,e2) -> espilon e1 || espilon e2
    | Concat(e1,e2) -> espilon e1 || espilon e2
;;

Printf.printf "%b\n" (espilon (Concat(Alt(Char('a'),Char('b')),Star(Char('c')))));;


(*2*)
let rec first a exp =
  match exp with
    Eps -> false
    | Char(c) -> c=a
    | Star(e) -> first a e
    | Alt(e1,e2) -> first a e1 || first a e2
    | Concat(e,_) -> first a e
;;

let rec last a exp =
  match exp with
    Eps -> false
    | Char(c) -> c=a
    | Star(e) -> last a e
    | Alt(e1,e2) -> last a e1 || last a e2
    | Concat(_,e) -> last a e
;;

print_regex (Concat(Alt(Char('a'),Char('b')),Star(Char('c'))));; Printf.printf "\n";
Printf.printf "%b\n" (first 'b' (Concat(Alt(Char('a'),Char('b')),Star(Char('c')))));;
Printf.printf "%b\n" (last 'b' (Concat(Alt(Char('a'),Char('b')),Star(Char('c')))));;


(*3*)
let rec follow a b exp =  (*faudrait faire un || avec l'expression où Star -> Eps*)
  match exp with
    Eps -> true (*bcs if two letters are separated by eps they are technicaly consecutively*)
    | Char(c) -> false
    | Star(e) -> follow a b e
    | Alt(e1,e2) -> follow a b e1 || follow a b e2
    | Concat(e1,e2) -> match e1,e2 with Char(c1),Char(c2) -> c1=a && c2=b |_ -> follow a b e1 || follow a b e2
;;


Printf.printf "%b\n" (follow 'a' 'c' (Concat(Char('d'),Concat(Alt(Char('a'),Char('b')),Star(Char('c'))))));;