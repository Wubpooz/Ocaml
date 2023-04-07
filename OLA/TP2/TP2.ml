(*------------------------------------- Exercice 1 -------------------------------------*)

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
    | Bin(Imp,f1,f2) -> Bin(Or,Not(elim_imp f1),elim_imp f2)
    | Bin(op,f1,f2) ->Bin(op,elim_imp f1,elim_imp f2)
;;

print_fmla (elim_imp (Bin(And,Bin(Imp,Var(5),Var(3)),False)));; Printf.printf "\n";;


(*3*)
let rec is_nnf f =
  match f with
    True | False | Var(_) | Not(Var(_)) -> true
  | Bin((And|Or),f1,f2) -> is_nnf f1 && is_nnf f2
  | _ -> false
;;

Printf.printf "%b\n" (is_nnf (Not(Bin(And,Bin(Imp,Var(5),Var(3)),False))));;


(*4*)
let rec neg_nnf f =
  match f with
    True -> False
    | False -> True
    | Var(x) -> Not(Var(x))
    | Not(nf) -> nf
    | Bin(op,f1,f2) -> (match op with 
                        | And -> Bin(Or,neg_nnf f1,neg_nnf f2) 
                        | Or -> Bin(And,neg_nnf f1, neg_nnf f2) 
                        | Imp -> Bin(Imp,f1,f2) (*SHOULD NEVER HAPPEN*))
;;

print_fmla (neg_nnf (Bin(And,Bin(Or,Not(Var(5)),Var(3)),False)));; Printf.printf "\n";;


(*5*)
(*let nnf f = neg_nnf (neg_nnf (elim_imp f));;    shouldn't work bcs neg_nnf works iff f is nnf ?*)
(*Alt_version, working ?
let rec propagate_not f = (*without Imp*)
  match f with
  True -> True
  | False -> False
  | Var(x) -> Var(x)
  | Not(nf) -> (match nf with 
                  Var(x) -> Not(Var(x)) 
                  | True  -> False
                  | False -> True
                  | Not(f) -> propagate_not (Not(f))
                  | Bin(op,f1,f2) -> if op=And then Bin(Or, propagate_not (Not(f1)),propagate_not (Not(f2))) else Bin(And,propagate_not (Not(f1)),propagate_not (Not(f2))))
  | Bin(op,f1,f2) -> Bin(op, propagate_not f1, propagate_not f2)
;;
let nnf = propagate_not (elim_imp f));; *)

let rec nnf f = (*HAVE TO TEST IF COMPILES *)
  match f with 
  | True -> True | False -> False | Var(x) -> Var(x) 
  | Not(nf) -> neg_nnf (nnf f) (*bcs f is now in nnf*) 
  | Bin(Imp,f1,f2) -> Bin(Or,neg_nnf (nnf f1), nnf f2)
  | Bin(op,f1,f2) -> Bin(op,nnf f1,nnf f2) 
;;

Printf.printf "%b\n" (is_nnf (nnf(Not(Bin(And,Bin(Imp,Var(5),Var(3)),False)))));;

(*6*)
(*complexité en 2*neg_nnf + elim_imp = 2*O(n) + O(n) = O*)


(*6*)
(*complexité en 2*neg_nnf + elim_imp = 2*O(n) + O(n) = O(n) *)



(*------------------------------------- Exercice 2 -------------------------------------*)

let s_not f = match f with
  True -> False
  | False -> True
  | Not f' -> f'
  | Var(x) -> Var(x)    (*l'énoncé se plante ?*)
  | _ -> Not f

(*1*)
let s_and f1 f2 =
  match f1,f2 with
    True, True -> True
    | False, _ | _, False -> False
    | True, _ -> f2
    | _, True -> f1
    | _ -> Bin(And,f1,f2)
;;


(*2*)
let s_or f1 f2 = 
  match f1,f2 with
  False, False -> False
  | True, _ | _, True -> True
  | False, _ -> f2
  | _, False -> f1
  | _ -> Bin(Or,f1,f2)
;;

let s_imp f1 f2 =
  match f1, f2 with
  False,_ | True,True -> True
  | Var(x),_ -> Var(x)  (*l'énoncé se plante ?*)
  | _ -> Bin(Imp,f1,f2)
;;

print_fmla (Bin ( Imp , Not ( Var 0) , Bin ( And , Var 1, False )));; Printf.printf "\n";;
print_fmla (s_imp ( s_not ( Var 0)) ( s_and ( Var 1) False ));; Printf.printf "\n";;


(*3*)
let s_iff f1 f2 = s_and (s_imp f1 f2) (s_imp f2 f1);;
let s_xor f1 f2 = s_or (s_and f1 (s_not f2)) (s_and (s_not f1) f2);;


(*4*)
let rec simplify f =
  match f with
    True -> True
    | False -> False
    | Var(x) -> Var(x)
    | Not(f) -> s_not (simplify f)
    | Bin(op,f1,f2) -> let sf1=simplify f1 in let sf2=simplify f2 in match op with And -> s_and sf1 sf2 | Or -> s_or sf1 sf2 | Imp -> s_imp sf1 sf2
;;

print_fmla (simplify (Bin ( Imp , Not ( Var 0) , Bin ( And , Var 1, False ))));; Printf.printf "\n";;
print_fmla (simplify (Bin(And,Bin(Or,Not(Var(5)),Var(3)),False)));; Printf.printf "\n";;


(*------------------------------------- Exercice 3 -------------------------------------*)

(*1*)
type exp = CST of int | VAR of int | UMINUS of exp | PLUS of exp*exp | MINUS of exp*exp | TIMES of exp*exp (*int of VAR is the indice*)

(*2*)
let rec constant e =
  match e with
  CST(_) -> true
  | VAR(_) -> false
  | UMINUS(e) -> constant e
  | PLUS(e1,e2) | MINUS(e1,e2) | TIMES(e1,e2) -> constant e1 && constant e2
;;


(*3*)
let rec eval e arr =
  match e with
  CST(x) -> x
  | VAR(i) -> arr.(i)
  | UMINUS(e) -> -(eval e arr)
  | PLUS(e1,e2) -> (eval e1 arr)+(eval e2 arr)
  | MINUS(e1,e2) -> (eval e1 arr)-(eval e2 arr)
  | TIMES(e1,e2) -> (eval e1 arr)*(eval e2 arr)
;;

Printf.printf "%d\n" (eval (TIMES(CST(3),PLUS(VAR(1),UMINUS(VAR(0)))))  [|1;2;3|]);; (*  3* (2+ -1) *)


(*------------------------------------- Exercice 4 -------------------------------------*)

type regex = Empty | Eps | Char of char | Alt of regex*regex | Concat of regex*regex | Star of regex
let rec print_regex e = 
  match e with 
    Empty -> ()
    | Eps -> Printf.printf "ɛ" 
    | Char(c) -> Printf.printf"%c" c 
    | Alt(e1,e2) -> Printf.printf "("; print_regex e1; Printf.printf "|";print_regex e2; Printf.printf ")";
    | Concat(e1,e2) -> print_regex e1; print_regex e2;
    | Star(e) -> Printf.printf "("; print_regex e; Printf.printf ")*";
;;

(*1*)
let rec espilon exp=
  match exp with
    Empty -> false
    | Eps -> true
    | Char(_) -> false
    | Star(_) -> true
    | Alt(e1,e2) -> espilon e1 || espilon e2
    | Concat(e1,e2) -> espilon e1 || espilon e2
;;

Printf.printf "%b\n" (espilon (Concat(Alt(Char('a'),Char('b')),Star(Char('c')))));;


(*2*)
let rec first a exp =
  match exp with
    Empty -> failwith "Empty"
    | Eps -> false
    | Char(c) -> c=a
    | Star(e) -> first a e
    | Alt(e1,e2) -> first a e1 || first a e2
    | Concat(e,_) -> first a e
;;

let rec last a exp =
  match exp with
    Empty -> failwith "Empty"
    | Eps -> false
    | Char(c) -> c=a
    | Star(e) -> last a e
    | Alt(e1,e2) -> last a e1 || last a e2
    | Concat(_,e) -> last a e
;;

print_regex (Concat(Alt(Char('a'),Char('b')),Star(Char('c'))));; Printf.printf "\n";
Printf.printf "%b\n" (first 'b' (Concat(Alt(Char('a'),Char('b')),Star(Char('c')))));;
Printf.printf "%b\n" (last 'b' (Concat(Alt(Char('a'),Char('b')),Star(Char('c')))));;


(*3*)
let rec follow a b exp =
  match exp with
    Empty -> failwith "Empty"
    | Eps -> true
    | Char(c) -> false
    | Star(e) -> follow a b e
    | Alt(e1,e2) -> follow a b e1 || follow a b e2
    | Concat(e1,e2) -> (last a e1 && first b e2) || follow a b e1 || follow a b e2
;;

let rec collapse_star_to_eps exp = match exp with
  Empty -> Empty
  | Eps -> Eps
  | Char(c) -> Char(c)
  | Star(e) -> Eps
  | Alt(e1,e2) -> Alt(collapse_star_to_eps e1, collapse_star_to_eps e2)
  | Concat(e1,e2) -> Concat(collapse_star_to_eps e1, collapse_star_to_eps e2)
;;
let follow a b exp = follow a b exp || follow a b (collapse_star_to_eps exp);; (*takes into acocunt that star->eps is right*)

print_regex (Concat(Char('d'),Concat(Alt(Char('a'),Char('b')),Concat(Star(Concat(Star(Char('f')),Star(Char('c')))),Char('e'))))); Printf.printf "\n";
print_regex (collapse_star_to_eps (Concat(Char('d'),Concat(Alt(Char('a'),Char('b')),Concat(Star(Concat(Star(Char('f')),Star(Char('c')))),Char('e')))))); Printf.printf "\n";
Printf.printf "%b\n" (follow 'a' 'c' (Concat(Char('d'),Concat(Alt(Char('a'),Char('b')),Concat(Star(Concat(Star(Char('f')),Star(Char('c')))),Char('e'))))));;


(*4*)


(*5*)
let rec derive exp a = 
  match exp with
    Empty -> Empty
    | Eps -> Empty
    | Char(c) -> if c=a then Eps else Empty
    | Star(e) -> Concat(derive e a,e)
    | Alt(e1,e2) -> Alt(derive e1 a,derive e2 a)
    | Concat(e1,e2) -> if espilon e1 then Alt(Concat(derive e1 a,e2),derive e2 a) else Concat(derive e1 a,e2)
;;

print_regex (derive (Alt(Concat(Char('a'),Concat(Char('b'),Char('c'))), Alt(Concat(Char('a'),Char('d')),Alt(Concat(Char('e'),Concat(Char('f'),Char('g'))),Char('a'))))) 'a');; Printf.printf "\n";;
(*result is not as expected (bc|d|eps) so I guess it's wrong but can't figure out why*)


(*6*)
let rec accept exp s = 
  match s with
    [] -> espilon exp
    | a::s -> accept (derive exp a) s
;;

Printf.printf "%b\n" (accept (Alt(Concat(Char('a'),Concat(Char('b'),Char('c'))), Alt(Concat(Char('a'),Char('d')),Alt(Concat(Char('e'),Concat(Char('f'),Char('g'))),Char('a'))))) ['a';'b';'c']);;

