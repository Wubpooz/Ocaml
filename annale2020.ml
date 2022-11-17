

(*II.*)
(*1*)

let rec log2 n =
  if n<=0 then failwith "error, Unbound log2"
  else if n=1 then 0 else 1+(log2 (n/2))
;;

Printf.printf "%d\n" (log2 8);;


(*2*)
let rec somme f n =
  if n<=0 then f 0
  else (f n) + (somme f (n-1))
;;

Printf.printf "%d\n" (somme (fun x->x*x) 4);;


(*3*)
let sous_liste i j l =
  let rec loop k l =
    match l with
    [] -> []
    | e :: s -> if i<=k && j>=k then e::(loop (k+1) s) else loop (k+1) s
  in
    loop 0 l
;;

List.iter (fun x -> Printf.printf "%d " x) (sous_liste 1 3 [0;1;2;3;4;5]);; Printf.printf "\n";;


(*4*)
let zip l1 l2 =
  let rec loop l1 l2 =
    match l1,l2 with
    | [],[] -> []
    | _::_, [] | [], _::_ -> failwith "erreur"
    | e1::s1, e2::s2 -> e1::e2::(loop s1 s2)
  in
    loop l1 l2
;;

List.iter (fun x -> Printf.printf "%d " x) (zip [ 1;2;3;4;5] [10;11;12;13;14]);; Printf.printf "\n";;



(*III.*)

type elem = BO of string * (string*string) list | BF of string | T of string;;
type doc = elem list;;


(*1*)
let docu = [BO ("xx", []);
  BO ("yy", [("bar","42")]);  T ("Hello, world!");
  BF ("yy");
BF("xx")]


(*2*)
let pr_att_list l =
  List.iter (fun (a,b)-> Printf.printf "%s='%s' " a b) l;
;;

pr_att_list [("a", "25"); ("id", "toto"); ("foo", "bar")];; Printf.printf "\n";;


(*3*)
let pr_elem e =
  match e with
  | T (s)-> Printf.printf "%s\n" s
  | BF (s)-> Printf.printf "</%s>\n" s
  | BO (s,att)-> Printf.printf "<%s " s; pr_att_list att; Printf.printf ">\n";
;;


(*4*)
let pr_doc d =
  List.iter pr_elem d
;;

pr_doc docu;;


(*5*)
let verif_att_list l =
  let compare_fst (a,b) (c,d) = compare a c in
  let l_sorted = List.sort compare_fst l in
  let rec loop l =
    match l with
    [] -> true
    |e :: f :: s -> if compare_fst e f =0 then false else loop s
    |impair -> true
  in
    loop l_sorted
;;


(*6*)
let valide d =
  let rec loop d =
    match d with
    [] -> true
    | e:: s -> if not (verif_att_list e) then false else
