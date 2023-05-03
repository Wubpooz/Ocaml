(*
-------------------------------------- Fractions -------------------------------------- *)

(*1*)
let rec pgcd a b = 
  if b<>0 then a
  else pgcd a (a mod b)
;; (* récursive terminale car évalue pgcd a (a mod b) en dernier *)


(*2*)
type frac = { num : int ; denom : int};;

(*3*)
let sign i =
  if i>0 then 1
  else if i<0 then -1
  else 0
;;

(*4*)
let simp f =
  let num, denom =
    if sign f.num * sign f.denom >=0 then
        abs f.num, abs f.denom
    else
      -(abs f.num), (abs f.denom)
  in
  let p = pgcd (abs num) denom in
    {num = f.num/p ; denom = f.denom/p}
;;


(*5*)

let frac a b = simp {num = a ; denom = b};;

let add_frac f1 f2 = simp {num = f1.num*f2.denom + f2.num*f1.denom ; denom = f2.denom*f1.denom};;

let neg_frac f = {num = -f.num ; denom = f.denom};;

let sub_frac f1 f2 = add_frac f1 (neg_frac f2);;

let mul_frac f1 f2 = simp {num = f1.num*f2.num ; denom = f1.denom*f2.denom};;

let inv_frac f = {num = f.denom; denom = f.num};;

let  div_frac f1 f2 = mul_frac f1 (inv_frac f2);;

let string_of_frac f = (string_of_int f.num) ^ "/" ^ (string_of_int f.denom);;

let float_of_frac f = (float f.num)/.(float f.denom);;


(*Test:

let test_frac () =
  Printf.printf "%s \n" (string_of_frac (frac 2 10));
  Printf.printf "%s \n" (string_of_frac (frac 5 10));
  Printf.printf "%s \n" (string_of_frac (frac 100 20));
  Printf.printf "%s \n" (string_of_frac (add_frac (frac 2 10) (frac 4 10)));
  Printf.printf "%s \n" (string_of_frac (frac (-2) 10));
  Printf.printf "%s \n" (string_of_frac (frac (-5) (-10)));
  Printf.printf "%s \n" (string_of_frac (frac 100 (-20)));
  Printf.printf "%s \n" (string_of_frac (mul_frac (frac (-2) 10) (frac (-4) 10)))


let () = test_frac ()

*)



(*
-------------------------------------- Nombres -------------------------------------- *)

type num = Int of int | Float of float | Frac of frac;;

(*1*)
let string_of_num n =
  match n with
    Int i -> string_of_int i
    | Float f -> string_of_float f
    | Frac ff -> string_of_frac ff
;;

(*2,3*)
let exec_op n1 n2 op_i op_fr op_fl = (* exec_op : num -> num -> (int->int) -> (frac->frac) -> (float->float)-> num*)
  match n1, n2 with 
  | Float f1, Float f2 -> Float (op_fl f1 f2)
  | Float f1, Frac ff2 -> Float (op_fl f1 (float_of_frac ff2))
  | Float f1, Int i2 -> Float (op_fl f1 (float_of_int i2))
  | Frac ff1, Float f2 -> Frac (op_fr ff1 (frac (int_of_float f2) 1))
  | Frac ff1, Frac ff2 -> Frac (op_fr ff1 ff2)
  | Frac ff1, Int i2 -> Frac (op_fr ff1 (frac i2 1))
  | Int i1, Float f2 -> Int (op_i i1 (int_of_float f2))
  | Int i1, Frac ff2 -> Int (op_i i1 (int_of_float (float_of_frac ff2)))
  | Int i1, Int i2 -> Int (op_i i1 i2)
;; (* On a pris l'initiative de faire prévaloir le type de n1 sur le résultat et pas de prendre à chaque fois 
le type le plus "précis" (souvent float) *)


(*4*)

let add_num n1 n2 = exec_op n1 n2 (+) (add_frac) (+.);;

let sub_num n1 n2 = exec_op n1 n2 (-) (sub_frac) (-.);;

let mul_num n1 n2 = exec_op n1 n2 ( *) (mul_frac) ( *.);;

let div_num n1 n2 = exec_op n1 n2 (/) (div_frac) (/.);;


(*5*)

let rec pow n k = 
  if k==1 then n
  else mul_num n (pow n (k-1))
;; 