let rec sum_if f n =
  let rec loop f n acc =
    if n=0 then acc
    else if f n then loop f (n-1) (acc+n) else loop f (n-1) acc
  in 
    loop f n 0
;;



let dupl l =
  let rec loop l acc =
    match l with
    |[] -> acc
    | e :: s -> loop s (acc @ [e;e])
  in
    loop l []
;;

(*
List.iter (fun x-> Printf.printf "%d " x)(dupl [1;2;3]);; Printf.printf "\n" *)


let total_len l =
  let rec loop l acc = 
    match l with
    |[] -> acc
    |e::s -> loop s (acc+String.length e)
  in 
    loop l 0
;;

(*
Printf.printf "%d\n" (total_len ["a";"aa";""]);; *)

(*
let rotate l = 
  let head l = match l with |[] -> failwith "No head" |x::_ -> x in
  let h = head l in 
  let rec loop l acc =
    match l with
    |[] -> acc
    | e::s -> if e==h then loop s (e::acc) else loop s ([e]@acc)
  in
    loop l []
;;*)

let rotate l =
  let head l = match l with |[] -> failwith "No head" |x::_ -> x  in
  let h = head l in
  let rec loop l acc =
    match l with
    [] -> acc
    | e::s -> if e==h then loop s (acc@[e]) else loop s (e::acc)
  in
    loop (List.rev l) []  (* write it without List *)
;;

List.iter (fun x-> Printf.printf "%s " x) (rotate ["A";"B";"C";"D"]);;




type event = { date : int; len : int; text : string};;
type calendar = event list;;
let string_of_date n =    (*A FAIRE*)
  string_of_int n;;


let string_of_len n =
  let h = n mod 3600 in 
  let m = (n- h*3600) mod 60 in
  let s = (n- h*3600 -m*60) in
  if h>0 && m>0 then (string_of_int h )^"h "^(string_of_int m)^"m "^(string_of_int s)^"s"
  else if h=0 && m>0 then (string_of_int m)^"m "^(string_of_int s)^"s" else if h>0 && m=0 then (string_of_int h)^"h "^(string_of_int s)^"s" else string_of_int s ^"s"
;;

let string_of_event e =
  string_of_date e.date ^" "^string_of_len e.len ^" "^e.text;;

let pr_event c =
  List.iter (fun x-> Printf.printf "%s\n" (string_of_event x)) c;;

let compare_event e1 e2 =
  compare e1.date e2.date
;;

let sort_calendar c =
  List.sort compare_event c;;

let rem_overlap c =
  let c_sorted = sort_calendar c in
  let rec loop l acc =
    match l with
      []->acc
      |e1::e2::s -> if compare_event e1 e2 =0 then loop s (e1::acc) else loop s (e1::(e2::acc))
      |e::s -> loop s (e::acc)
    in
    loop c_sorted []
;;

let merge_event c =
  let c_sorted = sort_calendar c in
  let rec loop l acc =
    match l with
    [] -> acc
    |e1::e2::s -> if compare_event e1 e2 !=0 then loop s (e1::(e2::acc)) else if e1.text!=e2.text then loop s (e1::acc) 
    else loop s ({date = e1.date; len =e1.len+e2.len - (e2.date - e1.date); text = e1.text}::acc)
    | e::s -> loop s (e::acc)
  in
    loop c_sorted []
;;

(*WRITE TESTS*)
