(*
--------------------------------------- E/S -----------------------------*)

(*
let existe = 
    if Array.length(Sys.argv)<2 then "Pas assez d'arguments"
    else if Sys.file_exists Sys.argv.(1) then "Le fichier existe"
    else if Sys.is_directory Sys.argv.(1) then "Le répertoire existe"
    else "Aucun fichier ou répertoire correspondant"
;;

Printf.printf "%s\n" existe;;
*)



(*
--------------------------------------- RECURSIVES -----------------------------*)

(*
let rec somme_entier n =
    if n==0 then 
        0
    else 
        n + somme_entier (n-1)
;;
    
Printf.printf "Somme à 10 = %d\n" (somme_entier 10);;


let rec somme_carre n =
    if n==0 then 0
    else n*n + somme_carre(n-1)
;;

Printf.printf "∑ carré à 10 = %d\n" (somme_carre 10);;


let rec leibniz n =
    if n==0 then
        1
    else (-1)**n * 1/(2*n+1);;
    
Printf.printf "approx π/4 au 100e = %f\n" (leibniz 100);;
*)



(*
--------------------------------------- DATES -----------------------------*)

(*
let bissextile a =
  if (a mod 400)==0 || ((a mod 100)==0 && (a mod 4)!=0) then true
  else false
;;(*type int->bool*)

let jour_mois m a =
  if (m mod 2)==0 && m!=2 then 30
  else if m==2 && bissextile a then 28
  else if m==2 then 27
  else 31
;;

let rec jour_date j m a = 
  if m==1 then j
  else jour_mois (m-1) a + jour_date j (m-1) a
;;

Printf.printf "days from january to 27 august en 2022 : %d\n" (jour_date 27 8 2022);;
*)




(*
--------------------------------------- HANOI -----------------------------*)

let rec hanoi_aux dep mil dest n =
  if n > 0 then begin
    hanoi_aux dep dest mil (n-1);
    Printf.printf "%s -> %s\n" dep dest;
    hanoi_aux mil dep dest (n-1);
  end
;;

let hanoi n = hanoi_aux "A" "B" "C" n;;

let t0 = Sys.time ();;
hanoi 10;;
let t1 = Sys.time ();;
Printf.printf "Dt écoulé : %f\n" (t1 -. t0);;