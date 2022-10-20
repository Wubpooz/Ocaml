
(*1*)
let hanoi n =
  let rec hanoi_aux dep mil arr n =
    if n>0 then begin
      hanoi_aux dep arr mil (n-1);
      Printf.printf "%s -> %s\n" dep arr;
      hanoi_aux mil dep arr (n-1)
    end
  in
  hanoi_aux "dep" "mid" "arr" n
;;

(*2*)
type piquet = string * int list;;
type jeu = piquet list;;

(*3*)
let affiche_piquet pic =
  Printf.printf "%s|" (fst pic);
  List.iter (fun i -> Printf.printf "%d-" i)(List.rev (snd pic));
  Printf.printf "\n";
;;

(*
affiche_piquet ("mid", [1;2;5]);;
*)


(*4*)
let choix_piquet piquets s = (* (s, List.assoc s piquets) ;;*)
  (*let rec trouve lst nom =
    match piquets with
      [ ] -> failwith "Pile invalide"
      | (name, stack) :: ll -> if name==s then (name,stack) else trouve ll s
  in
  trouve piquets s*)
  match piquets with
    [] -> failwith "Pile invalide"
    | (c,nbs) when c==s -> (c,nbs)
;;

let pic = [ ("dep", [2;3;4]);("mid", []);("arr", [1])];;

affiche_piquet (choix_piquet pic "mid");;