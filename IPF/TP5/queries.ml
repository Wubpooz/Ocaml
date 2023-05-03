type movie = { 
    id : int;
    title : string;
    year : int;
    runtime : int;
    rank : int
}

type res = Movie of movie | Invalid | Eof

let input_movie in_c =
  try
    let s = input_line in_c in
    match String.split_on_char ';' s with
    [ s_id; title; s_year; s_runtime ; s_rank ] ->
       Movie ({
        id = int_of_string s_id;
        title = title;
        year = int_of_string s_year;
        runtime = int_of_string s_runtime;
        rank = int_of_string s_rank;
      })
    | _ -> Invalid

  with
   End_of_file -> Eof
  | _ -> Invalid
;;

let load_movies f =
  let in_c = open_in f in
  let rec loop in_c acc =
    match input_movie in_c with
    | Eof -> acc
    | Invalid -> loop in_c acc
    | Movie m -> loop in_c (m :: acc)
  in
    let res = loop in_c [] in
    close_in in_c;
    res
;;

let movies = load_movies "movies.csv";;


let pr_movie m = 
    Printf.printf "{ id=%d; title=%s; year=%d; runtime=%d; rank=%d }\n" m.id m.title m.year m.runtime m.rank;;
    
(*let m = {id=2004;title ="TGBU";year=1966;runtime=190;rank=5};;

pr_movie m;;*)


let pr_movies movies =
    List.iter pr_movie movies
;;

(*pr_movies movies;;*)
let moviesTop10 movies = 
  List.iter pr_movie (List.filter (fun x -> x.rank<=10) movies);; 


(*moviesTop10 movies;;*)

let movies1980 movies =
    List.filter (fun x -> x.year>=1980 && x.year<=1989) movies;;

(*pr_movies (movies1980 movies);;*)

let movie_titles movies =
    List.map (fun x -> x.title) movies;;

(*List.iter (fun x -> Printf.printf "%s\n" x) (movie_titles movies);; *)


let max_id movies =
    let maximum a b = if a>b then a else b in
    List.fold_left (fun max x -> maximum max x.id) 0 movies;;

(* Printf.printf "%d\n" (max_id movies);; *)

let average_runtime movies = 
  let sum_count = List.fold_left (fun sc x -> ((fst sc) + x.runtime, (snd sc)+1 ) ) (0,0) movies in
  (fst sum_count)/(snd sum_count);;

(* Printf.printf "%d\n" (average_runtime movies);; *)

let average_by_year movies =
    List.sort (fun x y -> x.year-y.year) movies in
    let rec loop movies acc = 
      match movies with
      | [] -> acc
      | (*------------------------*)
      | m -> if m.year == fst acc then loop movies ((fst acc), m :: (snd acc)) else loop movies ((m.year,m) :: acc)
      if acc == [] then (*--------------------*)
    in
      let mvs = loop movies acc in
      List.iter average_runtime mvs
;;

(*List.iter (fun x -> Printf.printf "%s : %d runtime\n" x (fst snd)) (average_by_year movies)*)