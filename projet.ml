type coordinate = int * int;;
type importance = int ;;
type puzzle = (coordinate * importance) list;;
type bridge = {isVertical : bool; isDoubled : bool};;
type cell = Nothing | Island of importance | Bridge of bridge;;
type solution = cell list list;;

let puzzle = [((0,1), 2); ((1,0), 3); ((1,1), 8); ((1,2), 4); ((2,0), 3); ((2,1), 5); ((2,2), 3)];;

(* Cette fonction a pour but de definir la hauteur du puzzle *)
let hauteur_puzzle = fun p ->
  let rec aux = fun max p ->
  match p with 
  |[] -> max
  |h::t -> if (fst (fst h)) > max then aux (fst (fst h)) t else aux max t
  in (aux 0 p) + 1;;
  hauteur_puzzle puzzle;;

  (* Cette fonction a pour but de definir la largeur du puzzle *)
  let largeur_puzzle = fun p ->
  let rec aux = fun max p ->
  match p with 
  |[] -> max
  |h::t -> if (snd (fst h)) > max then aux (snd (fst h)) t else aux max t
  in (aux 0 p) + 1;;

  largeur_puzzle puzzle;;

  (* Cette fonction prend en parametre un puzzle et en fait un tableau 2D vide *)
  let puzzle_to_empty_tab = fun p ->
    let t = Array.make (hauteur_puzzle p) [||] in
      for i = 0 to ((hauteur_puzzle p) - 1) do
        let ligne = Array.make (largeur_puzzle p) 0 in
          for j = 0 to ((largeur_puzzle p) - 1) do ligne.(j) <- 0
          done ;
        t.(i) <- ligne
    done ; 
  t;;

  let t = puzzle_to_empty_tab puzzle;;

  (* Cette fonction a pour but de transformer un puzzle en tableau 2D d'entiers *)
let puzzle_to_tab = fun p t ->
  let rec aux = fun p t ->
    match p with 
    |[] -> t
    |a::b -> t.(fst(fst a)).(snd(fst a)) <- snd a; aux b t
  in aux p t;;

  let tab = puzzle_to_tab puzzle t;;


let solveur = fun tab -> 
  