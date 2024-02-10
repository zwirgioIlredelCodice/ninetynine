(*7. Flatten a nested list structure. (medium)*)
(* There is no nested list type in OCaml, so we need to define one
   first. A node of a nested list is either an element, or a list of
   nodes. *)

type 'a node =
  | One of 'a
  | Many of 'a node list

let rec flatten = function
  | [] -> []
  | One h :: t -> h :: (flatten t)
  | Many h :: t -> List.concat [(flatten h); (flatten t)]

(*sol*)
let flatten list =
  let rec aux acc = function
    | [] -> acc
    | One x :: t -> aux (x :: acc) t
    | Many l :: t -> aux (aux acc l) t in
  List.rev (aux [] list)

let () = assert ((flatten [One "a"; Many [One "b"; Many [One "c" ;One "d"]; One "e"]]) = ["a"; "b"; "c"; "d"; "e"])
