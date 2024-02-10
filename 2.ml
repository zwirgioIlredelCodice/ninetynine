(*Find the last but one (last and penultimate) elements of a list. (easy)*)

let rec last_two = function
  | [] -> None
  | [x] -> None
  | [a; b] -> Some (a, b)
  | _ :: t -> last_two t

(*sol*)
let rec last_two = function
  | [] | [_] -> None
  | [a; b] -> Some (a, b)
  | _ :: t -> last_two t 

let () = assert ((last_two ["a"; "b"; "c"; "d"]) = Some ("c", "d"))
let () = assert ((last_two ["a"]) = None)