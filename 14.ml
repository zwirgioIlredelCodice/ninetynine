(*14. Duplicate the elements of a list. (easy)*)

let rec duplicate = function
  | [] -> []
  | h :: t -> h :: h :: duplicate t

let () = assert ((duplicate ["a"; "b"; "c"; "c"; "d"]) = ["a"; "a"; "b"; "b"; "c"; "c"; "c"; "c"; "d"; "d"])
