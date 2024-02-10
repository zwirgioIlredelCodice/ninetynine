(*Find the number of elements of a list. (easy)*)

let rec length = function
  | [] -> 0
  | _ :: t -> 1 + (length t)

(*tail recursive*)

let rec length_aux n = function
  | [] -> n
  | _ :: t -> length_aux (n+1) t

let length lst = length_aux 0 lst

(*sol*)
let length list =
  let rec aux n = function
    | [] -> n
    | _ :: t -> aux (n + 1) t
  in aux 0 list;;

let () = assert ((length ["a"; "b"; "c"]) = 3)
let () = assert ((length []) = 0)