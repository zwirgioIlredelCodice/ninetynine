(*Write a function last : 'a list -> 'a option 
that returns the last element of a list. (easy)*)

let rec last lst = 
  match lst with
  | [] -> None
  | h :: t ->
    let r = last t in match r with
    | None -> Some h
    | Some x -> Some x

(*sol*)
let rec last = function
 | [] -> None
 | [x] -> Some x
 | _ :: t -> last t

let () = assert ((last ["a" ; "b" ; "c" ; "d"]) = Some "d")
let () = assert ((last []) = None)