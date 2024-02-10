(*8. Eliminate consecutive duplicates of list elements. (medium)*)

let compress lst =
  let rec aux acc = function
    | [] -> acc
    | h :: t -> match acc with
      | [] -> aux (h :: acc) t
      | x :: _ -> if x = h then aux acc t else aux (h :: acc) t
  in List.rev (aux [] lst) 

(*sol*)
let rec compress = function
  | a :: (b :: _ as t) -> if a = b then compress t else a :: compress t
  | smaller -> smaller

let () = assert ((compress ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"]) = ["a"; "b"; "c"; "a"; "d"; "e"])