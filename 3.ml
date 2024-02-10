(*Find the K'th element of a list. (easy)*)

let rec at x = function
  | [] -> None
  | h :: t -> if x = 1 then Some h else at (x-1) t

let () = assert ((at 3 ["a"; "b"; "c"; "d"; "e"]) = Some "c")
let () = assert ((at 3 ["a"]) = None)