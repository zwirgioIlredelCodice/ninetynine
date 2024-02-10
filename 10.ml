(*9. Pack consecutive duplicates of list elements into sublists. (medium)*)

let encode lst =
  let rec aux acc = function
    | [] -> acc
    | h :: t -> match acc with
      | [] -> aux [(1, h)] t
      | (n, x) :: tt -> if x = h then aux ((n+1, x) :: tt) t else aux ((1, h) :: acc) t
  in List.rev (aux [] lst)

(*sol*)
let encode list =
    let rec aux count acc = function
      | [] -> [] (* Can only be reached if original list is empty *)
      | [x] -> (count + 1, x) :: acc
      | a :: (b :: _ as t) -> if a = b then aux (count + 1) acc t
                              else aux 0 ((count + 1, a) :: acc) t in
    List.rev (aux 0 [] list)

let () = assert ((encode ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"]) = [(4, "a"); (1, "b"); (2, "c"); (2, "a"); (1, "d"); (4, "e")])