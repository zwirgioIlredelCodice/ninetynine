(*11. Modified run-length encoding. (easy)*)

type 'a rle =
  | One of 'a 
  | Many of int * 'a

  let encode lst =
    let rec aux acc = function
      | [] -> acc
      | h :: t -> match acc with
        | [] -> aux [(One h)] t
        | One x :: tt -> if x = h then aux (Many (2, x) :: tt) t else aux (One h :: acc) t
        | Many (n, x) :: tt -> if x = h then aux (Many (n+1, x) :: tt) t else aux (One h :: acc) t
    in List.rev (aux [] lst)


let () = assert ((encode ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"]) = [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d";
Many (4, "e")])