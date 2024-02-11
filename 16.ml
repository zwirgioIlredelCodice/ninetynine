(*Drop every N'th element from a list. (medium)*)
let drop lst n =
  let rec aux lst nn acc =
    match lst with
    | [] -> acc
    | h :: t -> if nn = 1 then aux t n acc else aux t (nn-1) (h :: acc)
  in List.rev (aux lst n [])

let () = assert ((drop ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 3) = ["a"; "b"; "d"; "e"; "g"; "h"; "j"])