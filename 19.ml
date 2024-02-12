(*19. Rotate a list N places to the left. (medium)*)

let rotate lst n =
  let rec aux lst n acc =
    match lst with
    | [] -> acc
    | h :: t -> if n = 0 then lst @ (List.rev acc) else aux t (n-1) (h :: acc)
  in if n < 0 then aux lst (List.length lst + n) [] else aux lst n []

let () = assert ((rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] 3) = ["d"; "e"; "f"; "g"; "h"; "a"; "b"; "c"])
let () = assert ((rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] (-2)) = ["g"; "h"; "a"; "b"; "c"; "d"; "e"; "f"]) 