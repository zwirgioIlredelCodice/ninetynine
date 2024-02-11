(*Split a list into two parts; the length of the first part is given. (easy)*)

let split lst n = 
  let rec aux lst n acc =
    match lst with 
    | [] -> (List.rev acc, [])
    | h :: t as l -> if n = 0 then (List.rev acc, l) else aux t (n-1) (h :: acc)
  in aux lst n []

let () = assert ((split ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 3) = (["a"; "b"; "c"], ["d"; "e"; "f"; "g"; "h"; "i"; "j"]))
let () = assert ((split ["a"; "b"; "c"; "d"] 5) = (["a"; "b"; "c"; "d"], []))