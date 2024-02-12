(*20. Remove the K'th element from a list. (easy)*)

let remove_at n lst =
  let rec aux n lst acc =
    match lst with 
    | [] -> List.rev acc
    | h :: t -> if n = 0 then List.rev acc @ t else aux (n-1) t (h :: acc)
  in aux n lst []

let () = assert ((remove_at 1 ["a"; "b"; "c"; "d"]) = ["a"; "c"; "d"])