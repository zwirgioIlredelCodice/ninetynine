(*Extract a slice from a list. (medium)*)

let slice lst l u =
  let rec aux lst i acc =
    match lst with
    | [] -> List.rev acc
    | h :: t ->
      if i > u then acc
      else 
        if i >= l then aux t (i+1) (h :: acc)
        else aux t (i+1) acc
  in aux lst 0 [] 

let () = assert ((slice ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 2 6) = ["c"; "d"; "e"; "f"; "g"])