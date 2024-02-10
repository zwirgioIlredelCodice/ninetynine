(*9. Pack consecutive duplicates of list elements into sublists. (medium)*)

let pack lst =
  let rec aux acc = function
    | [] -> acc :: []
    | h :: t -> match acc with
      | [] -> aux (h :: []) t
      | hh :: _ -> 
        if h = hh then aux (h :: acc) t 
        else acc :: (aux (h :: []) t)
  in aux [] lst 

(*sol*)
let pack list =
  let rec aux current acc = function
    | [] -> []    (* Can only be reached if original list is empty *)
    | [x] -> (x :: current) :: acc
    | a :: (b :: _ as t) ->
       if a = b then aux (a :: current) acc t
       else aux [] ((a :: current) :: acc) t  in
  List.rev (aux [] [] list);;

let () = assert ((pack ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "d"; "e"; "e"; "e"; "e"]) = [["a"; "a"; "a"; "a"]; ["b"]; ["c"; "c"]; ["a"; "a"]; ["d"; "d"];["e"; "e"; "e"; "e"]])