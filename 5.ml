(*5. Reverse a list. (easy)*)

let rev lst =
  let rec rev_aux lst acc =
    match lst with
    | [] -> acc
    | h :: t -> rev_aux t (h :: acc)
  in rev_aux lst []

let () = assert (rev ["a"; "b"; "c"] = ["c"; "b"; "a"])