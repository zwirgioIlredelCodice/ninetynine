(*Replicate the elements of a list a given number of times. (medium)*)

let rec times x n =
  match n with 
    | 0 -> []
    | n -> x :: times x (n-1) 

let rec replicate lst n =
  match lst with
  | [] -> []
  | h :: t -> List.append (times h n) (replicate t n)  

let () = assert ((replicate ["a"; "b"; "c"] 3) = ["a"; "a"; "a"; "b"; "b"; "b"; "c"; "c"; "c"])