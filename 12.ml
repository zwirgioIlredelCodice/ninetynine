(*12. Decode a run-length encoded list. (medium)*)
type 'a rle =
  | One of 'a 
  | Many of int * 'a

let decode lst =
  let rec aux acc = function
    | [] -> acc
    | One x :: t -> aux (x :: acc) t
    | Many (2, x) :: t -> aux (x :: acc) (One x :: t)
    | Many (n, x) :: t -> aux (x :: acc) (Many ((n-1), x) :: t)
  in List.rev (aux [] lst)

(*sol*)
let decode list =
  let rec many acc n x =
    if n = 0 then acc else many (x :: acc) (n - 1) x
  in
  let rec aux acc = function
    | [] -> acc
    | One x :: t -> aux (x :: acc) t
    | Many (n, x) :: t -> aux (many acc n x) t
  in
    aux [] (List.rev list)

let () = assert ((decode [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d"; Many (4, "e")]) = ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"])