(*a*)
(*mapdoble (function x -> x) (function x -> -x)*)

let mapdoble f g list = match list with
    | [] -> raise (Invalid_argument "empty list")
    | _ ->
        let rec aux f g result = function
            [] -> List.rev result
            | h::t -> aux g f ((f h) :: result) t
        in aux f g [] list
;;

(*b*)
(*mapdoble : (fun a' -> 'b) -> (fun a' -> b') -> [a'] -> [b']*)

(*c*)
(*la llamada mapdoble (function x -> x*2) (function x -> "x") [1;2;3;4;5];; producirá un error de tipo:
    - su primera función tiene el tipo a' -> int
    - la segunda a' -> char
por lo que la función intentará crear una lista con valores int y char, produciendo una excepcion*)

(*d*)
(*tipo (fun '_weak1 -> int) -> ['_weak1] -> [int]
siendo '_weak1 un tipo polimorfico*)
