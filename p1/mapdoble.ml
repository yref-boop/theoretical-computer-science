(*a*)
(*mapdoble (function x -> x) (function x -> -x)*)
(*recursiva simple*)   
let rec mapdoble f g list = match list with 
    [] -> []
    | h::t -> f h :: mapdoble g f t
;;

(*recursiva terminal*)
let mapdoble f g list =
    let rec aux f g list result = match list with
        [] -> result
        | h::t -> aux g f t (result :: (f h))
    in aux f g list []
;;

(*b*)
(*mapdoble : (fun a' -> 'b) -> (fun a' -> b') -> [a'] -> [b']*)

(*c*)
(*la llamada mapdoble (function x -> x*2) (function x -> "x") [1;2;3;4;5];; producirá un*)
(*error de tipo ya que su primera funcion tiene el tipo a' -> int y la segunda a' -> char*)
(*por lo que la funcion intentara crear una lista con valores int y char, produciendo una excepcion*)

(*d*)
(*tipo (fun '_weak1 -> int) -> ['_weak1] -> [int]*)
(*siendo '_weak1 un tipo polimorfico*)


