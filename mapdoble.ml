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


(*dado un predicado ('a -> bool) -> ['a] -> 'a*)
(*primero_que_cumple : ('a -> bool) -> ['a] -> 'a option*)
let rec primero_que_cumple predicado list = match list with
    [] -> None
    | h :: t -> if (predicado h) then (Some h) else primero_que_cumple predicado t
;;


(* existe : ('a -> bool ) -> a' list -> bool*)
(*exception x of String*)
(*raise (x "not found")*)
let rec existe predicado list = match list with
    [] -> false
    | h :: t -> if (predicado h) then true else primero_que_cumple predicado t
;;

let existe predicado list = match (primero_que_cuple predicado list) with
   Some -> true
   False -> false
;;
