(*USANDO EXCEPCIONES*)
(*don't like it*)
(*robust & efficient*)

(*dado un predicado ('a -> bool) -> ['a] -> 'a*)
(*primero_que_cumple : ('a -> bool) -> ['a] -> 'a*)
let rec primero_que_cumple predicado list = match list with
    [] -> raise (Not_found)
    | h :: t -> if (predicado h) then h else primero_que_cumple predicado t
;;

(*funcion auxiliar de existe*)
let check n = true;;

(* existe : ('a -> bool ) -> a' list -> bool*)
(*exception x of String*)
(*raise (x "not found")*)
let rec existe predicado list =
    try check (primero_que_cumple predicado list) with
        Not_found -> false
;;

(*funcion auxiliar de asociado*)
let comprobar valor n = if valor == n then true else false;;

(*lista de pares (clave, valor)*)
(*dado conjunto y clave, devuelve valor*)
let asociado lista valor =
    primero_que_cumple (comprobar valor) lista
;;

(*funcion auxiliar de asociado*)
let comprobar valor n = valor == (fst n) ;;

(*lista de pares (clave, valor)*)
(*dado conjunto y clave, devuelve valor*)
let asociado lista valor =
    snd (primero_que_cumple (comprobar valor) lista)
;;



(*USANDO TIPOS OPCIONALES*)
(*worse performance*)
(*feels simpler*)

(*dado un predicado ('a -> bool) -> ['a] -> 'a*)
(*primero_que_cumple : ('a -> bool) -> ['a] -> 'a*)
let rec primero_que_cumple predicado list = match list with
    [] -> None
    | h :: t -> if (predicado h) then Some h else primero_que_cumple predicado t
;;

(* existe : ('a -> bool ) -> a' list -> bool*)
(*exception x of String*)
(*raise (x "not found")*)
let rec existe predicado list =
    match (primero_que_cumple predicado list) with
        None -> false
        | Some h -> true
;;

(*funcion auxiliar de asociado*)
let comprobar valor n = valor == (fst n);;

(*lista de pares (clave, valor)*)
(*dado conjunto y clave, devuelve valor*)
let asociado lista valor =
    match primero_que_cumple (comprobar valor) lista with
        None -> 0
        | Some h -> snd h
;;
