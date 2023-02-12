(*dado un predicado ('a -> bool) -> ['a] -> 'a*)
(*primero_que_cumple : ('a -> bool) -> ['a] -> 'a*)
let rec primero_que_cumple predicado list = match list with
    | [] -> raise (Not_found)
    | h :: t -> if (predicado h) then h else primero_que_cumple predicado t
;;

(* existe : ('a -> bool ) -> a' list -> bool*)
(*exception x of String*)
(*raise (x "not found")*)
let rec existe predicado list =
    let check n = true in
    try check (primero_que_cumple predicado list) with
        Not_found -> false
;;

(*lista de pares (clave, valor)*)
(*dado conjunto y clave, devuelve valor*)
let asociado lista valor =
    let comprobar valor n = valor == n in
    primero_que_cumple (comprobar valor) lista
;;

(*funcion auxiliar de asociado*)
let comprobar valor n = valor == (fst n) ;;

(*lista de pares (clave, valor)*)
(*dado conjunto y clave, devuelve valor*)
let asociado lista valor =
    snd (primero_que_cumple (comprobar valor) lista)
;;
