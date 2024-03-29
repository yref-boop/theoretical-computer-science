(*dado un predicado ('a -> bool) -> ['a] -> 'a*)
(*primero_que_cumple : ('a -> bool) -> ['a] -> 'a*)
let rec primero_que_cumple predicado list = match list with
    | [] -> raise (Invalid_argument "empty list")
    | _ ->
        let rec aux predicado = function
            | [] -> raise (Not_found)
            | h :: t -> if (predicado h) then h else aux predicado t
        in aux predicado list
;;

(* existe : ('a -> bool ) -> a' list -> bool*)
(*exception x of String*)
(*raise (x "not found")*)
let rec existe predicado list =
    try (function a -> true) (primero_que_cumple predicado list) with
        | Not_found -> false
        | Invalid_argument _ -> raise (Invalid_argument "empty list")
;;

(*lista de pares (clave, valor)*)
(*dado conjunto y clave, devuelve valor*)
let asociado lista valor =
    let (a, b) =
        primero_que_cumple (function (a, b) -> a == valor) lista
    in b
;;
