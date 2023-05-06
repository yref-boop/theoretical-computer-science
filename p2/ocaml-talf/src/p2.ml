open Auto;;
open Conj;;
open Ergo;;
open Graf;;

(* funcion basada en es_regular *)
let es_fnc = function
    Gic (no_terminales, terminales, Conjunto reglas, _) ->
        let rec aux = function
            | [] -> true
            | Regla_gic (_, h :: []) :: t ->
                (pertenece h terminales) && (aux t)
            | Regla_gic (_, h1 :: h2 :: []) :: t ->
                (pertenece h2 no_terminales) &&
                (pertenece h2 no_terminales) && (aux t)
            | _ -> false
        in aux reglas
;;
