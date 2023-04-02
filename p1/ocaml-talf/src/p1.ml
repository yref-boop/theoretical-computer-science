(*
open Auto
open Conj
open Ergo
open Graf
*)

(*
función es_afne(automata):
  para cada estado en el automata:
    para cada transición en las transiciones del estado:
      si el símbolo de entrada de la transición es epsilon:
        devolver true
  para cada estado en el autómata:
    para cada transición en las transiciones del estado:
      si el símbolo de entrada de la transición no es epsilon:
        si el estado de destino tiene épsilon-transiciones:
          devolver true
  devolver false
*)
open Auto;;

let es_afne (Af (_, _, _, (Conjunto arcos), _) as automata) =
    let rec aux automata = function
        | [] -> false
        | Arco_af (_, _, simbolo) :: list ->
                if simbolo = Terminal "" then true
                else aux automata list
    in
        aux automata arcos
;;

(*
función es_afn(automata):
  para cada estado en el automata:
    para cada transición en las transiciones del estado:
      si hay algúm símbolo de entrada duplicado en las transiciones del estado:
        devolver true
  devolver false
*)

(* no deben existir otros arcos con igual origen y simbolo ni epsilon *)
let arco_determinista conjunto (Arco_af (origen, destino, simbolo)) =
    let predicado (Arco_af (i, o, s)) =
        (simbolo = Terminal"") || (i = origen && o != destino && s = simbolo)
    in
        not (List.exists (predicado) conjunto)
;;

let es_afn (Af (_, _, _, (Conjunto arcos), _) as automata) =
    let rec aux automata accumulator = function
        | [] -> false
        | arco :: lista ->
                if not (arco_determinista accumulator arco) then true
                else aux automata (arco :: accumulator) lista
    in
        aux automata [] arcos
;;

(*
funcion es_afd(automata):
  simbolos_entrada_inicial = lista de símbolos de entrada de las transiciones del estado inicial del automata
  estados_visitados = [estado inicial del automata]
  para cada estado en estados_visitados:
    transiciones_estado = lista de transiciones del estado en el automata
    simbolos_entrada = lista de simbolos de entrada de las transiciones_estado
    si hay algún símbolo de entrada duplicado en simbolos_entrada:
      devuelve false
    para cada símbolo de simbolos_entrada:
      destino = estado de destino de la transición correcpondiente al símbolo
      si el destino no es un estado del autómata:
        devolver false
      si el destino no está en estados_visitados:
        añadir destino a estados_visitados
  devolver true
*)

(* revisar que el numero de arcos es el correcto *)
let arcos_completos estados alfabeto arcos =
    Conj.cardinal arcos = (Conj.cardinal alfabeto * Conj.cardinal estados)
;;

let es_afd automata = match automata with
    Af (estados, alfabeto, _, arcos, _) ->
        not (es_afn automata) && arcos_completos estados alfabeto arcos
;;

(*
añadir estado_actual1, estado_actual2 a estados_visitados
para cada simbolo en el alfabeto:
    nuevo_estado1 = estado alcanzado desde el estado_actual_1 con el simbolo
    nuevo_estado2 = estado alcanzado desde el estado_actual_2 con el simbolo
    añadir (nuevo_estado1, nuevo_estado2) a la cola
*)

let rec siguiente estado simbolo = function
    | [] -> estado;
    | h :: t ->
        match h with Arco_af (origen, destino, simbolo_arco) ->
            if (origen = estado) && (simbolo_arco = simbolo)
                then destino
            else siguiente estado simbolo t
;;

let equivalentes (automata1, automata2) = match (automata1, automata2) with

    (Af (_,alfabeto1,inicial1,arcos1,finales1),
        Af(_,alfabeto2,inicial2,arcos2,finales2)) ->

        let alfabeto = Conj.union alfabeto1 alfabeto2 in
        let queue = Queue.create () in
        Queue.add (inicial1, inicial2) queue;
        let rec consume queue visitados =
            if Queue.is_empty queue then true
            else
                let estado_actual = Queue.pop queue in
                if Conj.pertenece estado_actual visitados
                    then consume queue visitados
                else
                    if not ((Conj.pertenece (fst estado_actual) finales1) = (Conj.pertenece (snd estado_actual) finales2))
                        then false
                    else
                    let rec procesar = function
                        | [] -> consume queue (Conj.agregar estado_actual visitados)
                        | h :: t ->
                            let nuevo_estado1 = siguiente (fst estado_actual) h (Conj.list_of_conjunto arcos1) in
                            let nuevo_estado2 = siguiente (snd estado_actual) h (Conj.list_of_conjunto arcos2) in
                            Queue.add (nuevo_estado1, nuevo_estado2) queue;
                            procesar t
                    in
                    procesar (Conj.list_of_conjunto alfabeto);
    in consume queue Conj.conjunto_vacio
;;

(*
function equivalentes (automata1, automata2):
    estados1 = estados del automata1
    estados_finales_1 = estados finales del automata 1
    estados2 = estados del automata2
    estados_finales_2 = estados finales del automata 2
    alfabeto = alfabeto del automata1 y del automata2
    estados_visitados = conjunto vacio
    cola = [(estado_inicial1, estado_inicial2)]
    mientras cola no este vacia:
        (estado_actual1, estado_actual2) = extraer el primer elemeento de la cola si (estado_actual1, estado_actual2) esta en estados visitados: continuar con el siguiente elemento de la cola si estado_actual1 es final y estado_actual2 no es final o viceversa:
            devolver false
        si no:
            añadir estado_actual1, estado_actual2 a estados_visitados
            para cada simbolo en el alfabeto:
                nuevo_estado1 = estado alcanzado desde el estado_actual_1 con el simbolo
                nuevo_estado2 = estado alcanzado desde el estado_actual_2 con el simbolo
                añadir (nuevo_estado1, nuevo_estado2) a la cola
    devolver true
*)

(* original *)
let escaner_af cadena (Af (_, _, inicial, _, finales) as a) =

   let rec aux = function
        | (Conj.Conjunto [], _) -> false
        | (actuales, []) ->
           not (Conj.es_vacio (Conj.interseccion actuales finales))
        | (actuales, simbolo :: t) ->
           aux ((epsilon_cierre (avanza simbolo actuales a) a), t)
   in
      aux ((epsilon_cierre (Conjunto [inicial]) a), cadena)
   ;;

(* sin aceptar epsilon-transiciones *)
let escaner_afn cadena (Af (_, _, inicial, _, finales) as a) =

   let rec aux = function
        | (Conj.Conjunto [], _) -> false
        | (actuales, []) ->
           not (Conj.es_vacio (Conj.interseccion actuales finales))
        | (actuales, simbolo :: t) ->
           aux (Auto.avanza simbolo actuales a, t)
   in
      aux (Conjunto [inicial], cadena)
;;


let avanza_determinista simbolo estado (Af (_, _, _, Conjunto arcos, _)) =
    let rec aux = function
        | [] -> Estado "vacio"
        | Arco_af (origen, destino, simbolo_arco) :: t ->
                if (simbolo_arco = simbolo) && (origen = estado) then
                    destino
                else aux t
    in
        aux arcos
;;

(* solo aceptando automatas finitos deterministas *)
let escaner_afd cadena (Af (_, _, inicial, _, finales) as a) =

   let rec aux = function
        | (Estado "vacio", _) -> false
        | (actual, []) ->
           (Conj.pertenece actual finales)
        | (actual, simbolo :: t) ->
           aux (avanza_determinista simbolo actual a, t)
   in
      aux (inicial, cadena)
;;


let afne = Af (
    Conjunto [Estado "0"; Estado "1"; Estado "2"; Estado "3"],
    Conjunto [Terminal "a"; Terminal "b"; Terminal "c"], Estado "0",
    Conjunto [Arco_af (Estado "0", Estado "1", Terminal "a");
        Arco_af (Estado "1", Estado "1", Terminal "b");
        Arco_af (Estado "2", Estado "3", Terminal "");
        Arco_af (Estado "2", Estado "0", Terminal "");
        Arco_af (Estado "2", Estado "0", Terminal "");
        Arco_af (Estado "1", Estado "2", Terminal "a");
        Arco_af (Estado "2", Estado "3", Terminal "c")],
    Conjunto [Estado "1"; Estado "3"])
;;

let afn = Af (
    Conjunto [Estado "0"; Estado "1"; Estado "2"; Estado "3"],
    Conjunto [Terminal "a"; Terminal "b"; Terminal "c"], Estado "0",
    Conjunto [Arco_af (Estado "0", Estado "1", Terminal "a");
        Arco_af (Estado "1", Estado "1", Terminal "b");
        Arco_af (Estado "1", Estado "2", Terminal "a");
        Arco_af (Estado "2", Estado "3", Terminal "c");
        Arco_af (Estado "2", Estado "1", Terminal "c")],
    Conjunto [Estado "1"; Estado "3"])
;;

let afd = Af (
    Conjunto [Estado "0"; Estado "1"; Estado "2"],
    Conjunto [Terminal "a"; Terminal "b"; Terminal "c"],
    Estado "0",
    Conjunto [
        Arco_af (Estado "0", Estado "1", Terminal "a");
        Arco_af (Estado "0", Estado "2", Terminal "b");
        Arco_af (Estado "0", Estado "0", Terminal "c");

        Arco_af (Estado "1", Estado "2", Terminal "a");
        Arco_af (Estado "1", Estado "1", Terminal "b");
        Arco_af (Estado "1", Estado "0", Terminal "c");

        Arco_af (Estado "2", Estado "0", Terminal "a");
        Arco_af (Estado "2", Estado "1", Terminal "b");
        Arco_af (Estado "2", Estado "2", Terminal "c")],
    Conjunto [Estado "0"; Estado "2"])
;;
