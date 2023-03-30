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

let epsilon_transicion arco = match arco with
    Arco_af (origen, out, simbolo) ->
        (origen == Estado "") || (out == Estado "") || (simbolo == Terminal "")


let es_afne (automata) = match automata with
    Af (_, _, _, arcos, _) ->
        let rec aux automata = function
            | [] -> false
            | Arco_af (_, _, simbolo) :: list ->
                    if simbolo == Terminal "" then true
                    else aux automata list
        in
    aux automata (Conj.list_of_conjunto arcos)
;;

let es_afne (automata) = match automata with
    Af (_, _, _, arcos, _) ->
        let rec aux automata = function
            | [] -> false
            | Arco_af (_, _, simbolo) :: list ->
                    if simbolo = Terminal "" then true
                    else aux automata list
        in
    aux automata (Conj.list_of_conjunto arcos)
;;

let es_afne (automata) = match automata with
    Af (estados, _, _, _, _) ->
        not ((Conj.cardinal (estados)) == (Conj.cardinal (Auto.epsilon_cierre estados automata)))
;;


let ec automata = match automata with
    Af (estados, _, _, _, _) ->
        Auto.epsilon_cierre estados automata
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

let es_afd (automata) = match automata with
    Af (estados, alfabeto, estado_inicial, function_transicion, estados_finales)
    (* comprobar que para cada estado, funcion_transicion, (estado1, estado2, simbolo) estado1 no se repite *)
    -> false;
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
        (estado_actual1, estado_actual2) = extraer el primer elemeento de la cola
        si (estado_actual1, estado_actual2) esta en estados visitados:
            continuar con el siguiente elemento de la cola
        si estado_actual1 es final y estado_actual2 no es final o viceversa:
            devolver false
        si no:
            añadir estado_actual1, estado_actual2 a estados_visitados
            para cada simbolo en el alfabeto:
                nuevo_estado1 = estado alcanzado desde el estado_actual_1 con el simbolo
                nuevo_estado2 = estado alcanzado desde el estado_actual_2 con el simbolo
                añadir (nuevo_estado1, nuevo_estado2) a la cola
    devolver true
*)



(*
optativa: ocaml.talf -> simplificar & optimizar
- (entender escanear_af)
*)

        Arco_af (Estado "2", Estado "3", Terminal "");
        Arco_af (Estado "2", Estado "0", Terminal "");



Terminal "";;



let afne = Af (
    Conjunto [Estado "0"; Estado "1"; Estado "2"; Estado "3"],
    Conjunto [Terminal "a"; Terminal "b"; Terminal "c"], Estado "0",
    Conjunto [Arco_af (Estado "0", Estado "1", Terminal "a");
        Arco_af (Estado "1", Estado "1", Terminal "b");
        Arco_af (Estado "2", Estado "3", Terminal "");
        Arco_af (Estado "2", Estado "0", Terminal "");
        Arco_af (Estado "1", Estado "2", Terminal "a");
        Arco_af (Estado "2", Estado "3", Terminal "c")],
    Conjunto [Estado "1"; Estado "3"])
