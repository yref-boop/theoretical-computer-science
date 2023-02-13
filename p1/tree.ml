(*tipo definido:*)
type 'a arbol_binario =
    | Vacio
    | Nodo of 'a * 'a arbol_binario * 'a arbol_binario
;;

(*ejemplo dado*)
let t = Nodo (3, Nodo (2, Vacio, Vacio), Nodo (5, Nodo (4, Vacio, Vacio), Nodo (1, Vacio, Vacio)));;
(*ejemplo extra*)
let t = Nodo (3, Nodo (2, Nodo (9, Vacio, Vacio), Vacio), Nodo (5, Nodo (4, Vacio, Vacio), Nodo (1, Vacio, Vacio)));;

let in_orden tree =
    let rec fold accum = function
        | Vacio -> accum
        | Nodo (root, left, right) ->
            fold (root :: (fold accum right)) left
    in fold [] tree
;;

let pre_orden tree =
    let rec fold accum = function
        | Vacio -> accum
        | Nodo (root, left, right) ->
            root :: (fold (fold accum right) left)
    in fold [] tree
;;

let post_orden tree =
    let rec fold accum = function
        | Vacio -> accum
        | Nodo (root, left, right) ->
            fold (fold (root :: accum) right) left
    in fold [] tree
;;

let anchura tree =
    let rec fold result = function
        | [] -> List.rev result
        | Vacio :: t -> fold result t
        | Nodo (root, left, right) :: t ->
            fold (root :: result) (List.rev (right :: left :: List.rev t))
    in fold [] [tree]
;;

(* queues O(1) inserting & taking*)
let anchura tree =
    let queue = Queue.create () in
    Queue.add tree queue;
    let rec fold result queue =
        if Queue.is_empty queue then List.rev result
        else
            let value = Queue.pop queue in
            let aux result queue = function
                | Vacio -> fold result queue
                | Nodo (root, left, right) ->
                    Queue.add left queue;
                    Queue.add right queue;
                    fold (root :: result) queue
            in aux result queue value
    in fold [] queue
;;
