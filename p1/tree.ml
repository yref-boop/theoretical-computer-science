(*tipo definido:*)
type 'a arbol_binario =
    | Vacio
    | Nodo of 'a * 'a arbol_binario * 'a arbol_binario
;;

(*ejemplo dado*)
let t = Nodo (3, Nodo (2, Vacio, Vacio), Nodo (5, Nodo (4, Vacio, Vacio), Nodo (1, Vacio, Vacio)));;
(*ejemplo extra*)
let t = Nodo (3, Nodo (2, Nodo (9, Vacio, Vacio), Vacio), Nodo (5, Nodo (4, Vacio, Vacio), Nodo (1, Vacio, Vacio)));;

(*funcion auxiliar:*)
let rec fold_tree f accum = function
    | Vacio -> accum
    | Nodo (root, left, right) ->
        f root (fold_tree f accum left) (fold_tree f accum right);;

let in_order tree =
    fold_tree (fun root left right -> left @ root :: right) [] tree;;

let pre_order tree =
    fold_tree (fun root left right -> root :: left @ right) [] tree;;

let post_order tree =
    fold_tree (fun root left right -> left @ right @ [root]) [] tree;;

let anchura tree =
    let rec aux accum results = match accum with
        | [] -> List.rev results
        | Vacio :: t -> aux t results
        | (Nodo (root, left, right)) :: t ->
            aux (t @ [left] @ [right]) (root :: results)
    in aux [tree] []
;;
