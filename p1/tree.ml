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


let in_order tree =
    let rec aux tree accumulator = match tree with
        | Vacio -> accumulator
        | Nodo (value, left, right) ->
            let acc_with_right = aux right accumulator in
            let acc_with_value = value :: acc_with_right in
            aux left acc_with_value in
    aux tree []
;;

let pre_order tree =
    let rec aux tree accumulator = match tree with
        | Vacio -> accumulator
        | Nodo (value, left, right) ->
            let acc_with_right = aux right accumulator in
            let acc_with_left = aux left acc_with_right in
            value :: acc_with_left in
    aux tree []
;;

let post_order tree =
    let rec aux tree accumulator = match tree with
        | Vacio -> accumulator
        | Nodo (value, left, right) ->
            let acc_with_value = value :: accumulator in
            let acc_with_right = aux right acc_with_value in
            aux left acc_with_right in
    aux tree []
;;

let anchura tree =
    let rec aux accum results = match accum with
        | [] -> List.rev results
        | Vacio :: t -> aux t results
        | (Nodo (root, left, right)) :: t ->
            aux (List.rev (right :: left :: List.rev t)) (root :: results)
    in aux [tree] []
;;
