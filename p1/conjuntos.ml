(*definicion de tipo dada*)
type 'a conjunto = Conjunto of 'a list
;;

let rec pertenece item = function
    | Conjunto [] -> false
    | Conjunto (h :: t) ->
        if (item == h) then true else pertenece item (Conjunto t)
;;

let agregar item (Conjunto set) =
    if pertenece item (Conjunto set)
        then Conjunto set
        else Conjunto (item::set)
;;

let conjunto_of_list list =
    Conjunto list
;;

let suprimir item (Conjunto set) =
    Conjunto (List.filter (fun x -> x != item) set)
;;

let cardinal (Conjunto set) =
    List.length set
;;

let rec union set1 = function
    | Conjunto [] -> set1
    | Conjunto (h :: t) ->
        union (agregar h set1) (Conjunto t)
;;

let interseccion set1 set2 =
    let rec aux accum set1 = function
        | Conjunto [] -> Conjunto accum
        | Conjunto (h :: t) ->
            if pertenece h set1
                then aux (h::accum) set1 (Conjunto t)
                else aux accum set1 (Conjunto t)
    in aux [] set1 set2
;;

let rec diferencia set1 = function
    | Conjunto [] -> set1
    | Conjunto (h :: t) ->
        diferencia (suprimir h set1) (Conjunto t)
;;

let incluido set1 set2 =
    (cardinal (interseccion set1 set2)) == (cardinal set1)
;;

let igual set1 set2 =
    (incluido set1 set2) && (cardinal set1) == (cardinal set2)
;;

let producto_cartesiano (Conjunto set1) (Conjunto set2) =
    List.concat (List.map (fun elem1 -> List.map (fun elem2 -> (elem1, elem2)) set2) set1)

let list_of_conjunto (Conjunto set) = set ;;
