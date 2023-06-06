let es_fnc = function
  Gic (no_terminales, terminales, Conjunto reglas, _) ->
    let rec aux = function
      [] -> true

      | Regla_gic (_, h :: []) :: t -> 
        (pertenece h terminales) &&
        (aux t)

      | Regla_gic (_, h1 :: h2 :: []) :: t ->
        (pertenece h1 no_terminales) &&
        (pertenece h2 no_terminales) &&
        (aux t)

      | _ -> false
    in aux reglas
;;

let rec conjunto_of_list l =
	let rec aux l conjunto =
		match l with
		  [] -> conjunto
			| h :: t ->aux t (agregar h conjunto)
	in aux l (Conjunto([]))
;;

let rec get_terminales simbolo reglas l_no_terms = 
	match reglas with
		| [] ->  l_no_terms
		| (Regla_gic(s,l))::t -> 
			if List.mem simbolo l then 
				get_terminales simbolo t (s::l_no_terms)
			else 
				get_terminales simbolo t l_no_terms;;

let rec buscar_no_terminales cadena reglas l = 
	match cadena with
	  [] ->List.rev l
    | h::t ->
      buscar_no_terminales t reglas
        (conjunto_of_list(get_terminales h reglas []) :: l)
;;

let rec get_prod_cart_list_conj l_conj l_conj_cart = 
	match l_conj with
	  [] -> l_conj_cart
		| h :: [] -> l_conj_cart
		| h1 :: h2 :: t -> get_prod_cart_list_conj (h2 :: t)
					((cartesiano h1 h2) :: l_conj_cart)
;;

let rec comp_terminales a b reglas l_terminales =
	match reglas with
	  [] -> l_terminales
		| (Regla_gic(_,(h :: []))) :: t ->
			comp_terminales a b t l_terminales
		| (Regla_gic(s,(h1 :: h2 :: []))) :: t ->
			if (h1 = a && h2 = b )then
				comp_terminales a b t (s :: l_terminales)
			else comp_terminales a b t l_terminales
		| (Regla_gic(_,_) :: t) -> comp_terminales a b t l_terminales
;;

let rec recorre_conjunto conjunto reglas lista_resultados =
	match conjunto with
    Conjunto [] ->conjunto_of_list lista_resultados
    | Conjunto ((a,b) :: t) ->
      recorre_conjunto (Conjunto t) reglas (comp_terminales a b reglas lista_resultados)
;;
			
let rec segunda_fila l_conj_cartesianos reglas lpadres = 
	match l_conj_cartesianos with
	  [] -> List.rev lpadres
		| (Conjunto ((a,b) :: t)) :: t2 ->
			segunda_fila t2 reglas ((recorre_conjunto (Conjunto ((a,b) :: t)) reglas []) :: lpadres)
		| (Conjunto [] :: t2) -> segunda_fila t2 reglas lpadres
;;

let rec get_pos l cont = 
	match l with
	  [] -> []
		| h :: t ->
			if cont = 0 then h
			else get_pos t (cont - 1)
;;

let diagonal l cont cont_col = 
	let lista = get_pos l cont in
	let rec aux lista cont_col = 
		match lista with
		  [] -> Conjunto []
			| h :: t ->
				if cont_col = 0 then h
				else aux t (cont_col - 1)
	in aux lista (cont+cont_col + 1)
;;

let vertical l cont cont_col = 
	let lista = get_pos l cont in
	let rec aux lista cont_col = 
		match lista with
		  [] -> Conjunto []
			| h :: t ->
				if cont_col = 0 then h
				else aux t (cont_col - 1)
	in aux lista cont_col
;;

let rec diagonal_vertical list_of_list reglas cont_bajar cont_subir cont_col =
	let conj_vertical = vertical list_of_list cont_bajar cont_col in 
	let conj_diagonal = diagonal list_of_list cont_subir cont_col in [conj_vertical; conj_diagonal]
;;

let rec subir_y_bajar list_of_list reglas n_col vertical diagonal lista= 
	if diagonal = -1 then lista
	else
		subir_y_bajar list_of_list reglas n_col (vertical + 1) (diagonal - 1)
			((diagonal_vertical list_of_list reglas vertical diagonal n_col) :: lista)
;;

let rec casilla l lres = 
	match l with
	  [] -> lres
		| h :: t -> casilla t ((get_prod_cart_list_conj h lres))
;;

let rec simplificar l reglas lres = 
	match l with
	  [] -> lres
		| h::t -> simplificar t reglas ((segunda_fila h reglas []) :: lres)
;;

let rec limpieza l lres = 
	match l with
	  [] -> lres
		| h::t -> limpieza t ((casilla h []) :: lres)
;;
		
let rec recorrer_fila list_of_list reglas =
	match list_of_list with
    [] -> []
    | h :: t ->
      let rec aux lista n_col diagonal lres = 
        match lista with
          [] -> limpieza lres []
          | h2 :: [] -> limpieza lres []
          | h2 :: t2 -> aux t2 (n_col+1) diagonal 
            ((subir_y_bajar list_of_list reglas n_col 0 diagonal []) :: lres)
      in aux h 0 ((List.length list_of_list) - 1) []
;;

let rec unir l lres =
	match l with
    [] -> lres
    | h :: t -> let rec aux lista_conjuntos csol =
      match lista_conjuntos with
        [] -> csol
        | h1 :: t1 -> aux t1 ((union h1 csol))
    in unir t ((aux h (Conjunto [])) :: lres)
;;

let rec comprobar_axioma lista axioma = 
	match lista with
    [] -> false
    | h::t ->
      let rec aux l =
        match l with
          | [] -> false
          | h::t -> 
            if pertenece axioma h then true
            else false 
      in aux h
;;

let cyk cadena gic = 
	if not (es_fnc gic) then false
  else
		match gic with
		Gic(no_terminales, terminales, Conjunto reglas, axioma) -> 
			let fila1 = buscar_no_terminales cadena reglas [] in 
			let lista = [fila1] in 
			let rec aux lista = 
				if (List.length lista) >= (List.length cadena) then
					comprobar_axioma lista axioma
				else 
					let fila = recorrer_fila lista reglas in
					let simp = simplificar fila reglas [] in 
					let lista = (unir simp [])::lista in aux lista 
			in aux lista
;;
