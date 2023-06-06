open Auto;;
open Conj;;
open Ergo;;
open Graf;;


let cyk cadena = function gic -> 
  if (not (es_fnc gic)) then 
    raise (Failure "No está en FNC")
  else if ((List.length cadena)=0) then
    raise (Failure "La cadena de entrada debe tener al menos un símbolo")
  else 
    (*Reglas para hojas -> está en FNC por lo que las reglas de producción que buscamos son del formato A->a*)
    let buscar_reglas s rp =
      let rec loop s l = function
        | [] -> Conjunto (l)
        | (Regla_gic(no_tem,[term]))::t -> if term=s then (loop s (no_tem::l) t) else (loop s l t) 
        | (Regla_gic(_,_))::t -> (loop s l t)
      in loop s [] rp (*tail recursive*)

    (*habrá que comprobar que ninguno quede vacío ¿?¿?*)
    in let rec inicializar rp = function
      | [] -> [] 
      | h::t -> (buscar_reglas h rp)::(inicializar rp t)

    (*
      dados 2 cjtos de posibles símbolos, 
      calculamos que reglas (FNC) podrían generar cualquien combinación de 1s de c1 y 1s de c2.
      Añadimos los no terminales de las reglas a cS
    *)


    in let rec gen_prods c1 c2 cS = function
      | [] -> cS
      | (Regla_gic(no_tem,[term1;term2]))::t -> 
        (*cambiar esto por eager*)
        if ((pertenece term1 c1) && (pertenece term2 c2)) then gen_prods c1 c2 (agregar no_tem cS) t 
        else gen_prods c1 c2 cS t
      | _::t -> gen_prods c1 c2 cS t
    
    (*revisar convenio de ij*)
    in let get_simbols i j matrix = (Printf.printf "i %8d j %8d\n" i j; List.nth (List.nth matrix (j-1)) (i-1)) 

    in let generar_celda i j top matrix rp =

      Printf.printf "k max: %8d\n" top;
      let rec loop k i j top c matrix rp =
        Printf.printf "lo intentaremos con: (%8d,%8d) y (%8d,%8d)\n" i k (i+k) (j-k);
        if k>top then
          c
        else
          loop (k+1) i j top (gen_prods (get_simbols i k matrix) (get_simbols (i+k) (j-k) matrix) c rp) matrix rp (*estoy mal (i+k) (j-k)*)
          
      in loop 1 i j top (Conjunto([])) matrix rp

    in let generar_celdas j top matrix rp=
      let rec loop i j top l matrix rp=
        if i>top then
          l
        else 
          (Printf.printf "-gen celda: %8d\n" i;loop (i+1) j top (l@[(generar_celda i j (j-1) matrix rp)]) matrix rp)
      in loop 1 j top [] matrix rp

    in let rec check_last_level axiom = function
      | [last]::[] -> pertenece axiom last
      | h::t -> check_last_level axiom t
      | _ -> raise (Failure "Niveles mal generados")

    in let cyk_loop n axiom matrix rp =
      let rec loop j n axiom matrix rp =
        if j>n then (*revisar que llegue a donde debe*)
          check_last_level axiom matrix
        else 
          (Printf.printf "------Cyk bulce j: %8d\n" j;loop (j+1) n axiom (matrix@[(generar_celdas j (n-j+1) matrix rp)]) rp) (*mirar de poner tail recursive*)
      in loop 2 n axiom matrix rp




    in let get_rp_axiom = function (Gic(_,_,(Conjunto(rp)),axiom)) -> (rp,axiom)
    in let (rp,axiom) = get_rp_axiom gic
    in let lvl1 = inicializar rp cadena
    in cyk_loop (List.length cadena) axiom [lvl1] rp

