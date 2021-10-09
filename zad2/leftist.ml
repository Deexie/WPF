(*
  Drzewa Lewicowe
  Autor kodu: Aleksandra Martyniuk gr. 7
  Reviewer: Mikołaj Grzebieluch gr. 1
*)


(* wyjątek podnoszony przez procedure delete_min, gdy kolejka jest pusta *)
exception Empty


(*
  typ zlaczalnej kolejki priorytetowej na drzewie lewicowym;
  wierzcholek drzewa lewicowego sklada sie z wartosci w tym wierzcholku
  oraz krotki zawierajacej poddrzewo (kolejke) z lewej strony wierzcholka,
  poddrzewo z prawej strony wierzcholka oraz dlugosc sciezki z prawej strony
  (bez tego wierzcholka);
  Null oznacza puste drzewo (poddrzewo)
*)
type 'a queue = Node of 'a * ('a queue * 'a queue * int) | Null


(* tworzy puste drzewo (kolejke priorytetowa) *)
(* val empty : 'a queue *)
let empty = Null


(*
  dla dwoch drzew lewicowych zwraca krotke zawierajaca drzewo o dluzszej
  prawej sciezce, drzewo o krotszej prawej sciezce oraz dlugosc sciezki
  jaka bedzie mialo drzewo lewicowe powstale z polaczenia drzew q1 i q2
  (do dlugosci prawej sciezki w poddrzewie nie wliczamy korzenia tego
  poddrzewa)
*) 
(* val max_min : 'a queue -> 'a queue -> 'a queue * 'a queue * int *)
let max_min q1 q2 =
  match (q1, q2) with
  | Null, _ -> (q2, Null, 0)
  | _, Null -> (q1, Null, 0)
  | Node (x1, (l1, r1, r_dist1)), Node (x2, (l2, r2, r_dist2)) ->
    if (r_dist1 > r_dist2) then (q1, q2, r_dist2 + 1) else (q2, q1, r_dist1 + 1)


(*
  laczy dwie kolejki priorytetowe (dwa drzewa lewicowe);
  zwrocone wartosc jest drzewem lewicowym;
*)
(* val join : 'a queue -> 'a queue -> 'a queue *)
let join queue1 queue2 =
  let rec joinPom q1 q2 =
    match (q1, q2) with
    | Null, Null -> Null
    | Null, _ -> q2
    | _, Null -> q1
    | Node (x1, (l1, r1, r_dist1)), Node (x2, (l2, r2, r_dist2)) ->
      if x1 < x2 then
        let poddrzewo = joinPom r1 q2
        in
          Node (x1, max_min l1 poddrzewo)
      else
        let poddrzewo = joinPom r2 q1
        in
          Node (x2, max_min l2 poddrzewo)
  in
    joinPom queue1 queue2


(* dodaje element x na kolejke priorytetowa *)
(* val add : 'a -> 'a queue -> 'a queue *)
let add x q = (join (Node (x, (Null, Null, 0))) q)


(*
  jesli kolejka nie jest pusta, usuwa z niej najmniejszy element i zwraca
  pare zawierajaca wartosc tego elementu i oraz kolejke tego elementu;
  jesli kolejka jest pusta, podnosi wyjatek Empty
*)
(* val delete_min : 'a queue -> 'a * 'a queue *)
let delete_min q =
  match q with
  | Null -> raise Empty
  | Node (x, (l, r, _)) -> (x, join l r)


(* zwraca true, jesli kolejka jest pusta i false, jesli nie jest pusta *)
(* val is_empty : 'a queue -> bool *)
let is_empty q = (q = Null)


(* testy pochodza ze wspolnej puli *)

(*
let a = empty;;
let b = add 1 empty;;

assert (is_empty a = true);;
assert (try let _=delete_min a in false with Empty -> true);;
assert (is_empty b <> true);;

let b = join a b ;;
assert (is_empty b <> true);;

let (x,y) = delete_min b;;

assert (x = 1);;
assert (is_empty y = true);;
assert (try let _=delete_min y in false with Empty -> true);;

let b = add 1 empty;;
let b = add 3 b;;
let b = add (-1) b;;
let b = add 2 b;;
let b = add 1 b;;

let (a,b) = delete_min b;;
assert (a = -1);;

let (a,b) = delete_min b;;
assert (a = 1);;

let (a,b) = delete_min b;;
assert (a = 1);;

let (a,b) = delete_min b;;
assert (a = 2);;

let (a,b) = delete_min b;;
assert (a = 3);;

assert(is_empty b = true);;

let b = add "a" empty;;
let b = add "aca" b;;
let b = add "nzbzad" b;;
let b = add "nzbza" b;;
let b = add "bxbxc" b;;

let (a,b) = delete_min b;;
assert (a = "a");;

let (a,b) = delete_min b;;
assert (a = "aca");;

let (a,b) = delete_min b;;
assert (a = "bxbxc");;

let (a,b) = delete_min b;;
assert (a = "nzbza");;

let (a,b) = delete_min b;;
assert (a = "nzbzad");;

assert(is_empty b = true);;
assert (try let _=delete_min b in false with Empty -> true);;

let b = add 1 empty;;
let b = add 3 b;;
let b = add (-1) b;;
let b = add 2 b;;
let b = add 1 b;;

let c = add 10 empty;;
let c = add (-5) c;;
let c = add 1 c;;
let c = add 4 c;;
let c = add 0 c;;

let b = join b c;;

let (a,b) = delete_min b;;
assert (a = (-5));;

let (a,b) = delete_min b;;
assert (a = (-1));;

let (a,b) = delete_min b;;
assert (a = 0);;

let (a,b) = delete_min b;;
assert (a = 1);;

let (a,b) = delete_min b;;
assert (a = 1);;

let (a,b) = delete_min b;;
assert (a = 1);;

let (a,b) = delete_min b;;
assert (a = 2);;

let (a,b) = delete_min b;;
assert (a = 3);;

let (a,b) = delete_min b;;
assert (a = 4);;

let (a,b) = delete_min b;;
assert (a = 10);;

assert (try let _=delete_min b in false with Empty -> true);;
*)












