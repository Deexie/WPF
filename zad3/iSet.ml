(*
 * ISet - Interval sets
 * Copyright (C) 1996-2003 Xavier Leroy, Nicolas Cannasse, Markus Mottl, Jacek Chrzaszcz
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version,
 * with the special exception on linking described in file LICENSE.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)

(** Interval Set.

    This is an interval set, i.e. a set of integers, where large
    intervals can be stored as single elements. Intervals stored in the
    set are disjoint. 

*)


(*
  zadanie: iSet
  autor: Aleksandra Martyniuk gr. 7
  reviewer: Karol Zagródka gr. 7
*)


(*
  typ zmodyfikowanych drzew AVL (setow);
  wierzcholek drzewa sklada sie z krotki zawierajacej:
  lewe poddrzewo, krotke, zawierajaca poczatek i koniec przedzialu w tym
  wierzcholku, prawe poddrzewo, wysokosc drzewa oraz liczbe elementow
  w drzewie (krancowe wartosci przedzialow tez sa elementami setu);
  Empty oznacza puste drzewo (poddrzewo);
  wysokosci lewego i prawego poddrzewa roznia sie o nie wiecej niz 2;
  przedzialy, zawarte w wierzcholkach danego setu sa rozlaczne i nie
  sa sasiadujace (tj. nie sa takie, ze koniec jednego przedzialu jest o 1
  mniejszy od poczatku innego przedzialu)
*)
type t =
  | Empty
  | Node of t * (int * int) * t * int * int


(* tworzy pusty set *)
(* val empty : t *)
let empty = Empty


(* zwraca [true] jesli set jest pusty *)
(* val is_empty : t -> bool *)
let is_empty x = 
  x = Empty


(*
  zwraca sume dwoch liczb, jesli taka miesci sie w zakresie int
  w przeciwnym przypadku zwraca min_int jesli suma jest mniejsza niz min_int
  lub max_int jesli suma jest wieksza niz max_int
*)
(* val sum : int -> int -> int *)
let sum a b =
  if a < 0 && b < 0 && min_int - a > b then min_int
  else if a > 0 && b > 0 && max_int - a < b then max_int
  else a + b


(*
  [length (a, b)] zwraca dlugosc przedzialu [(a, b)] (ilosc elementow od [a]
  do [b] wlacznie), jesli miesci sie ona w zakresie int w przeciwnym przypadku
  zwraca max_int
*)
(* val length : int * int -> int *)
let length (a, b) =
  if (a = min_int) then sum 2 (sum b (max_int))
  else sum 1 (sum b ((-1) * a))


(*
  zwraca krotke, zawierajaca poczatek i koniec przedzialu w danym wierzcholku
  setu lub (1, -1), jesli dany wierzcholek (set) jest pusty
*)
(* val beg_end : t -> int * int *)
let beg_end = function
  | Empty -> (1, -1)
  | Node (_, (b, e), _, _, _) -> (b, e)


(*
  przyjmuje liczbe elementow (lub max_int, jesli jest ich co najmniej max_int)
  w lewym poddrzewie drzewa, liczbe elementow  (lub max_int, jesli jest ich
  co najmniej max_int) w prawym poddrzewie drzewa oraz przedzial w korzeniu
  drzewa;
  zwraca liczbe elementow w drzewie, jesli miesci sie ona w zakresie int
  w przeciwnym przypadku zwraca max_int
*)
(* val count_n : int -> int -> int * int -> int *) 
let count_n n1 n2 k = sum n1 (sum n2 (length k))


(* zwraca wysokosc danego drzewa *)
(* val height : t -> int *)
let height = function
  | Node (_, _, _, h, _) -> h
  | Empty -> 0


(* zwraca liczbe elementow w danym drzewie *)
(* val num : t -> int *)
let num = function 
  | Node (_, _, _, _, n) -> n
  | Empty -> 0


(*
  [make l k r] tworzy drzewo, w ktorego korzeniu znajduje sie przedzial [k],
  [l] jest jego lewym, a [r] prawym poddrzewem
*)
(* val make : t -> int * int -> t -> t *)
let make l k r =
  Node (l, k, r, max (height l) (height r) + 1, count_n (num l) (num r) k)


(*
 dla danej liczby i przedzialu zwraca:
  0, jesli liczba zawiera sie w przedziale,
  -2, jesli liczba znajduje sie przed przedzialem (tj. jest mniejsza niz
  poczatek przedzialu), ale nie jest mniejsza o 1 od poczatku przedzialu,
  -1, jesli jest o 1 mniejsza od poczatku przedzialu,
  1, jesli jest o 1 wieksza od konca przedzialu
  2, jesli jest za przedzialem (tj. jest wieksza niz koniec przedzialu), ale
  nie jest wieksza o 1 od konca przedzialu
*)
(* val compare_with_int : int -> int * int -> int *) 
let compare_with_int x (beg, endd) =
  if (beg <= x && x <= endd) then 0 else
  if sum x 1 = beg then -1 else
  if sum x (-1) = endd then 1 else
  if sum x 1 < beg then -2 else 2


(*
  [bal l k r] przyjmuje dwa rozlaczne (nie majace wspolnych ani rozniacych
  sie o 1 elementow) sety [l] i [r], ktorych roznica wysokosci wynosi co
  najwyzej 4 i rozlaczny z nimi przedzial [k], takie, ze wszyskie elementy
  przedzialu [k] sa wieksze od wszystkich elementow setu [l] i mniejsze
  od wszystkich elementow setu [r];
  zwraca set zawierajacy wszystkie elementy setow [l], [r] i przedzialu [k];
*)
(* val bal : t -> int * int -> t -> t *)
let bal l k r =
  let hl = height l in
  let hr = height r in
  if hl > hr + 2 then
    match l with
    | Node (ll, lk, lr, _, _) ->
      if height ll >= height lr then make ll lk (make lr k r)
      else
        (match lr with
        | Node (lrl, lrk, lrr, _, _) ->
          make (make ll lk lrl) lrk (make lrr k r)
        | Empty -> assert false)
    | Empty -> assert false
  else if hr > hl + 2 then
    match r with
    | Node (rl, rk, rr, _, _) ->
      if height rr >= height rl then make (make l k rl) rk rr
      else
        (match rl with
        | Node (rll, rlk, rlr, _, _) ->
          make (make l k rll) rlk (make rlr rk rr)
        | Empty -> assert false)
    | Empty -> assert false
  else Node (l, k, r, max hl hr + 1,  count_n (num l) (num r) k)


(*
  [add_sep (b, e) s] przyjmuje przedzial [(b, e)] i set [s], takie, ze
  zaden element przedzialu <[b] - 1, [e] + 1> nie wystepuje w secie [s];
  zwraca set zawierajacy takie same elementy jak [s] oraz wszystkie elementy
  przedzialu [(b, e)] (poczatkowy set z dodanym przedzialem <[b], [e]>)
*)
(* val add_sep : int * int -> t -> t *)
let rec add_sep (beg, endd) = function
  | Node (l, k, r, _, _) ->
    let c = compare_with_int beg k in
    if c < 0 then
      let nl = add_sep (beg, endd) l in
      bal nl k r
    else
      let nr = add_sep (beg, endd) r in
      bal l k nr
  | Empty ->
    let x = (beg, endd) in
    Node (Empty, x, Empty, 1, length x)


(*
  [join l k r] przyjmuje dwa rozlaczne (nie majace wspolnych ani rozniacych
  sie o 1 elementow) sety [l] i [r] i rozlaczny z nimi przedzial [k], takie,
  ze wszyskie elementy przedzialu [k] sa wieksze od wszystkich elementow
  setu [l] i mniejsze od wszystkich elementow setu [r];
  zwraca set zawierajacy wszystkie elementy setow [l], [r] i przedzialu [k];
*)
(* val join : t -> int * int -> t -> t *)
let rec join l v r =
  match (l, r) with
  | (Empty, _) -> add_sep v r
  | (_, Empty) -> add_sep v l
  | (Node(ll, lv, lr, lh, _), Node(rl, rv, rr, rh, _)) ->
    if lh > rh + 2 then bal ll lv (join lr v r) else
    if rh > lh + 2 then bal (join l v rl) rv rr else
    make l v r

(*
  [split x s] przyjmuje liczbe x oraz set s i zwraca krotke [(l, present, r)]
  gdzie:
  [l] to set, zawierajacy elementy mniejsze niz [x], 
  [r] to set, zawierajacy elementy wieksze niz [x],
  [present] przyjmuje wartosc [true], gdy set s zawiera element rowny [x]
*)
(* val split : int -> t -> t * bool * t *)
let split x s =
  let rec loop x = function
    | Empty -> (Empty, false, Empty)
    | Node (l, (beg, endd), r, _, _) ->
      let c = compare_with_int x (beg, endd) in
      if c = 0 then 
        ((if beg < x then (add_sep (beg, x - 1) l) else l), true,
         (if x < endd then (add_sep (x + 1, endd) r) else r))
      else if c < 0 then
        let (ll, pres, rl) = loop x l in (ll, pres, join rl (beg, endd) r)
      else
        let (lr, pres, rr) = loop x r in (join l (beg, endd) lr, pres, rr)
  in
  loop x s


(* zwraca najmniejszy przedzial w secie (o najmniejszym pierwszym elemencie) *)
(* val min_elt : t -> int * int *)
let rec min_elt = function
  | Node (Empty, k, _, _, _) -> k
  | Node (l, _, _, _, _) -> min_elt l
  | Empty -> raise Not_found


(* usuwa najmniejszy przedzial w secie *)
(* val remove_min_elt : t -> t *)
let rec remove_min_elt = function
  | Node (Empty, _, r, _, _) -> r
  | Node (l, x, r, h, n) ->
    bal (remove_min_elt l) x r
  | Empty -> invalid_arg "PSet.remove_min_elt"


(* 
  [remove (a, b) s] przyjmuje przedzial [(a, b)] oraz set [s] i zwraca set,
  zawierajacy elementy zawarte w secie [s], ale nienalezace do przedzialu 
  [(a, b)]
*)
(* val remove : int * int -> t -> t *)
let remove (a, b) set = 
  let (left, _, r) = split a set in
  let (_, _, right) = split b r in
  match (left, right) with
  | Empty, Empty -> Empty
  | Empty, _ -> right
  | _, Empty -> left 
  | _ ->
    let lowest_of_right = min_elt right  in
    join left lowest_of_right (remove_min_elt right)


(*
  [sides x s] zwraca przedzial, w jakim znajdowalby sie element [x], gdyby do 
  setu [s] dodac przedzial ([x], [x]) (uwzgledniajac zasade, ze przedzialy 
  w secie musza byc rozlaczne i nie moga byc sasiadujace), o ile przedzial ten
  bylby rozny od ([x], [x]), gdyby byl rowny ([x], [x]), procedura zwroci 
  krotke (1, -1)
*)
(* val sides : int -> t -> int * int *)
let sides x set =
  let rec loop x = function
  | Empty -> (1, -1)
  | Node (l, (b, e), r, _, _) ->
    let c = compare_with_int x (b, e)
    in
    if c = 0 then (b, e) else
    if c = -2 then loop x l else
    if c = 2 then loop x r else
    if c = -1 then
      let (beg, endd) = beg_end l in
      if ((beg, endd) <> (1, -1) && sum endd 1 = x) then (beg, e)
      else (sum b (-1), e)
    else 
      let (beg, endd) = beg_end r in            
      if ((beg, endd) <> (1, -1) && sum beg (-1) = x) then (b, endd)
      else (b, sum e 1)
  in loop x set


(*
  [add (x, y) s] przyjmuje krotke [(x, y)] i zwraca set zawierajacy wszytkie 
  elementy setu [s] oraz wszystkie elementy przedzialu [(x, y)]
*)
(* val add : int * int -> t -> t *)
let add (x, y) set =
  let (b1, e1) = sides x set
  and (b2, e2) = sides y set
  in
  if (b1 = 1 && e1 = -1 && b2 = 1 && e2 = -1) then
    add_sep (x, y) (remove (x, y) set)
  else if (b1 = 1 && e1 = -1) then
    add_sep (x, e2) (remove (x, e2) set)
  else if (b2 = 1 && e2 = -1) then
    add_sep (b1, y) (remove (b1, y) set)
  else 
    add_sep (b1, e2) (remove (b1, e2) set)


(* [mem x s]  zwraca true, jesli [x] jest elementem setu [s] *)
(* val mem : int -> t -> bool *)
let mem x set =
  let rec loop = function
    | Node (l, (b, e), r, _, _) ->
       ((b <= x && x <= e) || loop (if x < b then l else r))
    | Empty -> false in
  loop set


(* 
  [below n s] zwraca liczbe elementow set [s], ktore sa mniejsze lub rowne [n],
  jesli liczba ta miesci sie w zakresie int, w przweciwnym przypadku zwraca 
  max_int
*)
(* val below : int -> t -> int *)
let below n set =
  let rec loop acc = function
    | Node (l, (b, e), r, _, _) ->
      let cmp = compare_with_int n (b, e) in 
        if cmp = 0 then count_n (num l) acc (b, n) 
        else if cmp < 0 then loop acc l
        else loop (count_n (num l) acc (b, e)) r
    | Empty -> acc
  in loop 0 set


(* 
  [iter f s] przyklada [f] do  wszystkich przedzialow setu [s] w kolejnosci 
  rosnacej
*)
(* val iter : (int * int -> unit) -> t -> unit *)
let iter f set =
  let rec loop = function
    | Empty -> ()
    | Node (l, k, r, _, _) -> loop l; f k; loop r in
  loop set


(*
  [fold f s a] oblicza [(f xN ... (f x2 (f x1 a))...)], gdzie x1
  ... xN sa przedzialami setu [s] w kolejnosci rosnacej
*)
(* val fold : (int * int -> 'a -> 'a) -> t -> 'a -> 'a *)
let fold f set acc =
  let rec loop acc = function
    | Empty -> acc
    | Node (l, k, r, _, _) ->
      loop (f k (loop acc l)) r in
  loop acc set


(* zwraca liste przedzialow danego setu posortowana w kolejnosci rosnacej *)
(* val elements : t -> (int * int) list *)
let elements set = 
  let rec loop acc = function
    | Empty -> acc
    | Node(l, k, r, _, _) -> loop (k :: loop acc r) l in
  loop [] set



(* testy pochodza ze wspolnej puli *)

(*
let zle = ref 0
let test n b =
  if not b then begin
    Printf.printf "Zly wynik testu %d!!\n" n;
    assert (false);
  end

let a = empty;;
let a = add (2, 5) a;;
let a = add (7, 10) a;;
let a = add (12, 20) a;;
let a = add (0, 0) a;;

test 1 (mem 1 a = false);;
test 2 (mem 2 a = true);;
test 3 (mem 9 a = true);;
test 4 (mem 21 a = false);;

let elem = elements a;;

test 5 (elem = [(0,0);(2,5);(7,10);(12,20)]);;
test 6 (below 6 a == 5);;
test 7 (below 10 a == 9);;
test 8 (below 19 a == 17);;

let (l,_,r) = split 15 a;;

test 9 (elements l = [(0,0);(2,5);(7,10);(12,14)]);;
test 10 (elements r = [(16,20)]);;

let (l,_,r) = split 8 a;;

test 11 (elements l = [(0,0);(2,5);(7,7);]);;
test 12 (elements r = [(9,10);(12,20)]);;


let a = add (6, 6) a;;
let b = add (11, 11) a;;

test 13 (elements a = [(0,0);(2,10);(12,20)]);;
test 14 (elements b = [(0,0);(2,20)]);;

let b = empty;;
let b = add (-10, 5) b;;
let b = add (10, 34) b;;
test 15 (elements b  = [(-10,5);(10,34)]);;

let b = add (22, 40) b;;
test 16 (elements b  = [(-10, 5);(10, 40)]);;

let b = add (41, 45) b;;
test 17 (elements b  = [(-10, 5);(10, 45)]);;

let b = add (80, 102) b;;
let b = add (130, 220) b;;
test 18 (elements b  = [(-10, 5);(10, 45);(80,102);(130,220)]);;

let b = add (45, 140) b;;
test 19 (elements b  = [(-10, 5);(10, 220)]);;


let c = empty;;
let c = add (4, max_int) c;;
let c = add (min_int, 0) c;;

test 20 (mem 4 c = true);;
test 21 (mem 0 c = true);;
test 22 (mem 20 c = true);;
test 23 (mem 2 c = false);;
test 24 (elements c = [(min_int, 0);(4, max_int)]);;
test 25 (below 0 c = max_int);;
test 26 (below max_int c = max_int);;


let d = empty;;
let d = add (min_int, max_int) d;;

test 27 (below 0 c = max_int);;
test 28 (below (-2) c = max_int);;
test 29 (below min_int c = 1);;
test 30 (below (min_int+1) c = 2);;


let a = remove (5,6) a;;
test 50 (elements a = [(0,0);(2,4);(7,10);(12,20)]);;
test 51 (below 11 a = 8);;
let (l,x,_) = split 1 a;;
test 52 ((elements l, x) = ([(0,0)], false));;
let (_,x,r) = split 20 a;;
test 53 ((x, r) = (true, empty));;

let pom = a;;


let a = remove (12,19) a;;
test 54 (elements a = [(0,0);(2,4);(7,10);(20,20)]);;
test 55 (mem 19 a = false);;
test 56 (below 19 a = 8);;
test 57 (below 1 a = 1);;
test 58 (below 5 a = 4);;

let (l, x, r) = split 7 a;;
test 59 (x = true);;
test 60 (elements l = [(0,0);(2,4)]);;
test 61 (elements r = [(8,10);(20,20)]);;


let a = remove (1,1) a;;
test 62 (elements a = [(0,0);(2,4);(7,10);(20,20)]);;
test 63 (mem 19 a = false);;
test 64 (below 20 a = 9);;
test 65 (below 1 a = 1);;
test 66 (below 5 a = 4);;

let (l, x, r) = split 7 a;;
test 67 (x = true);;
test 68 (elements l = [(0,0);(2,4)]);;
test 69 (elements r = [(8,10);(20,20)]);;


let a = remove (0,20) a;;
test 70 (elements a = []);;
test 71 (below max_int a = 0);;
test 72 (is_empty a = true);;


(* elements pom = [(0,0);(2,4);(7,10);(12,20)] *)
let x = ref 0;;
let f (c, d) = 
	x := !x + c;;
iter f pom;;
test 100 (!x = 21);;

let x = fold (fun (c,d) acc -> acc + (d-c+1)) pom 0;;
test 101 (x = below 100 pom);;
*)
