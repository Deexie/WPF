(*
  zadanie : przelewanka
  autor : Aleksandra Martyniuk gr. 7
  reviewer : Michał Wiśniewski gr. 1
*)

exception Znalezione of int

open Array

let przelewanka tab =
  let tab_rozm = length tab in
  let q = Queue.create () in
  let h = Hashtbl.create 1000 in


  (*
    sprawdza czy konfiguracja koncowa zawiera przynajmniej jedno naczynie
    puste lub pelne (w przeciwnym przypadku uzyskanie konfiguracji koncowej
    jest niemozliwe)
  *)
  (* val pelny_czy_pusty : unit -> bool *)
  let pelny_czy_pusty () =
    fold_left (fun acc (a, b) -> (b = 0 || a = b || acc)) false tab
  in


  (*
    znajduje najwiekszy wspolny dzielnik wielkosci szklanek (pierwszych
    elementow par w tablicy [tab])
  *)
  (* val nwd : unit -> int *)
  let nwd () =
    let rec nwd_pom a b =
      if a = 0 then b else nwd_pom (b mod a) a
    in
    fold_left (fun acc (a, _) -> nwd_pom acc a) (fst tab.(0)) tab
  in


  (*
    sprawdza, czy stany koncowe (drugie elementy par tablicy [tab])
    sa podzielne przez nwd wielkosci szklanek (pierwszych elementow tych
    par) (w przeciwnym wypadku uzyskanie koncowej konfiguracji wody
    w szklankach jest niemozliwe)
  *)
  (* val sprawdz_nwd : unit -> bool *)
  let sprawdz_nwd () =
    let nwd1 = nwd () in
    if (nwd1 = 0) then true
    else
      fold_left (fun acc (_, b) -> (b mod nwd1) = 0 && acc) true tab
  in


  (* nalewa wode do a-tej szklanki *)
  (* val nalej : int array -> int -> int array *)
  let nalej t a =
    let nowe_t = copy t in
    nowe_t.(a) <- fst tab.(a);
    nowe_t
  in


  (* wylewa wode z a-tej szklanki *)
  (* val wylej : int array -> int -> int array *)
  let wylej t a =
    let nowe_t = copy t in
    nowe_t.(a) <- 0;
    nowe_t
  in


  (* przelewa wode z a-tej do b-tej szklanki (a <> b) *)
  (* val przelej : int array -> int -> int -> int array *)
  let przelej t a b =
    let do_b = min t.(a) ((fst tab.(b)) - t.(b))
    and nowe_t = copy t in
    nowe_t.(a) <- (t.(a) - do_b);
    nowe_t.(b) <- (t.(b) + do_b);
    nowe_t
  in


  (*
    sprawdza, czy stan [t] jest szukanym stanem koncowym;
    jesli tak, podnosi wyjatek [Znalezione dis]
  *)
  (* val porownaj : int array -> int -> unit *)
  let porownaj t dis =
    let (stan_koncowy, _) =
      fold_left (fun (x, y) (_, b) -> ((b = t.(y) && x), y + 1)) (true, 0) tab
    in
    if stan_koncowy then raise (Znalezione dis)
  in


  (*
    sprawdza, czy stan [t] byl osiagniety wczesniej;
    jesli nie, wrzuca ten stan na tablice haszujaca i kolejke
  *)
  (* val sprawdz : int array -> int -> unit *)
  let sprawdz t dis =
    if not (Hashtbl.mem h t) then begin
      Hashtbl.add h t dis;
      Queue.add (dis, t) q;
      porownaj t dis end
  in


  (* generuje stany, do jakich mozna przejsc w jednym ruchu ze stanu [t] *)
  (* val sasiadujace : int * int array -> unit *)
  let sasiadujace (dis, t) =
    for i = 0 to tab_rozm - 1 do
      if t.(i) <> 0 then
        sprawdz (wylej t i) (dis + 1);
      if t.(i) <> fst tab.(i) then
        sprawdz (nalej t i) (dis + 1);
      for j = 0 to tab_rozm - 1 do
        if j <> i then begin
          let przelane = przelej t i j in
          sprawdz przelane (dis + 1);
        end
      done;
    done
  in


  (*
    przeszukuje mozliwe do osiagniecia stany;
    zwraca ilosc ruchow, potrzebna do osiagniecia stanu koncowego lub -1,
    jesli osiagniecie tego stanu jest niemozliwe
  *)
  (* val bfs : unit -> int *)
  let bfs () =
    let t = Array.make tab_rozm 0 in
    try
      (porownaj t 0;
      Hashtbl.add h t 0;
      Queue.add (0, t) q;
      while not (Queue.is_empty q) do
        sasiadujace (Queue.pop q)
      done; (-1))
    with
      Znalezione x -> x
  in
  if tab_rozm = 0 then 0 else
    if pelny_czy_pusty () && sprawdz_nwd () then bfs () else -1


(* testy pochodza ze wspolnej puli *)

(*
let zle = ref 0
let test n a b =
  let res = przelewanka a in
  if res <> b then begin
    Printf.printf "Zly wynik testu %d!!\n" n;
    incr zle
  end;;

test 1 [||] 0;;
test 2 [|(0, 0);(1, 0);(2, 0);(3, 0)|] 0;;
test 3 [|(5,5);(3,3)|] 2;;
test 4 [|(2,2)|] 1;;
test 5 [|(3,1)|] (-1);;
test 6 [|(3,1);(2,2)|] 2;;
test 7 [|(5,1);(3,0);(2,0)|] 4;;

test 8 [|(6,2);(2,1);(10,3);(12,10)|] (-1);;
test 9 [|(2,2);(6,6);(8,1)|] (-1);;
test 10 [| (42,24); (24,9); (10,0) |] (-1);;
test 11 [| (15,6); (12,11); (0,0) |] (-1);;
test 12 [| (12,10); (20,6) |] (-1);;

test 13 [| (1,0); (2,0); (1,0); (1,1); (1,0); (0,0); (0,0); (0,0); (2,2) |] (2);;
test 14 [| (2,0); (2,0); (2,1); (0,0); (0,0); (0,0); (2,0); (0,0); (2,2); (0,0) |] (-1);;
test 15 [| (2,1); (6,1); (0,0); (4,3) |] (-1);;
test 16 [| (16,13); (2,1) |] (-1);;
test 17 [| (2,2); (0,0); (2,2); (2,0); (1,1); (2,2); (0,0) |] (4);;
test 18 [| (1,0); (1,1); (4,3); (5,2) |] (5);;
test 19 [| (1,0); (6,0); (0,0); (2,1); (2,0); (0,0) |] (2);;

test 20 [| (2,1); (4,3); (4,4); (0,0) |] (-1);;
test 21 [| (4,2); (2,2); (4,2) |] (4);;
test 22 [| (3,1); (2,1); (6,6) |] (6);;
test 23 [| (0,0); (1,1); (2,0); (0,0); (1,1); (1,0); (0,0); (1,0); (1,1); (1,0) |] (3);;
test 24 [| (3,2); (7,0); (7,5) |] (10);;
test 25 [| (1,0); (1,1); (0,0); (0,0); (2,2); (1,0); (1,0); (2,0) |] (2);;

test 26 [| (11,7); (2,1) |] (-1);;
test 27 [| (6,3); (1,0); (6,1) |] (7);;
test 28 [| (2,2); (5,2); (0,0); (1,1); (3,3) |] (4);;
test 29 [| (3,2); (6,5); (4,1) |] (-1);;
test 30 [| (4,4); (8,2) |] (-1);;
test 31 [| (1,0); (7,6); (7,1) |] (3);;

let _ = 
  if !zle <> 0 then 
      Printf.printf "\nBlednych testow: %d...\n" !zle;
;;
*)
