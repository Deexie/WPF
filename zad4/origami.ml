(* 
  zadanie: origami
  autor: Aleksandra Martyniuk gr. 7
  reviewer: Piotr Prabucki gr. 7
*)


let ep = 0.000000001
let (#=) a b = (-1.) *. ep < a -. b && a -. b < ep
let (#<=) a b = a -. ep < b
let (#>=) a b = a +. ep > b


(* Punkt na płaszczyźnie *)
type point = float * float


(* Poskładana kartka: ile razy kartkę przebije szpilka wbita w danym punkcie *)
type kartka = point -> int


(* [odleglosc p1 p2] zwraca odleglosc miedzy punktami p1 i p2 *)
(* val odleglosc : point -> point -> float *)
let odleglosc ((x1, y1) : point) ((x2, y2) : point) =
  let roznica_x = (x2 -. x1) ** 2.
  and roznica_y = (y2 -. y1) ** 2. in
  sqrt (roznica_x +. roznica_y)


(* 
  [prostokat p1 p2] zwraca kartkę, reprezentującą domknięty
  prostokąt o bokach równoległych do osi układu współrzędnych i lewym
  dolnym rogu [p1] a prawym górnym [p2]. Punkt [p1] musi więc być
  nieostro na lewo i w dół od punktu [p2]. Gdy w kartkę tę wbije się 
  szpilkę wewnątrz (lub na krawędziach) prostokąta, kartka zostanie
  przebita 1 raz, w pozostałych przypadkach 0 razy 
*)
(* val prostokat : point -> point -> kartka *)
let prostokat ((x1, y1) : point) ((x2, y2) : point) : kartka = 
  function (x, y) ->
    if (x1 #<= x && x #<= x2 && y1 #<= y && y #<= y2) then 1 else 0


(* 
  [kolko p r] zwraca kartkę, reprezentującą kółko domknięte o środku
  w punkcie [p] i promieniu [r] 
*)
(* val kolko : point -> float -> kartka *)
let kolko p1 r : kartka =
  function p -> 
    if ((odleglosc p1 p) #<= r) then 1 else 0


(*
  [znajdz_odbicie p1 p2 p] znajduje wspolrzedne punktu polozonego symetrycznie
  do punktu [p] wzgledem prostej (ozn. k) wyznaczonej przez punkty [p1] i [p2]
*)
(* val znajdz_odbicie : point -> point -> point -> point *)
let znajdz_odbicie ((x1, y1) : point) ((x2, y2) : point) ((x, y) : point) =
  (* wspolczynniki A i B prostej (ozn. l) prostopadlej do prostej k *)
  let a = x1 -. x2 and b = y1 -. y2 in
  let c = a *. x +. b *. y (* (-1) * wyraz wolny prostej l *)
  and d = a *. y2 -. b *. x2 (* (-1) * wyraz wolny prostej k *)
  in (* punkt przeciecia (ozn. s) prostych k, l za pomoca wyznacznika *)
  let w = a ** 2. +. b ** 2. in
  let przec_x = (a *. c -. b *. d) /. w
  and przec_y = (a *. d +. b *. c) /. w 
  in (* przesuniecie punktu [p] dwukrotnie o wektor ps *)
  ((2. *. przec_x -. x, 2. *. przec_y -. y) : point) 


(* 
  [zloz p1 p2 k] składa kartkę [k] wzdłuż prostej przechodzącej
  przez punkty [p1] i [p2] (muszą to być różne punkty). Papier jest
  składany w ten sposób, że z prawej strony prostej (patrząc w kierunku
  od [p1] do [p2]) jest przekładany na lewą. Wynikiem funkcji jest
  złożona kartka. Jej przebicie po prawej stronie prostej powinno więc
  zwrócić 0. Przebicie dokładnie na prostej powinno zwrócić tyle samo,
  co przebicie kartki przed złożeniem. Po stronie lewej - tyle co przed
  złożeniem plus przebicie rozłożonej kartki w punkcie, który nałożył
  się na punkt przebicia. 
*)
(* val zloz : point -> point -> kartka -> kartka *)
let zloz ((x1, y1) : point) ((x2, y2) : point) (k : kartka) : kartka =
  function (x, y) -> 
    let x_1 = x2 -. x1 and x_2 = x -. x1
    and y_1 = y2 -. y1 and y_2 = y -. y1 in
    let rozn = x_1 *. y_2 -. x_2 *. y_1 in
    if rozn #= 0. then k (x, y)
    else if rozn < 0. then 0 
    else k (x, y) + k (znajdz_odbicie (x1, y1) (x2, y2) (x, y))


(*
  [skladaj [(p1_1,p2_1);...;(p1_n,p2_n)] k = 
  zloz p1_n p2_n (zloz ... (zloz p1_1 p2_1 k)...)] 
  czyli wynikiem jest złożenie kartki [k] kolejno wzdłuż wszystkich prostych 
  z listy 
*)
(* val skladaj : (point * point) list -> kartka -> kartka *)
let skladaj l k =
  List.fold_left (fun acc (p1, p2) -> zloz p1 p2 acc) k l 


(* testy pochodza ze wspolnej puli *)

(*
let test n b =
  if not b then begin
    Printf.printf "Zly wynik testu %d!!\n" n;
  end

let pr = prostokat (2.0, 2.0) (4.0, 5.0);;
test 1 ((pr (3.0,3.0)) =  1);;
test 2  ((pr (2.0,2.0)) =  1);;
test 3  ((pr (4.0,5.0)) =  1);;
test 4  ((pr (4.0,3.0)) =  1);;
test 5  ((pr (10.0, 0.0)) = 0);;
 
(*Prostokat zlozony na pol*)
let pr = (zloz (3.0,0.0) (3.0,10.0) pr);;

test 10 ((pr (3.0,5.0)) =  1);;
test 11 ((pr (4.0,5.0)) =  0);;
test 12 ((pr (2.5, 2.5)) = 2);;
test 13 ((pr (3.5, 2.5)) = 0);;
test 14 ((pr (3.0, 2.5)) = 1);;
test 15 ((pr (2.0, 5.0)) = 2);;
 
(*Kwadrat zlozony po przekatnej*)
let pr = prostokat (0.0, 0.0) (10.0, 10.0);;
let pr = zloz (-50.0, -50.0) (-49.0, -49.0) pr;;
test 20 ((pr (-1.0, -1.0)) = 0);;
test 21 ((pr (0.0, 0.0)) = 1);;
test 22 ((pr (10.0, 10.0)) = 1);;
test 23 ((pr (10.1, 10.1)) = 0);;
test 24 ((pr (5.0, 6.0)) = 2);;
test 25 ((pr (5.0, 10.1)) = 0);;
test 26 ((pr (0.0, 10.0)) = 2);;
test 27 ((pr (6.0, 5.0)) = 0);;
test 28 ((pr (10.0, 0.0)) = 0);;

let k = prostokat (1., 1.) (5., 4.) ;;
test 29 ((k (3., 3.)) = 1);;
test 30 ((k (1., 1.)) = 1);;
test 31 ((k (5., 4.)) = 1);;
test 32 ((k (1., 4.)) = 1);;
test 33 ((k (5., 1.)) = 1);;

let k = prostokat (0., 0.) (42., 29.);;
let k = skladaj [((15., 2.), (27., 26.)); ((15., 15.), (1., 27.)); ((4., 18.), (3., 18.))] k;;

test 34 ((k (11., 3.)) = 1);;
test 35 ((k (7., 1.)) = 1);;
test 36 ((k (-4., 6.)) = 1);;
test 37 ((k (2., 3.)) = 2);;
test 38 ((k (14., 6.)) = 2);;
test 39 ((k (-1., 14.)) = 2);;
test 40 ((k (5., 2.)) = 3);;
test 41 ((k (13., 8.)) = 4);;
test 42 ((k (1., 10.)) = 4);;
test 43 ((k (6., 15.)) = 8);;
test 44 ((k (3., 17.)) = 8);; 

let k = prostokat (-21., -14.5) (21., 14.5);;
let k = zloz (11., -17.) (11., 2.) k;;
let k = zloz (-3., 3.5) (-7., 8.5) k ;;
let k = zloz (-14., -9.5) (-4., -7.5) k;;

test 45 ((k (0., 10.)) = 0);;
test 46 ((k (-6., 6.5)) = 2);;
test 47 ((k (-16., -9.5)) = 2);;
test 48 ((k (-14., -1.5)) = 3);;
test 49 ((k (-13., -6.5)) = 4);;
test 50 ((k (3., 0.)) = 4);;
test 51 ((k (-1.5, -1.)) = 4);;
test 52 ((k (-3., -5.)) = 6);;
test 53 ((k (-0.7, -1.9)) = 7);;
test 54 ((k (2., -4.5)) = 8);;

let k = kolko (174., -63.) 132.;;
let k = zloz (218., -107.) (262., -19.) k;;
let k = skladaj [(174., 58.), (97., -30.); (86., -129.), (174., -151.); (141., -8.), (119., -96.); (119., -178.), (240., -62.)] k;;
let k = zloz (240., -40.) (218., 13.) k;;

test 55 ((k (170., 70.)) = 0);;
test 56 ((k (130., -30.)) = 0);;
test 57 ((k (220., -95.)) = 0);;
test 58 ((k (175., -30.)) = 1);;
test 59 ((k (180., 20.)) = 3);;
test 60 ((k (140., -74.)) = 3);;
test 61 ((k (149., -102.)) = 4);;
test 62 ((k (154., -100.)) = 5);;
test 63 ((k (165., -99.)) = 6);;
test 64 ((k (165., -104.)) = 7);;
test 65 ((k (182., -106.)) = 8);;
test 66 ((k (165., -108.)) = 9);;
test 67 ((k (166., -123.)) = 9);;
test 68 ((k (171., -115.)) = 10);;
*)
