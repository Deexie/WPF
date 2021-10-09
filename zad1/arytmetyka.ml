(*
   Arytmetyka
   Autor kodu: Aleksandra Martyniuk gr. 7
   Reviewer:  Wojciech Przytuła gr. 3
 *)

(*
  (a, b, true) oznacza przedzial <a, b>
  (a, b, false) oznacza (neg_infinity, a> u <b, infinity)
*)
type wartosc = float * float * bool


(* val cf : (float -> fpclass) -> (float -> fpclass) *)
let cf = classify_float


let not_a_number = 0. /. 0.


(* val wartosc_dokladnosc : float -> float -> wartosc *)  
let wartosc_dokladnosc x p =
  let a = x *. (100. -. p) /. 100. and b =  x *. (p +. 100.) /. 100.
  in (min a b, max a b, true)


(* val wartosc_od_do : float -> float -> wartosc *)
let wartosc_od_do x y =
  if (cf x = FP_infinite && cf y = FP_infinite) then
    ((neg_infinity, infinity, true) : wartosc)
  else
   let pocz = (x +. y) /. 2. -. (y -. x) /. 2.
   and kon = (x +. y) /. 2. +. (y -. x) /. 2.
   in ((pocz, kon, true) : wartosc)


(* val wartosc_dokladna : float -> wartosc *)
let wartosc_dokladna x = (x -. 0., x +. 0., true)


(* val in_wartosc : wartosc -> float -> bool *)
let in_wartosc ((beg, endd, t) : wartosc) x =
  if (cf beg = FP_nan) then false
  else
    if t = true then
      if (beg <= x && x <= endd) then true else false
    else
      if (x <= beg || endd <= x) then true else false


(* val min_wartosc : wartosc -> float *)
let min_wartosc ((beg, _, t) : wartosc) =
  if (cf beg = FP_nan) then beg
  else
    if t = true then beg else neg_infinity


(* val max_wartosc : wartosc -> float *)
let max_wartosc ((_, endd, t) : wartosc) =
  if (cf endd = FP_nan) then endd
  else
    if t = true then endd else infinity


(* sprawdza czy dany przedzial jest rowny (neg_infinity, infinity) *)
(* val czy_inf_inf : wartosc -> bool *)
let czy_inf_inf ((beg, endd, t) : wartosc) =
  if ((t = true && cf beg = FP_infinite && cf endd = FP_infinite) ||
    (t = false && beg = endd))
      then true else false


(* sprawdza czy dany przedzial jest rowny (0., 0., true) *)
(* val zero : wartosc -> bool *)
let zero ((beg, endd, t) : wartosc) =
  if (t = true && beg = 0. && beg = endd) then true else false


(* val sr_wartosc : wartosc -> float *)
let sr_wartosc ((beg, endd, t) : wartosc) =
  if (czy_inf_inf (beg, endd, t) = true) then (0. /. 0.)
  else (min_wartosc (beg, endd, t) +. max_wartosc (beg, endd, t)) /. 2.


(* val odwrotnosc : float -> float *)
let odwrotnosc a = (1. /. a)


(*
  zamienia 0. i (-0.) w argumencie w zaleznosci od tego, czy przedzial
  zbiega w nich do zera z lewej (przedzial zawiera liczby niedodatnie)
  czy z prawej strony (przedzial zawiera liczby nieujemne) odpowiednio na
  (-0.) lub 0.
  przedzial (0., 0., false) zamienia na (neg_infinity, infinity, true),
  poniewaz oba te zapisy sa rownowazne
*)
(* val zeruj : wartosc -> wartosc *)
let zeruj ((beg, endd, t) : wartosc) =
  if t = true then
    match (beg, endd) with
    | 0., 0. -> ((0., (-0.), t) : wartosc)
    | 0., _ -> ((0., endd, t) : wartosc)
    | _, 0. -> ((beg, (-0.), t) : wartosc)
    | _, _ -> ((beg, endd, t) : wartosc)
  else
    match (beg, endd) with
    | 0., 0. -> ((neg_infinity, infinity, true) : wartosc)
    | 0., _ -> (((-0.), endd, t) : wartosc)
    | _, 0. -> ((beg, 0., t) : wartosc)
    | _, _ -> ((beg, endd, t) : wartosc)


(*
  jesli dany przedzial jest rowny (a, b, false), gdzie a >= b,
  zostaje on zamieniony na rownowazny przedzial (neg_infinity, infinity, true)
*)
(* val scal : wartosc -> wartosc *)
let scal ((beg, endd, t) : wartosc) =
  if (t = false && beg >= endd) then
    ((neg_infinity, infinity, true) : wartosc)
  else ((beg, endd, t) : wartosc)


(* val plus : wartosc -> wartosc -> wartosc *)
let plus ((beg1, end1, t1) : wartosc) ((beg2, end2, t2) : wartosc) =
  if (cf beg1 = FP_nan || cf beg2 = FP_nan) then
    ((not_a_number, not_a_number, true) : wartosc)
  else
    match (t1, t2) with
    | true, true -> ((beg1 +. beg2, end1 +. end2, true) : wartosc) 
    | false, true -> scal ((beg1 +. end2, end1 +. beg2, false) : wartosc)
    | true, false -> scal ((beg2 +. end1, end2 +. beg1, false) : wartosc)
    | false, false -> ((neg_infinity, infinity, true) : wartosc)


(* val minus : wartosc -> wartosc -> wartosc *)
let minus ((beg1, end1, t1) : wartosc) ((beg2, end2, t2) : wartosc) =
  plus ((((-1.) *. end2), ((-1.) *. beg2), t2) : wartosc)
    ((beg1, end1, t1) : wartosc)


(* znajduje najwieksza z 4 wartosci *)
(* val maks : float -> float -> float -> float -> float *)
let maks a b c d =
  max a (max b (max c d))


(* znajduje najmniejsza z 4 wartosci *)
(* val mini : float -> float -> float -> float -> float *)
let mini a b c d =
  min a (min b (min c d))


(*
  laczy dwa dane przedzialy;
  funkcja dziala tylko dla przedzialow, ktore moga powstac w wyniku
  wykorzystywania funkcji dostepnych w programie, tj.
  zarowno argumenty jak i wynik funkcji moga byc wylacznie postaci
  (a, b, true) lub (a, b, false);
  przykladowo dla przedzialow (1., 2., true) i (5., 8., true) funkcja nie
  zwroci poprawnego wyniku, poniewaz nie bylby on typu wartosc
*)
(* val polacz : wartosc -> wartosc -> wartosc *)
let polacz ((beg1, end1, t1) : wartosc) ((beg2, end2, t2) : wartosc) =
  match (t1, t2) with
  | true, true -> 
    (match (cf beg1, cf end1, cf beg2, cf end2) with
    | FP_infinite, _, _, FP_infinite ->
      scal ((end1, beg2, false) : wartosc)
    | _, FP_infinite, FP_infinite, _ ->
      scal ((end2, beg1, false) : wartosc)
    | _, _, _, _ -> 
      ((min beg1 beg2, max end1 end2, true) : wartosc))
  | false, false -> scal ((max beg1 beg2, min end1 end2, false) : wartosc)
  | false, true ->
     (match (cf beg2, cf end2) with 
     | FP_infinite, _ -> scal ((max beg1 end2, end1, false) : wartosc)
     | _, FP_infinite -> scal ((beg1, min end1 beg2, false) : wartosc)
     | _, _ -> 
       if beg2 <= beg1 then
         scal ((max beg1 end2, end1, false) : wartosc)
       else 
         scal ((beg1, min beg2 end1, false) : wartosc))
  | true, false -> 
  (match (cf beg1, cf end1) with 
    | FP_infinite, _ -> scal ((max beg2 end1, end2, false) : wartosc)
    | _, FP_infinite -> scal ((beg2, min end2 beg1, false) : wartosc)
    | _, _ -> 
      if beg1 <= beg2 then
        scal ((max beg2 end1, end2, false) : wartosc)
      else 
        scal ((beg2, min beg1 end2, false) : wartosc))


(*
  znajduje iloczyn a b, jesli taki jest definiowany;
  w przeciwnym przypadku zwraca 0, kiedy dziel = false lub iloczyn czynnika 
  niezerowego i odwrotnosci zera, kiedy dziel = true
*)
(* val zn_il : float -> float -> bool -> float *)
let zn_il a b dziel =
  match (cf (a *. b)) with 
    |FP_nan ->
      if dziel = false then 0.
      else
        if (cf a = FP_zero) then b *. odwrotnosc a
        else a *. odwrotnosc b
    |_ -> a *. b


(* mnozy dwa pojedyncze przedzialy, tj. przedzialy typu (_, _, true) *)
(* val mn_na_poj : wartosc -> wartosc -> bool -> wartosc *)
let mn_na_poj  ((beg1, end1, _) : wartosc) ((beg2, end2, _) : wartosc) dziel =
  let minimum =
    mini (zn_il beg1 beg2 dziel) (zn_il beg1 end2 dziel) (zn_il end1 beg2 dziel)
      (zn_il end1 end2 dziel)
  and maksimum =
    maks (zn_il beg1 beg2 dziel) (zn_il beg1 end2 dziel) (zn_il end1 beg2 dziel)
      (zn_il end1 end2 dziel)
  in ((minimum, maksimum, true) : wartosc)


(*
  mnozy dwa przedzialy;
  argument dziel typu bool okresla, w ktorej funkcji - razy czy dz_na_poj 
  (odpowiedznio false lub true) zostala wywolana funkcja mnoz;
  funkcja mnoz przekazuje ten argument kolejnym funkcjom, ktore przekazuja  
  ja dalej, az do funkcji zn_il, ktora zwraca wartosc w zaleznosci od 
  wartosci logicznej dziel
*)
(* val mnoz : wartosc -> wartosc -> bool -> wartosc *)
let mnoz ((beg1, end1, t1) : wartosc) ((beg2, end2, t2) : wartosc) dziel =
  if (cf beg1 = FP_nan || cf beg2 = FP_nan) then
    ((not_a_number, not_a_number, true) : wartosc)
  else
    if (zero (beg1, end1, t1) || zero (beg2, end2, t2)) then (0., 0., true)
    else
      match (t1, t2) with 
      | true, true -> mn_na_poj (beg1, end1, t1) (beg2, end2, t2) dziel
      | true, false ->
        polacz (mn_na_poj ((beg1, end1, true) : wartosc)
			  ((neg_infinity, beg2, true) : wartosc) dziel)
               (mn_na_poj ((beg1, end1, true) : wartosc)
	                  ((end2, infinity, true) : wartosc) dziel)
      | false, true -> 
        polacz (mn_na_poj ((beg2, end2, true) : wartosc)
			  ((neg_infinity, beg1, true) : wartosc) dziel)
               (mn_na_poj ((beg2, end2, true) : wartosc)
	                  ((end1, infinity, true) : wartosc) dziel)
      | false, false -> 
        polacz (polacz (mn_na_poj ((neg_infinity, beg1, true) : wartosc) 
			  	  ((neg_infinity, beg2, true) : wartosc) dziel)
		       (mn_na_poj ((neg_infinity, beg1, true) : wartosc)
				  ((end2, infinity, true) : wartosc) dziel))
	       (polacz (mn_na_poj ((end1, infinity, true) : wartosc) 
				  ((neg_infinity, beg2, true) : wartosc) dziel)
		       (mn_na_poj ((end1, infinity, true) : wartosc)
				  ((end2, infinity, true) : wartosc) dziel))


(* mnozy dwa dane przedzialy *)
(* val razy : wartosc -> wartosc -> wartosc *)
let razy ((beg1, end1, t1) : wartosc) ((beg2, end2, t2) : wartosc) =
  mnoz (zeruj ((beg1, end1, t1) : wartosc))
    (zeruj ((beg2, end2, t2) : wartosc)) false


(* dzieli dwa pojedyncze przedzialy, tj. przedzialy typu (_, _, true) *)
(* val dz_na_poj : wartosc -> wartosc -> wartosc *)
let dz_na_poj ((beg1, end1, t1) : wartosc) ((beg2, end2, t2) : wartosc) =
  if (beg2 < 0. && end2 > 0.)
  then 
    polacz (mnoz ((beg1, end1, true) : wartosc) 
                 ((odwrotnosc beg2, odwrotnosc (-0.), true) : wartosc) true)
           (mnoz ((beg1, end1, true) : wartosc) 
                 ((odwrotnosc 0., odwrotnosc end2, true) : wartosc) true)
  else 
    mnoz ((beg1, end1, true) : wartosc)
         ((odwrotnosc beg2, odwrotnosc end2, true) : wartosc) true 


(* dzieli dwa dane przedzialy *)
(* val podzielic : wartosc -> wartosc -> wartosc *)
let podzielic ((beg11, end11, t11) : wartosc) ((beg22, end22, t22) : wartosc) =
  let ((beg1, end1, t1) : wartosc) = zeruj ((beg11, end11, t11) : wartosc)
  and ((beg2, end2, t2) : wartosc) = zeruj ((beg22, end22, t22) : wartosc)
  in
    if (cf beg1 = FP_nan || cf beg2 = FP_nan) then
      ((not_a_number, not_a_number, true) : wartosc)
    else
      if (zero (beg2, end2, t2)) then (not_a_number, not_a_number, true)
      else
        match (t1, t2) with
        | true, true -> 
          dz_na_poj ((beg1, end1, t1) : wartosc) ((beg2, end2, t2) : wartosc)   
        | true, false ->
          polacz (dz_na_poj ((beg1, end1, true) : wartosc)
		            ((neg_infinity, beg2, true) : wartosc))
                 (dz_na_poj ((beg1, end1, true) : wartosc)
		            ((end2, infinity, true) : wartosc))
        | false, true ->
          polacz (dz_na_poj ((neg_infinity, beg1, true) : wartosc)
		             ((beg2, end2, true) : wartosc))
                  (dz_na_poj ((end1, infinity, true) : wartosc)
		             ((beg2, end2, true) : wartosc))
        | false, false -> 
          polacz (polacz (dz_na_poj ((neg_infinity, beg1, true) : wartosc)
                                    ((neg_infinity, beg2, true) : wartosc))
                         (dz_na_poj ((neg_infinity, beg1, true) : wartosc)
                                    ((end2, infinity, true) : wartosc)))
                 (polacz (dz_na_poj ((end1, infinity, true) : wartosc)
                                    ((neg_infinity,beg2, true) : wartosc))
                         (dz_na_poj ((end1, infinity, true) : wartosc)
                                    ((end2, infinity, true) : wartosc)))

(* testy pochodza ze wspolnej puli testow *)

(* 
let is_nan x = compare x nan = 0;;

let a = wartosc_od_do (-1.) 1.            
let b = wartosc_dokladna (-1.)           
let c = podzielic b a                     
let d = plus c a                         
let e = wartosc_dokladna 0.              
let f = razy c e                         
let g = razy d e                         
let h = wartosc_dokladnosc (-10.) 50.    
let i = podzielic h e                    
let j = wartosc_od_do (-6.) 5.           
let k = razy j j                          
let l = plus a b                         
let m = razy b l                         
let n = podzielic l l                   
let o = podzielic l m                   
let p = razy o a                       
let q = plus n o                        
let r = minus n n                       
let s = wartosc_dokladnosc (-0.0001) 100. 
let t = razy n s;;                       

assert ((min_wartosc c, max_wartosc c) = (neg_infinity, infinity));
assert (is_nan (sr_wartosc c) );
assert (not (in_wartosc c 0.));
assert ((in_wartosc c (-1.)) && (in_wartosc c (-100000.)) && (in_wartosc c 1.) && 
  (in_wartosc c 100000.));
assert ((in_wartosc d 0.) && (in_wartosc d (-1.)) && (in_wartosc d (-100000.)) &&
  (in_wartosc d 1.) && (in_wartosc d 100000.));
assert ((min_wartosc f, max_wartosc f, sr_wartosc f) = (0., 0., 0.));
assert ((min_wartosc g, max_wartosc g, sr_wartosc g) = (0., 0., 0.));
assert ((min_wartosc h, max_wartosc h, sr_wartosc h) = (-15., -5., -10.));
assert (is_nan (min_wartosc i) && is_nan (sr_wartosc i) && is_nan (max_wartosc i));
assert ((min_wartosc k, max_wartosc k, sr_wartosc k) = (-30., 36., 3.));
assert ((min_wartosc n, max_wartosc n, sr_wartosc n) = (0., infinity, infinity));
assert ((min_wartosc o, max_wartosc o, sr_wartosc o) = 
  (neg_infinity, 0., neg_infinity));
assert ((min_wartosc p, max_wartosc p, is_nan (sr_wartosc p)) = 
  (neg_infinity, infinity, true));
assert ((min_wartosc q, max_wartosc q, is_nan (sr_wartosc q)) =
  (neg_infinity, infinity, true));
assert ((min_wartosc r, max_wartosc r, is_nan (sr_wartosc r)) =
  (neg_infinity, infinity, true));
assert ((min_wartosc t, max_wartosc t, sr_wartosc t) =
  (neg_infinity, 0., neg_infinity));;

let a = wartosc_od_do 0. infinity
let b = wartosc_dokladna 0.
let c = podzielic a b
let d = podzielic b b;;
assert ((is_nan(min_wartosc c), is_nan(max_wartosc c), is_nan (sr_wartosc c)) =
  (true, true, true));
assert ((is_nan(min_wartosc d), is_nan(max_wartosc d), is_nan (sr_wartosc d)) = 
  (true, true, true));;

let a = wartosc_od_do (-10.) 10.
let b = wartosc_od_do (-1.) 1000.
let c = podzielic a b;;
assert ((min_wartosc c, max_wartosc c, is_nan (sr_wartosc c)) = 
  (neg_infinity, infinity, true));;

let a = wartosc_od_do (-1.0) 1.0
let b = wartosc_dokladna 1.0
let c = podzielic b a
let d = wartosc_dokladna 3.0
let e = plus c d      
let f = podzielic b e 
let g = podzielic d a 
let h = podzielic g f 
let i = plus f g;;    

assert ((in_wartosc f 0.25, in_wartosc f 0.26, in_wartosc f 0.49,  in_wartosc f 0.50)
   = (true, false, false, true));
assert ((min_wartosc h, max_wartosc h, is_nan (sr_wartosc h), in_wartosc h 0.) = 
  (neg_infinity, infinity, true, true));
assert ((min_wartosc h, max_wartosc h, is_nan (sr_wartosc h), in_wartosc h 0.3) = 
  (neg_infinity, infinity, true, true));;

let jed = wartosc_dokladna 1.
let zero = wartosc_dokladna 0.;;
assert ((sr_wartosc zero, max_wartosc zero, min_wartosc zero) = (0.,0.,0.));;

let a = wartosc_od_do 0. 1. 
let b = podzielic a a      
let c = razy b zero;;      
assert ((sr_wartosc c, max_wartosc c, min_wartosc c) = (0.,0.,0.));;

let a = podzielic jed zero;;
assert (is_nan (min_wartosc a));
assert (is_nan (max_wartosc a));
assert (is_nan (sr_wartosc a));;

let a = wartosc_dokladnosc 1. 110.;; 
assert (in_wartosc a (-.0.1));
assert (in_wartosc a (2.1));;

let a = wartosc_od_do (-.3.) 0.  
let b = wartosc_od_do 0. 1.     
let c = podzielic a b;;         
assert (max_wartosc c = 0.);
assert (min_wartosc c = neg_infinity);
assert (sr_wartosc c = neg_infinity);;

let a = wartosc_od_do 1. 4.    
let b = wartosc_od_do (-.2.) 3. 
let c = podzielic a b          
let d = podzielic c b          
let e = plus d jed            
let f = sr_wartosc (podzielic jed (wartosc_dokladna 9.));; 
assert (is_nan (sr_wartosc d));
assert (in_wartosc d 0.12);
assert (not (in_wartosc d 0.));
assert (not (in_wartosc d (-0.125)));
assert (in_wartosc d f);
assert (not (in_wartosc e 1.));;
*)
