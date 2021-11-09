#open "graphics";;

(*Dimension de la fenetre*)
let l,h = 600,600;;
let dim_graph = " "^(string_of_int l)^"x"^(string_of_int h);;

(*Origine ramenee au centre*)
let centre_x, centre_y = l/2, h/2;;
let pi = 3.141592653589793;;

(*Carre de base, de cote 1/3, centre en (0,0)*)
let premiers_traits = [(-1.0,-.0.9), (1.0,-0.9)];;

(*Droite d'equation ax+by+c = 0*)
type droite = {a:float; b:float; c:float};;

let iof = int_of_float;;
let foi = float_of_int;;

(*Conversion point <->pixel*)
let pixel_de_point (x,y) = ( iof (x*.(foi (l/2))) + l/2, iof (y*.(foi (h/2))) +h/2);;

let va_en (x,y) = 
	let a,b = pixel_de_point (x,y) in
	lineto a b;;

let pointe_en (x,y) = 
	let a,b = pixel_de_point (x,y) in
	moveto a b;;

let point (x,y) =
	let a,b = pixel_de_point (x,y) in
	fill_circle a b 3;;

let norm (vx,vy) = let n = (sqrt (vx*.vx +. vy*.vy)) in
	(vx/.n,vy/.n);;

(*Milieu des points de coord. a,b et c,d*)
let milieu (xa,ya) (xb,yb) = ((xa+.xb)/.2.0, (ya+.yb)/.2.0);;

(*Equation de la mediatrce de [AB]*)
let mediatrice (xa,ya) (xb,yb) =
	let (xi,yi) = milieu (xa,ya) (xb,yb) in
	{a=xi-.xa; b=yi-.ya; c=xi*.(xa-.xi) +. yi*.(ya-.yi)};;

let longueur (xa,ya) (xb,yb) = sqrt ((xb-.xa)*.(xb-.xa) +. (yb-.ya)*.(yb-.ya));;

(*Ajoute le point C tq ABC soit equilateral*)
let complete_triangle a b =
	let d = mediatrice a b in
	let ix,iy = milieu a b in
	let l = (tan (pi/.3.)) *. (longueur a (ix,iy)) in
	let vx,vy =  norm (-.d.b,d.a) in	(ix+.l*.vx,iy+.l*.vy);;
	(*if vy>=0.0 then	(ix+.(l*.vx),iy+.l*.vy) else 	(ix-.l*.vx,iy-.l*.vy) *)

(*Dessine une liste de carres*)
let rec dessine_trait_liste l = match l with
	[]->()
	|t::q->let a,b = t in pointe_en a; va_en b; dessine_trait_liste q;;

(*Donne les points situes au 1/3 et 2/3 de [AB]*)
let points_inter (xa,ya) (xb,yb) =
	let x1 = xa +. ((xb-.xa)/.3.0) and
			y1 = ya +. ((yb-.ya)/.3.0) and
			x2 = xa +. (2.0*.(xb-.xa)/.3.0) and
			y2 = ya +. (2.0*.(yb-.ya)/.3.0) 
	in ((x1,y1), (x2,y2));;

(*Decoupe [MN] et ajoute le morceau de triangle*)
let decoupe_trait (m,n) =
	let (p1,p2) = points_inter m n in
		[(m,p1); (p1,(complete_triangle p1 p2)); (complete_triangle p1 p2),p2 ; p2,n];;

(*Ajoute les carres a garder sur une liste de carres*)
let rec decoupe_trait_liste l = match l with
	[]->failwith "Ne doit pas arriver"
	|[t]->(decoupe_trait  t)
	|t::q->(decoupe_trait t)@(decoupe_trait_liste q);;

(*Division appliquee it fois*)
let rec von_koch it l =
	if it=0 then l else von_koch (it-1) (decoupe_trait_liste l);;

open_graph dim_graph;
let fractale = von_koch 7 premiers_traits in
dessine_trait_liste fractale;;