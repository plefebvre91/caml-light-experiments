#open "graphics";;

(*Window size*)
let w,h = 513,513;;
let dim_graph = " "^(string_of_int w)^"x"^(string_of_int h);;
let centre_x, centre_y = w/2, h/2;;

(*Square type square: center coords + edge*)
type square = {centre: float*float; edge: float};;

let square_zero = {centre=0.0,0.0; edge=0.};;

(*Base square*)
let base_square = {centre=0.0,0.0; edge=0.666666};;

(*List of square to draw*)
let squares = [base_square];;

let iof = int_of_float;;
let foi = float_of_int;;
let pixel_from_point (x,y) = ( iof (x*.(foi (w/2))) + w/2, iof (y*.(foi (h/2))) +h/2);;

let line_to (x,y) =
	let a,b = pixel_from_point (x,y) in
	lineto a b;;

let move_to (x,y) =
	let a,b = pixel_from_point (x,y) in
	moveto a b;;

let point (x,y) =
	let a,b = pixel_from_point (x,y) in
	fill_circle a b 3;;

let draw_square square =
	let a,b = (pixel_from_point square.centre) in
	let width, height = iof (square.edge*.(foi w)), iof (square.edge*.(foi h)) in
	fill_rect (a-width/2) (b-height/2) width height;;

let rec draw_square_list l = match l with
	[]->()
	|t::q->draw_square t; draw_square_list q;;

(*Create 4 squares from a given square*)
let split_square square =
	let centre_x,centre_y = square.centre and edge = square.edge in
	let new_squares = make_vect 5 square_zero in
	new_squares.(0) <- {centre = centre_x -. edge, centre_y +. edge; edge = edge /. 3.0};
	new_squares.(1) <- {centre = centre_x +. edge, centre_y +. edge; edge = edge /. 3.0};
	new_squares.(2) <- {centre = centre_x, centre_y; edge = edge /. 3.0};
	new_squares.(3) <- {centre = centre_x -. edge, centre_y -. edge; edge = edge /. 3.0};
	new_squares.(4) <- {centre = centre_x +. edge, centre_y -. edge; edge = edge /. 3.0};
	list_of_vect new_squares;;

(*Ajoute les squares a garder sur une liste de squares*)
let rec split_square_list l = match l with
	[]->failwith "Ne doit pas arriver"
	|[t]-> (split_square t)
	|t::q->(split_square t)@(split_square_list q);;

(*Split n times*)
let rec vicsek n l =
	if n=0 then l else vicsek (n-1) (split_square_list l);;

open_graph dim_graph;;
let final = vicsek 4 squares in
draw_square_list final;;
