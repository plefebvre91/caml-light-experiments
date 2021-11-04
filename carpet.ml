#open "graphics";;

(*Window size*)
let w,h = 522, 522;;
let window_size = " "^(string_of_int w)^"x"^(string_of_int h);;

(*Origin point*)
let centre_x, centre_y = w/2, h/2;;

(*Square type: centre coords and edge*)
type square = {centre: float*float; cote: float};;

let square_zero = {centre=0.0,0.0; cote=0.};;

(*First square origin-centered, edge = 1/3*)
let first_square = {centre=0.0,0.0; cote=0.333333333};;

(*List containing the squares to draw*)
let squares = [first_square];;

let iof = int_of_float;;
let foi = float_of_int;;

(*Conversion point to pixel*)
let pixel_from_point (x,y) = (iof (x*.(foi (w/2))) + w/2, iof (y*.(foi (h/2))) +h/2);;

let line_to (x,y) =
	let a,b = pixel_from_point (x,y) in
	lineto a b;;

let move_to (x,y) =
	let a,b = pixel_from_point (x,y) in
	moveto a b;;

let draw_squares square =
	let a,b = (pixel_from_point square.centre) in
	let larg,long = iof (square.cote*.(foi w)), iof (square.cote*.(foi h)) in
	fill_rect (a-larg/2) (b-long/2) larg long;;

let rec draw_squares_list l = match l with
	[]->()
	|t::q->draw_squares t; draw_squares_list q;;

(*Adds 8 sqaure around a central square*)
let add_squares square =
	let tab = make_vect 8 square_zero in
	let ind = ref 0 in

	let (centre_x, centre_y) = square.centre
	and	cote = square.cote in

	for i=0 to 2 do
		for j=0 to 2 do
			if (i,j) <> (1,1) then begin
				let ix = foi i and jy = foi j in
					tab.(!ind) <- {centre= (centre_x +. cote*.2. -. (ix *. 2. *. cote),
																 	centre_y +. cote*.2. -. (jy *. 2. *. cote));
												cote = cote/.3.0};
					incr ind;
			end;
		done;
	done;
	list_of_vect tab;;

(*Add 8 squares around each square in a list*)
let rec add_squares_list l = match l with
	[]->failwith "Should not happen"
	|[t]-> (add_squares t)@l
	|t::q->(add_squares t)@(add_squares_list q);;

(*Division process repeated n times on the list*)
let rec sierpinski n l =
	if n=0 then l else sierpinski (n-1) (add_squares_list l);;


open_graph window_size;;
let final = sierpinski 4 squares in
draw_squares_list final;;
