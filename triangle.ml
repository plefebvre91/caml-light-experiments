#open "graphics";;

(*Window size*)
let w,h = 600, 600;;
let window_size = " "^(string_of_int w)^"x"^(string_of_int h);;

(*Origin point*)
let centre_x, centre_y = w/2, h/2;;

let iof = int_of_float;;
let foi = float_of_int;;

(*Base triangle*)
let first_triangle = ((0.5, 0.8),(0.9, 0.1), (0.1, 0.1));;

(*List containing the triangles to draw*)
let triangles = [first_triangle];;

(*Conversion point to pixel*)
let pixel_from_point (x,y) = (iof (x*.(foi (w/2))) + w/2, iof (y*.(foi (h/2))) +h/2);;

let line_to (x,y) =
	let a,b = pixel_from_point (x,y) in
	lineto a b;;

let move_to (x,y) =
	let a,b = pixel_from_point (x,y) in
	moveto a b;;

let draw_triangle (a,b,c) =
	move_to a;
	line_to b;
	line_to c;
	line_to a;;

(*Adds 3 triangles around a central triangles*)
let rec draw_triangles_list l = match l with
	[]->()
	|t::q->draw_triangle t; draw_triangles_list q;;

let centre (a,b) (c,d) = ((c+.a)/.2.0, (d+.b)/.2.0);;

(*Split the given triangle by adding new triangles in the list*)
let split (a,b,c) =
	let m1 = centre a b and
		m2 = centre b c and
		m3 = centre a c in
	[(a,b,c);(a,m1,m3); (b,m1,m2); (c,m2,m3)];;

(*Map split to a list*)
let rec split_list l = match l with
	[]->failwith "Should not happen"
	|[t]-> (split t)@l
	|t::q->(split t)@(split_list q);;

	(*Division process repeated n times on the list*)
let rec sierpinski n l =
	if n=0 then l else sierpinski (n-1) (split_list l);;

open_graph window_size;;
let final = sierpinski 8 triangles in draw_triangles_list final;;
