#open "graphics";;

type point = {x:float; y:float};;

(*Window size*)
let w,h = 500,500;;
let dim_graph = " "^(string_of_int w)^"x"^(string_of_int h);;
let zoom = 0.8;;
let centre_x, centre_y = l/2, h/2;;

let iof = int_of_float;;
let foi = float_of_int;;

let pixel_from_point p = ( iof (p.x*.(foi (w/2) *. zoom)) + w/2, iof (p.y*.(foi (h/2) *. zoom)) +h/2);;

let line_to p =
	let a,b = pixel_from_point p in
	lineto a b;;

let move_to p =
	let a,b = pixel_from_point p in
	moveto a b;;

let point p =
	let a,b = pixel_from_point p in
	fill_circle a b 3;;

let cart_de_pol p t = {x=p*.(cos t); y=p*.(sin t)};;

let axis () =
	set_color (rgb 200 200 200);
	moveto 0 (h/2);
	lineto l (h/2);
	moveto (l/2) h;
	lineto (l/2) 0;
	set_color black;;

let val_point f theta =
	let rho = f theta in
	cart_de_pol rho theta;;

let nb_theta = 1000;;
let pi = 4.0*.(atan 1.0);;

let draw_polar f =
	axis();
	let step = (2.0 *. pi) /. (foi nb_theta) in
	let val = make_vect nb_theta {x=0.0;y=0.0} in
	for i=0 to nb_theta-1 do
		let p = val_point f ((foi i)*.(step)) in
		val.(i) <- p;
	done;

	move_to val.(0);
	for i=1 to nb_theta-1 do
		line_to val.(i);
	done;;

let r0 = 1.0;;
let t0 = pi/.2.0;;
let f x = r0*.((sin x)/.(sin t0))*.((sin x)/.(sin t0));;
open_graph dim_graph;;
draw_polar f;;

let field_equation = make_vect 10 (f);;
for i=0 to 9 do
	let f x = ((foi i)/.5.0)*.((sin x)/.(sin t0))*.((sin x)/.(sin t0)) in
	field_equation.(i) <- f;
	draw_polar field_equation.(i);
done;;
