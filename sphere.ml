#open "graphics";;

(*Type*)
type point = {x:float; y:float; z:float};;
type face == point*point*point;;
type surface == face list;;

let zoom = 300.0;;
let centre_x = 500 and centre_y = 500;;
let iof = int_of_float;;
let foi = float_of_int;;

let o = {x = 0.0; y = 0.0; z = 0.0};;
let a = {x = 1.0; y = 0.0; z = 0.0};;
let b = {x = 0.0; y = 1.0; z = 0.0};;
let c = {x = -. 1.0; y = 0.0; z = 0.0};;
let d = {x = 0.0; y = -. 1.0; z = 0.0};;
let e = {x = 0.0; y = 0.0; z = 1.0};;
let f = {x = 0.0; y = 0.0; z = -. 1.0};;

let f1 = (a,b,e);;
let f2 = (b,c,e);;
let f3 = (c,d,e);;
let f4 = (d,a,e);;
let f5 = (b,a,f);;
let f6 = (c,b,f);;
let f7 = (d,c,f);;
let f8 = (a,d,f);;

let octa = [f1;f2;f3;f4;f5;f6;f7;f8];;

let longitude = 60;;
let latitude = 30;;
let pi = 3.141592;;
let long = pi *. (foi longitude) /. 180.;;
let lat = pi *. (foi latitude) /. 180.;;

let k = {x= sin(lat) ; y= sin(lat)*.sin(long) ; z= cos(lat)};;
let i = {x= sin(long) ; y= -.cos(long) ; z= 0.};;
let j = {x= cos(long)*.cos(lat) ; y= sin(long)*.cos(lat) ; z= -.sin(lat)};;

(*Vector function*)
let dot m p = m.x *. p.x +. m.y *. p.y +. m.z *. p.z ;;

let new_coords m = {x= dot m i ; y= dot m j; z= dot m k};;

let norm m =
	let x,y,z = m.x,m.y,m.z in sqrt (x*.x +. y*.y +. z*.z);;

let normalize m =
	let n = norm m in {x = m.x /. n; y = m.y /. n; z = m.z/.n};;

let centre_of_gravity p m =	{
	x= (m.x +. p.x) /. 2.0;
	y = (m.y +. p.y) /. 2.0;
	z= (m.z+. p.z) /. 2.0
};;

let pixel_from p =
	let m = new_coords p in
	(centre_x + iof (m.x *. zoom), centre_y + iof (m.y *. zoom));;

let move_to a =
	let x,y = pixel_from a in
	moveto x y;;

let line_to a =
	let x,y = pixel_from a in
	lineto x y;;

let pushed_centre a b = normalize (centre_of_gravity a b);;

let split_in_4 (a,b,c) =
	let d = pushed_centre a b and
			e = pushed_centre b c and
			f = pushed_centre a c in
		((a,d,f), (b,e,d), (c,f,e), (d,e,f));;

let draw_face (a,b,c) =
	let a1, a2 = pixel_from a and
	b1, b2 = pixel_from b and
	c1, c2 = pixel_from c in
	set_color (rgb 255 120 0);
	fill_poly [|(a1,a2); (b1,b2); (c1,c2)|];
	set_color black;
	move_to a;
	line_to b;
	line_to c;
	line_to a;;

let rec draw_surface surf = match surf with
	[]-> ()
	|t::q -> draw_face t; draw_surface q;;

let rec explode surf = match surf with
	[]->[]
	|t::q -> let (f1,f2,f3,f4) = split_in_4 t in f1::f2::f3::f4::(explode q);;

let rec sphere n = match n with
	0->octa;
	|_->explode (sphere (n-1));;

open_graph " 1000x900";
draw_surface (sphere 3);;
