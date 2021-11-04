#open "graphics";;

(*Types*)
type point = {x:float; y:float; z:float};;
type face == point*point*point;;
type surface == face list;;

(*Constants*)
let pi = 3.141592;;
let start_t = 0.0;;
let end_t = 2.0*.pi;;
let start_u = 0.0;;
let end_u = 2.0*.pi;;
let nb_t = 32;;
let nb_u = 16;;
let R=3.;;
let r = 0.8;;
let foi = float_of_int;;
let iof = int_of_float;;
let f_tore t u = {x=(R +. r*.cos(u))*.cos(t);y=(R +. r*.cos(u))*.sin(t);z=r*.sin(u)};;
let centre_x = 400;;
let centre_y = 250;;
let zoom = 100.0;;

(*POV*)
let longitude = 0;;
let latitude = 120;;
let long = pi *. (foi longitude) /. 180.;;
let lat = pi *. (foi latitude) /. 180.;;

(*Unit vectors*)
let k = {x = sin(lat) ; y = sin(lat)*.sin(long) ; z = cos(lat)};;
let i = {x = sin(long) ; y = -.cos(long) ; z = 0.};;
let j = {x = cos(long)*.cos(lat) ; y = sin(long)*.cos(lat) ; z = -.sin(lat)};;

(*Vector functions*)
let dot m p = m.x *. p.x +. m.y *. p.y +. m.z *. p.z ;;
let proj m = {x=dot m i; y=dot m j; z=dot m k};;
let diff_vect a b = {x=a.x-.b.x; y=a.y-.b.y; z=a.z-.b.z};;

let prod_vect a b = {
	x=a.y *. b.z -.a.z*.b.y;
	y=a.z *. b.x -. a.x *. b.z;
	z=a.x *. b.y -. a.y *. b.x
};;

let norm m =
	let x,y,z = m.x,m.y,m.z in sqrt (x*.x +. y*.y +. z*.z);;

let normalize m =
	let n = norm m in {x = m.x /. n; y = m.y /. n; z = m.z/.n};;

let normal_vec a b c =
	let v = diff_vect b a and
	 w = diff_vect c b in
	 let n = normalize (prod_vect v w)in n;;

let vertex f a b c d nb_t nb_u i j =
	let t = a*.((foi nb_t) -. (foi i)) +.(foi i) *. b /. (foi nb_t) and
	 		u = c*.((foi nb_u) -. (foi j)) +.(foi j) *. d /. (foi nb_u) in
	proj(f t u);;

let depth (a,b,c) = a.z+.b.z+.c.z;;

let nearest t1 t2 =
	let z1 = depth t1 and z2 = depth t2 in z1<z2;;

let create_surface f a b c d nbt nbu =
	let l = ref [] in
	for i = 0 to (nb_t - 1) do
		for j = 0 to (nb_u - 1) do
		let m1 = vertex f a b c d nb_t nb_u i j
		 and m2 = vertex f a b c d nb_t nb_u (i+1) j
		 and m3 = vertex f a b c d nb_t nb_u (i+1) (j+1)
	   and m4 = vertex f a b c d nb_t nb_u i (j+1) in l:=(m1,m2,m3)::(m1,m3,m4)::(!l);
		done;
	done;
	!l;;

let tore = create_surface f_tore start_t end_t start_u end_u nb_t nb_u;;

let pixel_from p = (centre_x + iof (p.x *. zoom), centre_y + iof (p.y *. zoom));;

let move_to a =
	let x,y = pixel_from a in
	moveto x y;;

let line_to a =
	let x,y = pixel_from a in
	lineto x y;;

let draw_face (a,b,c) =
	let a1, a2 = pixel_from a and	b1, b2 = pixel_from b and c1, c2 = pixel_from c in
	let n = normal_vec a b c in
	let fact = 0.85 *. (abs_float n.z) +. 0.15 in
	let coul1 = rgb (iof (fact*.(foi 200))) (iof (fact*.(foi 200)))  (iof (fact*.(foi 0))) and
	coul2 = rgb (iof (fact*.(foi 235))) (iof (fact*.(foi 10)))  (iof (fact*.(foi 50))) in

	if n.z>0.0 then set_color coul1 else set_color coul2 ;
	fill_poly [|(a1,a2); (b1,b2); (c1,c2)|];
	set_color black;;

let rec draw surf = match surf with
	[]-> ()
	|t::q -> draw_face t; draw q;;

(*Merge sort*)
let rec split l = match l with
	[]->([],[])
	|[a]->([a],[])
	|t::u::q-> let l1,l2 = split q in (t::l1,u::l2);;

let rec merge l1 l2 shortest = match l1,l2 with
	[],[]->[]
	|[],_->l2
	|_,[]->l1
	|t::q,u::v->if shortest t u then t::(merge q (u::v) shortest) else u::(merge v (t::q) shortest);;

let rec merge_sort l shortest = match l with
	[]->[]
	|[a]->[a]
	|_-> let (l1,l2) = split l in merge (merge_sort l1 shortest) (merge_sort l2 shortest) shortest;;


open_graph " 800x500";;
let tore1 = create_surface f_tore start_t end_t start_u end_u nb_t nb_u ;;
let tore = merge_sort tore1 nearest;;
draw tore;;
