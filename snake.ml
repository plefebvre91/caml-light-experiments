#open "graphics";;

type direction = Left|Right|Up|Down|Iddle;;
type fragment == int*int;;
type snake == fragment list;;

let foi  = float_of_int;;
let iof = int_of_float;;

let dim_fragment = 20;;
let snake = ref [(200,200); (200+dim_fragment,200);(200+(dim_fragment*2),200);(200+(dim_fragment*3),200)];;
let apple = ref (100,100);;
let score = ref 0;;

let pause t = let u = unix__select [] [] [] t in ();;

let direction_from_point (a,b) (x,y) d =
	let dx = (x-a) and
			dy = (y-b) in
	if abs dx > abs dy then
		if dx>0 then Right else Left
	else
		if dy>0 then Up else Down;;

let rec next_position (a,b) dir = match dir with
	 Right->(a+dim_fragment,b)
	|Left->(a-dim_fragment,b)
	|Down->(a,b-dim_fragment)
	|Up->(a,b+dim_fragment);
	|_->next_position (a,b) dir;;

let add_fragment l dir =
	let m = next_position (hd l) dir in
	m::l;;

let rec shift_list l = match l with
	 []->[]
	|[a]->[]
	|t::q->t::shift_list q;;

let update_snake l dir =
	let np = next_position (hd !l) dir in
		l:=np::(shift_list  !l);;

let rec is_in a l = match l with
	[]->false;
	|t::q->if t=a then true else is_in a q;;

let check_collision () =
	let t = hd !snake in
	is_in t (tl !snake);;

let new_apple () =
	let x = (random__int (400/dim_fragment)) + 1 in
	let y = (random__int (400/dim_fragment)) + 1 in
	apple := (dim_fragment*x,dim_fragment*y);;

let eat () =
	let (sx,sy) = hd !snake in
	let (px,py) = !apple in
	(sx = px) && (sy = py);;

let draw_fragment (a,b) =
	fill_circle (a-dim_fragment/2) (b-dim_fragment/2) (dim_fragment/2);;

let draw_apple p =	set_color (rgb 100 200 40); draw_fragment p;set_color black;;

let rec draw_snake l c = match l with
	[]->()
	|t::q->set_color (rgb (255-c) (255-c) (255-c));draw_fragment t; draw_snake q (c+10);;

let print_score () =
	let s = string_of_int !score in
	let x,y = current_point () in
	moveto 0 0;
	set_color white;
	draw_string ("Score: "^s);
	set_color black;
	moveto x y;;

let game_over () =
	moveto 180 180;
	set_color white;
	draw_string "Perdu...";
	set_color black;;

let countdown () =
	set_color white;
	for i=1 to 3 do
		moveto (((i-1)*10)+180) 300;
		draw_string ((string_of_int (4-i))^", ");
		pause 1.0;
	done;;

let draw () =
	draw_snake !snake 255;
	draw_apple !apple;
	print_score();;

let game () =
	let is_running = ref true in
	let d = ref Left in
	new_apple();
	open_graph " 400x400";

	fill_rect 0 0 400 400;
	draw ();
	countdown ();

	while !is_running do
		fill_rect 0 0 400 400;
		let ev = wait_next_event [Mouse_motion;Poll] in
		let x,y = mouse_pos() in
		d:=direction_from_point (hd !snake) (x,y) !d;

		if eat() then
			begin
				snake :=	add_fragment !snake !d;
				new_apple ();
				incr score;
			end;

		is_running := not (check_collision());
		update_snake snake !d;
		draw();
		pause 0.19;
	done;
	game_over();;

game();;
