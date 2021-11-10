#open "graphics";;

(*Data*)
let foi=float_of_int;;
let iof=int_of_float;;
let pi=3.141592;;
let centre_x=500;;
let centre_y=400;;
let fact=250.0;;

let b=0.7;;  (*ellipse  x2+y2/b=1  *)
let nb_border=100;;
let d_max=ref 0.0;;

exception should_not_occur;;

let f t=(cos(t)*.(1.0+.b*.cos(2.0*.t)),sin(t)*.(1.0+.b*.cos(2.0*.t)));;

type point_vect={x:float;y:float};;
let example=(0.0,0.0);;
let max_pts=10000;;
let points=make_vect max_pts example;;
let nb_pts=ref 0;;

let add_vect (x2,y2) (x1,y1)=(x2+.x1,y2+.y1);;
let diff_vect (x2,y2) (x1,y1)=(x2-.x1,y2-.y1);;
let prod_par_scal (x,y) a=(x*.a,y*.a);;
let dist a b=let (x,y)=diff_vect a b in sqrt(x*.x+.y*.y);;

let pixel (x,y)=(centre_x+iof(x*.fact),centre_y+iof(y*.fact));;
let circle m=let (a,b)=(pixel m) in draw_circle a b 2;;
let move_to m=let (a,b)=(pixel m) in moveto a b;;
let line_to m=let (a,b)=(pixel m) in lineto a b;;

let add_point m=
   circle m ;
   points.(!nb_pts)<- m;
   incr nb_pts;;

let add_border n =
  let ecct=2.0*.pi/.(foi n) in
    for i=0 to n-1 do
      let t=ecct*.(foi i) in
      let m=(f t) in (add_point m);
    done;
    move_to points.(0);
    for i=1 to n-1
      do line_to points.(i);
    done;
  line_to points.(0);;

let max_border() =
  let d_max0 = ref (dist points.(0) points.(nb_border-1)) in
  for i=0 to nb_border-2 do
    let d=dist points.(i) points.(i+1) in
      if d> !d_max0 then d_max0:= d ;
  done;
  !d_max0;;

let array_from_border() =
  let t = make_vect nb_border 0 in
  for i=0 to nb_border-1 do
    t.(i)<-i;
  done;
  t;;

let best_choice t nbt=
  let nb2 = nbt/2 in
  let distmin=ref (dist points.(t.(0)) points.(t.(nb2)))
    and i0 = ref 0
    and j0=ref nb2 in
  for i=1 to (nb2-1) do
    let d=dist points.(t.(i)) points.(t.(i+nb2)) in
    if d< (!distmin)
    then begin
      distmin := d;
      i0 := i;
      j0 := i + nb2;
    end;
  done;
  (!i0,!j0);;

let add_segment a b =
  let d = dist a b and u = diff_vect b a in
    if d<=(!d_max) then begin
      move_to a;line_to b;
      ([||],0);
    end
    else begin
      let n=1+iof (d/.(!d_max)) in
        let v=prod_par_scal u (1.0/.(foi n)) and tab=make_vect (n-1) 0 in
          for i=1 to n-1 do
            let w=prod_par_scal v (foi i) in add_point (add_vect a w);
            tab.(i-1)<-(!nb_pts-1);
          done;
          move_to a;
          for i=0 to n-2 do
            line_to points.(tab.(i));
          done;
          line_to b;
          (tab,n-1);
    end;;

let rec mesh l nbl = match nbl with
 0|1|2->raise should_not_occur;
 |3   ->()
 |_   ->let (p,q)=best_choice l nbl in
        let (inter,nbinter) = add_segment points.(l.(p)) points.(l.(q))
        in
          let n1=(nbinter+q-p+1) in let l1=make_vect n1 0 in
          for i=0 to nbinter-1 do l1.(i)<-inter.(nbinter-1-i);done;
          for j=0 to q-p do  l1.(j+nbinter)<-l.(p+j);done;
          mesh l1 n1;
          let n2=(nbinter+p+1+nbl-q) in let l2=make_vect n2 0 in
          for i=0 to nbinter-1 do l2.(i)<-inter.(i);done;
          for j=0 to nbl-q-1 do  l2.(j+nbinter)<-l.(q+j);done;
          for j=0 to p do  l2.(j+nbinter+(nbl-q))<-l.(j);done;
          mesh l2 n2;;

open_graph " 1000x700";;
clear_graph();;
add_border nb_border;;
d_max:=max_border();;
let front=array_from_border();;
mesh front nb_border;;
