(* Vector *)
type vector = float array

let create_vector x y z =
  let v = Array.make 4 0.0 in
    v.(0) <- x;
    v.(1) <- y;
    v.(2) <- z;
    v

let i = create_vector 1.0 0.0 0.0
let j = create_vector 0.0 1.0 0.0
let k = create_vector 0.0 0.0 1.0

let plus v1 v2 =
  create_vector
    (v1.(0) +. v2.(0))
    (v1.(1) +. v2.(1))
    (v1.(2) +. v2.(2))
    
let scalar_product v1 v2 =
  v1.(0) *. v2.(0) +. v1.(1) *. v2.(1) +. v1.(2) *. v2.(2)

let scale v f =
  create_vector (v.(0) *. f) (v.(1) *. f) (v.(2) *. f)
    
(* Point *)
type point = float array

let create_point x y z =
  let p = Array.make 4 0.0 in
    p.(0) <- x;
    p.(1) <- y;
    p.(2) <- z;
    p.(3) <- 1.0;
    p;;

let zero = create_point 0.0 0.0 0.0

let move p v =
  create_point (p.(0) +. v.(0)) (p.(1) +. v.(1)) (p.(2) +. v.(2))

let minus p1 p2 =
  create_vector (p1.(0) -. p2.(0)) (p1.(1) -. p2.(1)) (p1.(2) -. p2.(2))
	
(* Matrix *)
type matrix = float array array

(*
  a11 a12 a13 a14
  a21 a22 a23 a24
  a31 a32 a33 a34
  a41 a42 a43 a44 =
*)

let identity =
  let m = Array.make_matrix 4 4 0.0 in
    m.(0).(0) <- 1.0;
    m.(1).(1) <- 1.0;
    m.(2).(2) <- 1.0;
    m.(3).(3) <- 1.0;
    m;;

let r_x angle =
  let m = Array.make_matrix 4 4 0.0 in
  let c = cos angle and s = sin angle in
    m.(0).(0) <- 1.0;
    m.(3).(3) <- 1.0;
    m.(1).(1) <- c;
    m.(2).(2) <- c;
    m.(1).(2) <- s;
    m.(2).(1) <- -.s;
    m;;

let r_y angle =
  let m = Array.make_matrix 4 4 0.0 in
  let c = cos angle and s = sin angle in
    m.(1).(1) <- 1.0;
    m.(3).(3) <- 1.0;
    m.(0).(0) <- c;
    m.(2).(2) <- c;
    m.(0).(2) <- -.s;
    m.(2).(0) <- s;
    m;;

let r_z angle =
  let m = Array.make_matrix 4 4 0.0 in
  let c = cos angle and s = sin angle in
    m.(2).(2) <- 1.0;
    m.(3).(3) <- 1.0;
    m.(0).(0) <- c;
    m.(1).(1) <- c;
    m.(0).(1) <- s;
    m.(1).(0) <- -.s;
    m;;

exception NeverBe

let r axe angle =
  match axe with
      0 -> r_x angle
    | 1 -> r_y angle
    | 2 -> r_z angle
    | 3 -> r_x (-.angle)
    | 4 -> r_y (-. angle)
    | 5 -> r_z (-. angle)
    | _ -> raise NeverBe

let m_x =
  let m = Array.make_matrix 4 4 0.0 in
    m.(0).(0) <- -1.0;
    m.(1).(1) <- 1.0;
    m.(2).(2) <- 1.0;
    m.(3).(3) <- 1.0;
    m;;

let m_y =
  let m = Array.make_matrix 4 4 0.0 in
    m.(0).(0) <- 1.0;
    m.(1).(1) <- -1.0;
    m.(2).(2) <- 1.0;
    m.(3).(3) <- 1.0;
    m;;

let m_z =
  let m = Array.make_matrix 4 4 0.0 in
    m.(0).(0) <- 1.0;
    m.(1).(1) <- 1.0;
    m.(2).(2) <- -1.0;
    m.(3).(3) <- 1.0;
    m;;

let tr tx ty tz =
  let m = Array.make_matrix 4 4 0.0 in
    m.(0).(0) <- 1.0;
    m.(1).(1) <- 1.0;
    m.(2).(2) <- 1.0;
    m.(3).(3) <- 1.0;
    m.(0).(3) <- tx;
    m.(1).(3) <- ty;
    m.(2).(3) <- tz;
    m.(3).(3) <- 1.0;
    m;;

let sc factor =
  let m = Array.make_matrix 4 4 0.0 in
    m.(0).(0) <- factor;
    m.(1).(1) <- factor;
    m.(2).(2) <- factor;
    m.(3).(3) <- 1.0;
    m;;

let combine m1 m2 =
  let m = Array.make_matrix 4 4 0.0 in
    for i = 0 to 3 do
      for j = 0 to 3 do
        for k = 0 to 3 do
          m.(i).(j) <- m.(i).(j) +. m1.(i).(k) *. m2.(k).(j)
	done
      done
    done;
    m;;

let product m v =
  let r = create_vector 0.0 0.0 0.0 in
    for i = 0 to 3 do
      let s = ref 0.0 in
	for k = 0 to 3 do
          s := !s +. m.(i).(k) *. v.(k)
	done;
	r.(i) <- !s;
    done;
    r;;

(* Rect *)
type rect = { p : point; a : vector; b : vector }
    
let create_rect p a b =
  { p = p; a = a; b = b }

let empty_rect =
  { p = (create_point 0.0 0.0 0.0); a = i; b = j }
    
let apply r m =
  create_rect (product m r.p) (product m r.a) (product m r.b)

let less_or_equal r1 r2 =
  let z = r1.p.(2) +. (r1.a.(2) +. r1.b.(2)) /. 2.0 and
      w = r2.p.(2) +. (r2.a.(2) +. r2.b.(2)) /. 2.0 in
    z <= w;;

let contains r x y =
  let d = r.a.(0) *. r.b.(1) -. r.a.(1) *. r.b.(0) in
  let l = ((x -. r.p.(0)) *. r.b.(1) -. (y -. r.p.(1)) *. r.b.(0)) /. d in
    
    if l < 0.0 or l >= 1.0 then
      false
    else
      let m = ((y -. r.p.(1)) *. r.a.(0) -. (x -. r.p.(0)) *. r.a.(1)) /. d in
        not (m < 0.0 or m >= 1.0);;

let projection r =
   List.map (function p -> ((int_of_float p.(0)), (int_of_float p.(1))))
    [ r.p ; move r.p r.a ; move (move r.p r.a) r.b ; move r.p r.b ]
