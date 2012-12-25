type color = Red | Yellow | Blue | Orange | White | Green | None

type m_cube =
    { front : color;
      back : color;
      left : color;
      right : color;
      top : color;
      bottom : color }

let central_cube =
  { front  = None;
    back   = None;
    left   = None;
    right  = None;
    top    = None;
    bottom = None }

let turn_x c =
  { front  = c.top;
    back   = c.bottom;
    left   = c.left;
    right  = c.right;
    top    = c.back;
    bottom = c.front }
	
let turn_X c =
  { front  = c.bottom;
    back   = c.top;
    left   = c.left;
    right  = c.right;
    top    = c.front;
    bottom = c.back }

let turn_y c =
  { front  = c.left;
    back   = c.right;
    left   = c.back;
    right  = c.front;
    top    = c.top;
    bottom = c.bottom }

let turn_Y c =
  { front  = c.right;
    back   = c.left;
    left   = c.front;
    right  = c.back;
    top    = c.top;
    bottom = c.bottom }

let turn_z c =
  { front  = c.front;
    back   = c.back;
    left   = c.top;
    right  = c.bottom;
    top    = c.right;
    bottom = c.left }

let turn_Z c =
  { front  = c.front;
    back   = c.back;
    left   = c.bottom;
    right  = c.top;
    top    = c.left;
    bottom = c.right }

let make_rects c =
  [(Geometry.create_rect (Geometry.create_point (-0.5) (-0.5)    0.5) Geometry.j Geometry.i, c.front);
   (Geometry.create_rect (Geometry.create_point (-0.5) (-0.5) (-0.5)) Geometry.i Geometry.j, c.back);
   (Geometry.create_rect (Geometry.create_point (-0.5) (-0.5) (-0.5)) Geometry.j Geometry.k, c.left);
   (Geometry.create_rect (Geometry.create_point    0.5 (-0.5) (-0.5)) Geometry.k Geometry.j, c.right);
   (Geometry.create_rect (Geometry.create_point (-0.5)    0.5 (-0.5)) Geometry.i Geometry.k, c.top);
   (Geometry.create_rect (Geometry.create_point (-0.5) (-0.5) (-0.5)) Geometry.k Geometry.i, c.bottom)]

(* Cube *)
type cube = m_cube array

type coord = { i : int; j : int; k : int }
let coord i j k = {i=i; j=j; k=k}

let cube_get cube coords =
  cube.(coords.i + coords.j * 3 + coords.k * 9)

let cube_set cube coords value =
  cube.(coords.i + coords.j * 3 + coords.k * 9) <- value

let create_cube =
  let c = Array.make 27 central_cube in
    for i = 0 to 2 do
      for j = 0 to 2 do
	for k = 0 to 2 do
	  let fr = if k = 2 then Red else None and
	      bk = if k = 0 then Orange else None and
	      lf = if i = 0 then Green else None and
	      rt = if i = 2 then Blue else None and
	      tp = if j = 2 then White else None and
	      bt = if j = 0 then Yellow else None in
	    cube_set c {i=i; j=j; k=k}
	      { front = fr; back = bk; left = lf; right = rt; top = tp; bottom = bt }
	done
      done
    done;
    c

let as_list cube =
  let result = ref [] in
    for i = 0 to 2 do
      for j = 0 to 2 do
	for k = 0 to 2 do
	  let coord = (coord i j k) in
	  let m_cube = cube_get cube coord in
	    result := !result @ [ (coord, m_cube) ]
	done
      done
    done;
    !result
;;

let swap cube a1 a2 a3 a4 o =
  let tmp = cube_get cube a1 in
    cube_set cube a1 (o (cube_get cube a2));
    cube_set cube a2 (o (cube_get cube a3));
    cube_set cube a3 (o (cube_get cube a4));
    cube_set cube a4 (o tmp);;

let turn_front cube l =
  swap cube {i=1;j=2;k=l} {i=2;j=1;k=l} {i=1;j=0;k=l} {i=0;j=1;k=l} turn_z;
  swap cube {i=0;j=2;k=l} {i=2;j=2;k=l} {i=2;j=0;k=l} {i=0;j=0;k=l} turn_z;
  cube;;

let turn_right cube l =
  swap cube {i=l;j=2;k=1} {i=l;j=1;k=0} {i=l;j=0;k=1} {i=l;j=1;k=2} turn_x;
  swap cube {i=l;j=2;k=2} {i=l;j=2;k=0} {i=l;j=0;k=0} {i=l;j=0;k=2} turn_x;
  cube;;

let turn_top cube l =
  swap cube {i=1;j=l;k=0} {i=2;j=l;k=1} {i=1;j=l;k=2} {i=0;j=l;k=1} turn_y;
  swap cube {i=0;j=l;k=0} {i=2;j=l;k=0} {i=2;j=l;k=2} {i=0;j=l;k=2} turn_y;
  cube;;

let turn_FRONT cube l =
  swap cube {i=1;j=2;k=l} {i=0;j=1;k=l} {i=1;j=0;k=l} {i=2;j=1;k=l} turn_Z;
  swap cube {i=0;j=2;k=l} {i=0;j=0;k=l} {i=2;j=0;k=l} {i=2;j=2;k=l} turn_Z;
  cube;;

let turn_RIGHT cube l =
  swap cube {i=l;j=2;k=1} {i=l;j=1;k=2} {i=l;j=0;k=1} {i=l;j=1;k=0} turn_X;
  swap cube {i=l;j=2;k=2} {i=l;j=0;k=2} {i=l;j=0;k=0} {i=l;j=2;k=0} turn_X;
  cube;;

let turn_TOP cube l =
  swap cube {i=1;j=l;k=0} {i=0;j=l;k=1} {i=1;j=l;k=2} {i=2;j=l;k=1} turn_Y;
  swap cube {i=0;j=l;k=0} {i=0;j=l;k=2} {i=2;j=l;k=2} {i=2;j=l;k=0} turn_Y;
  cube;;

type t_side = Right | Top | Front | RIGHT | TOP | FRONT
type t_layer = L0 | L1 | L2

type t_axe = AXE_x | AXE_y | AXE_z | AXE_X | AXE_Y | AXE_Z

type move =
    Turn of t_side * t_layer
  | Rotate of t_axe

let turn cube (p : move) =
  match p with
      Turn (side, layer) -> begin
	let ilayer = match layer with
	    L0 -> 0
	  | L1 -> 1
	  | L2 -> 2
	in
	  match side with
	      Right -> turn_right cube ilayer
	    | Top   -> turn_top   cube ilayer
	    | Front -> turn_front cube ilayer
	    | RIGHT -> turn_RIGHT cube ilayer
	    | TOP   -> turn_TOP   cube ilayer
	    | FRONT -> turn_FRONT cube ilayer
      end
    | Rotate a -> begin
	ignore (match a with
	    AXE_x -> begin ignore (turn_right cube 0); ignore (turn_right cube 1); ignore (turn_right cube 2) end
	  | AXE_y -> begin ignore (turn_top   cube 0); ignore (turn_top   cube 1); ignore (turn_top   cube 2) end
	  | AXE_z -> begin ignore (turn_front cube 0); ignore (turn_front cube 1); ignore (turn_front cube 2) end
	  | AXE_X -> begin ignore (turn_RIGHT cube 0); ignore (turn_RIGHT cube 1); ignore (turn_RIGHT cube 2) end
	  | AXE_Y -> begin ignore (turn_TOP   cube 0); ignore (turn_TOP   cube 1); ignore (turn_TOP   cube 2) end
	  | AXE_Z -> begin ignore (turn_FRONT cube 0); ignore (turn_FRONT cube 1); ignore (turn_FRONT cube 2) end);
	cube
      end

let apply (c : cube) lst =
  (* TODO: should be fold *)
  let new_cube = ref (Array.copy c) in
    List.iter (fun x ->
		 new_cube := (turn !new_cube x)
	      ) lst;
    !new_cube
;;
