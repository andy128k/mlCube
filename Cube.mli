type color = Red | Yellow | Blue | Orange | White | Green | None

(* m_cube *)

type m_cube =
    { front : color;
      back : color;
      left : color;
      right : color;
      top : color;
      bottom : color }

val turn_x : m_cube -> m_cube
val turn_X : m_cube -> m_cube
val turn_y : m_cube -> m_cube
val turn_Y : m_cube -> m_cube
val turn_z : m_cube -> m_cube
val turn_Z : m_cube -> m_cube

val make_rects : m_cube -> (Geometry.rect * color) list

(* cube *)

type cube
val create_cube : cube
type coord = { i : int; j : int; k : int }
val coord : int -> int -> int -> coord
val cube_get : cube -> coord -> m_cube

val as_list : cube -> (coord * m_cube) list

type t_side = Right | Top | Front | RIGHT | TOP | FRONT
type t_layer = L0 | L1 | L2

type t_axe = AXE_x | AXE_y | AXE_z | AXE_X | AXE_Y | AXE_Z

type move =
    Turn of t_side * t_layer
  | Rotate of t_axe

val apply : cube -> move list -> cube
