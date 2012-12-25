type vector
val create_vector : float -> float -> float -> vector
val i : vector
val j : vector
val k : vector
val plus : vector -> vector -> vector
val scalar_product : vector -> vector -> float
val scale : vector -> float -> vector

type point
val create_point : float -> float -> float -> point
val zero : point
val move : point -> vector -> point
val minus : point -> point -> vector

type matrix
val identity : matrix
val r_x : float -> matrix
val r_y : float -> matrix
val r_z : float -> matrix
val m_x : matrix
val m_y : matrix
val m_z : matrix
val tr : float -> float -> float -> matrix
val sc : float -> matrix
val r : int -> float -> matrix

val combine : matrix -> matrix -> matrix
val product : matrix -> vector -> vector

type rect
val create_rect : point -> vector -> vector -> rect
val empty_rect : rect
val apply : rect -> matrix -> rect
val less_or_equal : rect -> rect -> bool
val contains : rect -> float -> float -> bool
val projection : rect -> (int * int) list
