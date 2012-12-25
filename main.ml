(* -*- Mode: tuareg -*- *)

let layer_to_int layer =
  match layer with
      Cube.L0 -> 0
    | Cube.L1 -> 1
    | Cube.L2 -> 2

let int_to_layer i =
  match i with
      0 -> Cube.L0
    | 1 -> Cube.L1
    | _ -> Cube.L2

let int_to_side x =
  match x with
      0 -> Cube.Right
    | 1 -> Cube.Top
    | 2 -> Cube.Front
    | 3 -> Cube.RIGHT
    | 4 -> Cube.TOP
    | _ -> Cube.FRONT

let pi = 4. *. atan 1.;;

let transform =
  List.fold_left Geometry.combine Geometry.identity [
    (Geometry.r_x (pi /. 6.0));
    (Geometry.r_y (pi /. 4.0));
    (Geometry.sc 40.0);
    Geometry.m_y; ]

let create_buttons () =
  let buttons = Array.make 18 Geometry.empty_rect in
    for i = 0 to 2 do
      let fi = (float_of_int i) -. 1.5 in
	buttons.(i)    <- Geometry.create_rect (Geometry.create_point     fi (-2.5)    1.5) Geometry.j Geometry.i;
	buttons.(i+3)  <- Geometry.create_rect (Geometry.create_point    1.5     fi (-2.5)) Geometry.k Geometry.j;
	buttons.(i+6)  <- Geometry.create_rect (Geometry.create_point (-2.5)    1.5     fi) Geometry.i Geometry.k;
	buttons.(i+9)  <- Geometry.create_rect (Geometry.create_point     fi    1.5 (-2.5)) Geometry.i Geometry.k;
	buttons.(i+12) <- Geometry.create_rect (Geometry.create_point (-2.5)     fi    1.5) Geometry.j Geometry.i;
	buttons.(i+15) <- Geometry.create_rect (Geometry.create_point    1.5 (-2.5)     fi) Geometry.k Geometry.j;
    done;
    
    for i = 0 to 17 do
      buttons.(i) <- Geometry.apply buttons.(i) transform
    done;
    buttons

let cube = ref Cube.create_cube

type anim_turn_type = (Cube.move * Geometry.matrix) list
let animTurn = ref []
let anim_shots = ref 0

let buttons = create_buttons ()

let width = ref 0
let height = ref 0

(*===========================================================*)
let get_color c =
  match c with
      Cube.Red    -> `NAME "red"
    | Cube.Yellow -> `NAME "yellow"
    | Cube.Green  -> `NAME "green"
    | Cube.Blue   -> `NAME "blue"
    | Cube.White  -> `NAME "white"
    | Cube.Orange -> `NAME "orange"
    | Cube.None   -> `NAME "black"

let compare_rects a b =
  let r1 = Geometry.less_or_equal a b and
      r2 = Geometry.less_or_equal b a in
    if r1 && r2 || not r1 && not r2 then
      0
    else if r1 then
      -1
    else
      1

let expose drawing_area ev =

  let rects = ref [] in
    for i = 0 to 2 do
      for j = 0 to 2 do
	for k = 0 to 2 do
	  
	  let a : Geometry.matrix =
	    match !animTurn with
	      | [] -> Geometry.identity
              | (cm, anim) :: rest ->
		  match cm with
		      Cube.Rotate axe -> anim
		    | Cube.Turn (side, layer) ->
			if layer_to_int layer =
			  match side with
			      Cube.Right | Cube.RIGHT -> i
			    | Cube.Top   | Cube.TOP   -> j
			    | Cube.Front | Cube.FRONT -> k
			then anim
			else Geometry.identity
	  in
	  let mat = Geometry.combine (Geometry.combine transform a)
	    ( Geometry.tr (float_of_int (i-1)) (float_of_int (j-1)) (float_of_int (k-1)) ) and
              cube = Cube.cube_get !cube (Cube.coord i j k) in
	    
	    rects := List.append !rects
	      
              (List.map (fun (r, color) ->
	  		   (Geometry.apply r mat, color))
		 
		 (Cube.make_rects cube))
	      
	done
      done
    done;
    
    rects := List.sort 
      (fun (a, color1) -> fun (b, color2) -> compare_rects a b)
      !rects;

    (*
      let area = GdkEvent.Expose.area ev in
      let x = Gdk.Rectangle.x area in
      let y = Gdk.Rectangle.y area in
      let width = Gdk.Rectangle.width area in
      let height = Gdk.Rectangle.width area in
    *)
    drawing_area#misc#realize ();
    let drawing = new GDraw.drawable (drawing_area#misc#window) in
      
      drawing#rectangle ~x:0 ~y:0 ~width:!width ~height:!height ~filled:true ();
      
      List.iter (fun (rect, color) ->
		   if color != Cube.None then
		     let w = (float_of_int !width) /. 2.0 in
		     let h = (float_of_int !height) /. 2.0 in
		     let proj = Geometry.projection
		       (Geometry.apply rect (Geometry.tr w h 0.0)) in
		       drawing#set_foreground (get_color color);
		       drawing#polygon ~filled:true proj;
		       drawing#set_foreground `BLACK;
		       drawing#polygon ~filled:false proj)
	!rects;
      
      false
;;

(*===========================================================*)
let go move drawing_area =

  animTurn := List.map (fun m -> (m, Geometry.identity)) move;
  anim_shots := 0;
  ignore (GMain.Timeout.add 100
	    (fun () ->
	       anim_shots := !anim_shots + 1;
	       
	       if !anim_shots >= 10 then
		 begin
		   let res =
		     (match !animTurn with
			| (t, anim) :: rest ->
			    cube := Cube.apply !cube [ t ];
			    animTurn := rest;
			    anim_shots := 0;
			    true
			| [] -> false)
		   in
		     GtkBase.Widget.queue_draw drawing_area#as_widget;
		     res
		 end
	       else
		 begin
		   
		   ignore (match !animTurn with
			     | [] -> ()
			     | (move, anim) :: rest -> 
				 begin
				   
				   let r =
				     match move with
				       | Cube.Turn (Cube.Right, _) -> 0
				       | Cube.Turn (Cube.Top,   _) -> 1
				       | Cube.Turn (Cube.Front, _) -> 2
				       | Cube.Turn (Cube.RIGHT, _) -> 3
				       | Cube.Turn (Cube.TOP,   _) -> 4
				       | Cube.Turn (Cube.FRONT, _) -> 5
				       | Cube.Rotate Cube.AXE_x	   -> 0
				       | Cube.Rotate Cube.AXE_y	   -> 1
				       | Cube.Rotate Cube.AXE_z	   -> 2
				       | Cube.Rotate Cube.AXE_X	   -> 3
				       | Cube.Rotate Cube.AXE_Y	   -> 4
				       | Cube.Rotate Cube.AXE_Z    -> 5
				   in
				     
				   let anim_step = Geometry.r r (-. pi /. 20.0) in
				     animTurn := (move, (Geometry.combine anim anim_step)) :: rest
				 end);
	       
		   GtkBase.Widget.queue_draw drawing_area#as_widget;
		   true
		 end))
;;

let click_to_move ev =
  let button = GdkEvent.Button.button ev and
      x = (GdkEvent.Button.x ev) -. (float_of_int !width) /. 2.0 and
      y = (GdkEvent.Button.y ev) -. (float_of_int !height) /. 2.0 in
    
  let m = ref (-1) in
    for i = 0 to 17 do
      if Geometry.contains buttons.(i) x y then
	m := i
    done;

    if !m = -1 then
      None
    else
      begin

	if button == 1 then
	  begin
	    let layer = int_to_layer (!m mod 3) and
		side = int_to_side (!m / 3) in
              Some (Cube.Turn (side, layer))
	  end
	else (* if button == 2 then *)
	  begin
	    let axe = match !m / 3 with
		0 -> Cube.AXE_x
              | 1 -> Cube.AXE_y
              | 2 -> Cube.AXE_z
              | 3 -> Cube.AXE_X
              | 4 -> Cube.AXE_Y
              | _ -> Cube.AXE_Z
	    in
              Some (Cube.Rotate axe)
	  end
      end
;;

let on_click drawing_area ev =
  (match click_to_move ev with
    | None -> ()
    | Some move -> go [ move ] drawing_area );
  true
;;

let on_configure ev =
  width := GdkEvent.Configure.width ev;
  height := GdkEvent.Configure.height ev;
  true

let rndClicked drawing_area ev =
  let cnt = Random.int 15 + 5 in

    for i = 1 to cnt do
      cube := Cube.apply !cube [ Cube.Turn(
				   int_to_side (Random.int 6),
				   int_to_layer (Random.int 3)) ]
    done;
    GtkBase.Widget.queue_draw drawing_area#as_widget

let ftClicked drawing_area ev =
  let moves = Solver.solve !cube in
    go moves drawing_area;
    (* cube := Cube.apply !cube moves; *)
  GtkBase.Widget.queue_draw drawing_area#as_widget

(*===========================================================*)
let main () =
  Random.init (int_of_float (Unix.time()));
  let xml = Glade.create ~file:"mlCube.glade" () in
  let wnd = new GWindow.window
      (GtkWindow.Window.cast
   (Glade.get_widget xml ~name:"mainwindow")) in

  let drawing_area = new GMisc.drawing_area
    (GtkMisc.DrawingArea.cast
       (Glade.get_widget xml ~name:"drawingarea")) in
      
    ignore (wnd#connect#destroy ~callback:GMain.Main.quit);
    drawing_area#event#add [`EXPOSURE; `BUTTON_PRESS];
    ignore (drawing_area#event#connect#expose ~callback:(expose drawing_area) );
    ignore (drawing_area#event#connect#button_press ~callback:(on_click drawing_area) );
    ignore (drawing_area#event#connect#configure ~callback:on_configure );

    (let btn = new GButton.tool_button
       (GtkButton.ToolButton.cast
    (Glade.get_widget xml ~name:"button1")) in
       ignore (btn#connect#clicked ~callback:(rndClicked drawing_area) ));

    (let btn = new GButton.tool_button
       (GtkButton.ToolButton.cast
    (Glade.get_widget xml ~name:"button2")) in
       ignore (btn#connect#clicked ~callback:(ftClicked drawing_area) ));

    wnd#show () ;
    GMain.Main.main () ;;

let _ = main ()
