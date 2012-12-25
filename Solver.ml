module ColorSet = Set.Make (struct
			      type t = Cube.color
			      let compare = compare
			    end);;

exception Error
;;

let rec make_set list =
  match list with
    | head :: tail -> ColorSet.add head (make_set tail)
    |  _ -> ColorSet.empty
;;

let find (cube : Cube.cube) c1 c2 c3 =
  let s1 = make_set [ c1 ; c2 ; c3 ; Cube.None ] in

    fst (List.find (fun pair ->
		      let c = snd pair in
		      let s2 = make_set [ c.Cube.front ; c.Cube.back ;
					  c.Cube.left ; c.Cube.right ;
					  c.Cube.top ; c.Cube.bottom ] in
			ColorSet.equal s1 s2)
	   (Cube.as_list cube))
;;

let int_to_layer i =
  match i with
      0 -> Cube.L0
    | 1 -> Cube.L1
    | _ -> Cube.L2
;;

let solver_put_ft (cube : Cube.cube) =
  let fc = find cube
    (Cube.cube_get cube (Cube.coord 1 2 1)).Cube.top
    (Cube.cube_get cube (Cube.coord 1 1 2)).Cube.front
    Cube.None in

  let i = fc.Cube.i and
      j = fc.Cube.j and
      k = fc.Cube.k in

  let li = int_to_layer i and
      lk = int_to_layer k in

  let t = ref [] in

    if j = 2 then
      begin
	if i = 1 then
	  begin
	    t := !t @ [ Cube.Turn(Cube.Front, lk) ;
			Cube.Turn(Cube.Front, lk) ];
	    if k = 0 then
	      t := !t @ [ Cube.Turn(Cube.Top, Cube.L0) ;
			  Cube.Turn(Cube.Top, Cube.L0) ];
	  end
	else (* k = 1 *)
	  begin
	    t := !t @ [ Cube.Turn(Cube.Right, li) ;
			Cube.Turn(Cube.Right, li) ];
	    if i == 0 then
	      t := !t @ [ Cube.Turn(Cube.Top, Cube.L0) ]
	    else
	      t := !t @ [ Cube.Turn(Cube.TOP, Cube.L0) ]
	  end
      end
    else if j = 1 then
      begin
	if k = 0 then
	  begin
	    t := !t @ [ Cube.Turn(Cube.RIGHT, li) ];
	    if i == 0 then
	      t := !t @ [ Cube.Turn(Cube.Top, Cube.L0) ]
	    else
	      t := !t @ [ Cube.Turn(Cube.TOP, Cube.L0) ];
	    t := !t @ [ Cube.Turn(Cube.Right, li) ]
	  end
	else (* k = 2 *)
	  begin
	    t := !t @ [ Cube.Turn(Cube.Right, li) ];
	    if i == 0 then
	      t := !t @ [ Cube.Turn(Cube.Top, Cube.L0) ]
	    else
	      t := !t @ [ Cube.Turn(Cube.TOP, Cube.L0) ];
	    t := !t @ [ Cube.Turn(Cube.RIGHT, li) ]
	  end
      end
    else (* j = 0 *)
      begin
	if k = 2 then
	  () (* turn ? *)
	else if k = 0 then
	  begin
	      t := !t @ [ Cube.Turn(Cube.Top, Cube.L0) ;
			  Cube.Turn(Cube.Top, Cube.L0) ]
	  end
	else if i = 0 then
	  begin
	    t := !t @ [ Cube.Turn(Cube.Top, Cube.L0) ]
	  end
	else if i = 2 then
	  begin
	    t := !t @ [ Cube.Turn(Cube.TOP, Cube.L0) ]
	  end
      end;
    
    let new_cube = Cube.apply cube !t in

      (* now it on (1, 0, 2) (should be on (1, 2, 2)) *)
      if (Cube.cube_get new_cube (Cube.coord 1 2 1)).Cube.top != (Cube.cube_get new_cube (Cube.coord 1 0 2)).Cube.bottom then
	t := !t @ [ Cube.Turn(Cube.Front, Cube.L2) ;
		    Cube.Turn(Cube.Right, Cube.L2) ;
		    Cube.Turn(Cube.TOP,   Cube.L0) ;
		    Cube.Turn(Cube.RIGHT, Cube.L2) ];

      t := !t @ [ Cube.Turn(Cube.Front, Cube.L2) ;
		  Cube.Turn(Cube.Front, Cube.L2) ];
      
      !t
;;

let rec put_ftl cube =
  let fc = find cube
    (Cube.cube_get cube (Cube.coord 1 2 1)).Cube.top
    (Cube.cube_get cube (Cube.coord 1 1 2)).Cube.front
    (Cube.cube_get cube (Cube.coord 0 1 1)).Cube.left in
    
  let i = fc.Cube.i and
      j = fc.Cube.j and
      k = fc.Cube.k in
    
    if i = 0 && j = 2 && k = 2 then
      []
    else if j = 2 then (* top *)
      begin

	let t = 
	  match i, k with
	    | 0, 0 -> [ Cube.Turn(Cube.RIGHT, Cube.L0) ;
			Cube.Turn(Cube.TOP,   Cube.L0) ;
			Cube.Turn(Cube.Right, Cube.L0) ]
	    | 2, 0 -> [ Cube.Turn(Cube.RIGHT, Cube.L2) ;
			Cube.Turn(Cube.TOP,   Cube.L0) ;
			Cube.Turn(Cube.Right, Cube.L2) ]
	    | 2, 2 -> [ Cube.Turn(Cube.Right, Cube.L2) ;
			Cube.Turn(Cube.TOP,   Cube.L0) ;
			Cube.Turn(Cube.RIGHT, Cube.L2) ]
	    | _ -> raise Error
	in

	let new_cube = Cube.apply cube t in
	  t @ (put_ftl new_cube)
	    
      end
    else (* j = 0, bottom *)
      begin

	(if i = 0 then
	   (if k = 0 then
	      [ Cube.Turn(Cube.TOP, Cube.L0) ]
	    else (* k = 2 *)
	      [ Cube.Turn(Cube.TOP, Cube.L0) ;
		Cube.Turn(Cube.TOP, Cube.L0) ])
	 else (* i = 2 *)
	   (if k = 2 then
	      [ Cube.Turn(Cube.Top, Cube.L0) ]
	    else
	      []))
	  
	@ [ Cube.Turn(Cube.Right, Cube.L0) ;
	    Cube.Turn(Cube.Top,   Cube.L0) ;
	    Cube.Turn(Cube.Top,   Cube.L0) ;
	    Cube.Turn(Cube.RIGHT, Cube.L0) ]
	  
      end; (* if *)
;;

let correct_ftl acube =
  let cube = ref acube and
      cmds = ref []
  in

    while (Cube.cube_get !cube (Cube.coord 1 2 1)).Cube.top != (Cube.cube_get !cube (Cube.coord 0 2 2)).Cube.top do

      let t = [ Cube.Turn(Cube.Right, Cube.L0) ;
		Cube.Turn(Cube.Top,   Cube.L0) ;
		Cube.Turn(Cube.Top,   Cube.L0) ;
		Cube.Turn(Cube.RIGHT, Cube.L0) ;
		Cube.Turn(Cube.Front, Cube.L2) ;
		Cube.Turn(Cube.TOP,   Cube.L0) ;
		Cube.Turn(Cube.TOP,   Cube.L0) ;
		Cube.Turn(Cube.FRONT, Cube.L2) ]
      in
	cube := Cube.apply !cube t;
	cmds := !cmds @ t;

    done;
    !cmds
;;

let solver_top_layer acube =
  let cube = ref acube and
      cmds = ref [] in

    (* cross *)
    for i = 0 to 3 do
      let t = (solver_put_ft !cube) @ [ Cube.Rotate Cube.AXE_y ] in
	cube := Cube.apply !cube t;
	cmds := !cmds @ t;
    done;

    (* corners *)
    for i = 0 to 3 do
      let t = put_ftl !cube in
	cube := Cube.apply !cube t;
	cmds := !cmds @ t;

	let t2 = correct_ftl !cube in
	  cube := Cube.apply !cube t2;
 	  cmds := !cmds @ t2;

 	  let r = [ Cube.Rotate Cube.AXE_y ] in
	    cube := Cube.apply !cube r;
	    cmds := !cmds @ r
    done;

    !cmds
;;

let insert_into_middle_level_l =
  [ Cube.Turn(Cube.Right, Cube.L2) ;
    Cube.Turn(Cube.Top,   Cube.L0) ;
    Cube.Turn(Cube.RIGHT, Cube.L2) ;
    Cube.Turn(Cube.Top,   Cube.L0) ;
    Cube.Turn(Cube.FRONT, Cube.L2) ;
    Cube.Turn(Cube.TOP,   Cube.L0) ;
    Cube.Turn(Cube.Front, Cube.L2) ]
;;

let insert_into_middle_level_r =
  [ Cube.Turn(Cube.FRONT, Cube.L2) ;
    Cube.Turn(Cube.TOP,   Cube.L0) ;
    Cube.Turn(Cube.Front, Cube.L2) ;
    Cube.Turn(Cube.TOP,   Cube.L0) ;
    Cube.Turn(Cube.Right, Cube.L2) ;
    Cube.Turn(Cube.Top,   Cube.L0) ;
    Cube.Turn(Cube.RIGHT, Cube.L2) ]
;;

let rec middle_layer_1 cube =

  let c1 = (Cube.cube_get cube (Cube.coord 1 1 2)).Cube.front and
      c2 = (Cube.cube_get cube (Cube.coord 2 1 1)).Cube.right in

  let fc = find cube c1 c2 Cube.None in

  let i = fc.Cube.i and
      j = fc.Cube.j and
      k = fc.Cube.k in

    if j = 1 then
      begin

	let t = match i, k with
	  | 0, 0 ->
	      [ Cube.Rotate Cube.AXE_y ;
		Cube.Rotate Cube.AXE_y ]
	      @ insert_into_middle_level_l
	      @ [ Cube.Rotate Cube.AXE_Y ;
		  Cube.Rotate Cube.AXE_Y ]
	  | 0, 2 ->
	      [ Cube.Rotate Cube.AXE_y ]
	      @ insert_into_middle_level_l
	      @ [ Cube.Rotate Cube.AXE_Y ]
	  | 2, 0 ->
	      [ Cube.Rotate Cube.AXE_Y ]
	      @ insert_into_middle_level_l
	      @ [ Cube.Rotate Cube.AXE_y]
	  | 2, 2 ->
	      if (Cube.cube_get cube fc).Cube.front != c1 then
 	        insert_into_middle_level_l
	      else
		[]
	  | _ -> raise Error
	in

	  if t == [] then
	    []
	  else
	    let new_cube = Cube.apply cube t in
	      t @ (middle_layer_1 new_cube)
		
      end
    else
      begin (* j = 0 *)
    
	if (Cube.cube_get cube fc).Cube.bottom == c1 then
	  begin

	    (match i, k with
	       | 0, 1 -> [ Cube.Turn(Cube.TOP, Cube.L0) ]	
	       | 1, 0 -> []
	       | 1, 2 -> [ Cube.Turn(Cube.Top, Cube.L0) ; Cube.Turn(Cube.Top, Cube.L0) ]
	       | 2, 1 -> [ Cube.Turn(Cube.Top, Cube.L0) ]
	       | _ -> raise Error)

	    @ insert_into_middle_level_r

	  end
	else
	  begin
	    
	    (match i, k with
	       | 0, 1 -> []
	       | 1, 0 -> [ Cube.Turn(Cube.Top, Cube.L0) ]
	       | 1, 2 -> [ Cube.Turn(Cube.TOP, Cube.L0) ]
	       | 2, 1 -> [ Cube.Turn(Cube.Top, Cube.L0) ; Cube.Turn(Cube.Top, Cube.L0) ]
	       | _ -> raise Error)
	    
	    @ insert_into_middle_level_l

	  end

      end (* end if *)
;;

let solver_middle_layer acube =
  let cube = ref acube and
      cmds = ref [] in
    
    for i = 0 to 3 do
      let t = (middle_layer_1 !cube) @ [Cube.Rotate Cube.AXE_y] in
	cube := Cube.apply !cube t;
	cmds := !cmds @ t;
    done;
    !cmds
;;

let take_another c1 c2 center =
  if c1 == center then
    c2
  else
    c1
;;

(* A'C'B'A'BAC *)
let swap =
  [ Cube.Turn(Cube.TOP,   Cube.L0) ;
    Cube.Turn(Cube.Front, Cube.L2) ;
    Cube.Turn(Cube.Right, Cube.L2) ;
    Cube.Turn(Cube.TOP,   Cube.L0) ;
    Cube.Turn(Cube.RIGHT, Cube.L2) ;
    Cube.Turn(Cube.Top,   Cube.L0) ;
    Cube.Turn(Cube.FRONT, Cube.L2) ]
;;

let rec bottom_cross cube =
  let center = (Cube.cube_get cube (Cube.coord 1 0 1)).Cube.bottom in

  let e1 = (Cube.cube_get cube (Cube.coord 1 1 2)).Cube.front and
      e2 = (Cube.cube_get cube (Cube.coord 2 1 1)).Cube.right and
      e3 = (Cube.cube_get cube (Cube.coord 1 1 0)).Cube.back and
      e4 = (Cube.cube_get cube (Cube.coord 0 1 1)).Cube.left in
    
  let c1 = ref (take_another
		  (Cube.cube_get cube (Cube.coord 1 0 2)).Cube.front
		  (Cube.cube_get cube (Cube.coord 1 0 2)).Cube.bottom
		  center) and
      c2 = ref (take_another
		  (Cube.cube_get cube (Cube.coord 2 0 1)).Cube.right
		  (Cube.cube_get cube (Cube.coord 2 0 1)).Cube.bottom
		  center) and
      c3 = ref (take_another
		  (Cube.cube_get cube (Cube.coord 1 0 0)).Cube.back
		  (Cube.cube_get cube (Cube.coord 1 0 0)).Cube.bottom
		  center) and
      c4 = ref (take_another
		  (Cube.cube_get cube (Cube.coord 0 0 1)).Cube.left
		  (Cube.cube_get cube (Cube.coord 0 0 1)).Cube.bottom
		  center) in

  let t = ref [] and
      found = ref false in 
    while not !found do
      
      let diff =
	(if !c1 == e1 then 0 else 1)
	+ (if !c2 == e2 then 0 else 1)
	+ (if !c3 == e3 then 0 else 1)
	+ (if !c4 == e4 then 0 else 1) in

	if diff <= 2 then
	  found := true
	else
	  begin
	    t := !t @ [ Cube.Turn(Cube.TOP, Cube.L0) ];
	    let tmp = ref !c1 in
	      c1 := !c2;
	      c2 := !c3;
	      c3 := !c4;
	      c4 := !tmp
	  end
    done;

    let bin_diff =
      (if !c1 == e1 then 0 else 1)
      + (if !c2 == e2 then 0 else 2)
      + (if !c3 == e3 then 0 else 4)
      + (if !c4 == e4 then 0 else 8) in
      
      match bin_diff with
	| 0  -> !t
	| 5
	| 10 -> let nt = !t @ swap in
	    nt @ (bottom_cross (Cube.apply cube nt))
	| 3  -> !t @ [ Cube.Rotate Cube.AXE_Y ] @ swap
	| 6  -> !t @ [ Cube.Rotate Cube.AXE_Y ; Cube.Rotate Cube.AXE_Y ] @ swap
	| 9  -> !t @ swap
	| 12 -> !t @ [ Cube.Rotate Cube.AXE_y ] @ swap
	| _ -> raise Error
;;

let rotate_corners_clockwise =
  [ Cube.Turn(Cube.Right, Cube.L2) ;
    Cube.Turn(Cube.FRONT, Cube.L0) ;
    Cube.Turn(Cube.RIGHT, Cube.L0) ;
    Cube.Turn(Cube.Front, Cube.L0) ;
    Cube.Turn(Cube.RIGHT, Cube.L2) ;
    Cube.Turn(Cube.FRONT, Cube.L0) ;
    Cube.Turn(Cube.Right, Cube.L0) ;
    Cube.Turn(Cube.Front, Cube.L0) ]
;;

let rotate_corners_counterclockwise =
  [ Cube.Turn(Cube.FRONT, Cube.L0) ;
    Cube.Turn(Cube.RIGHT, Cube.L0) ;
    Cube.Turn(Cube.Front, Cube.L0) ;
    Cube.Turn(Cube.Right, Cube.L2) ;
    Cube.Turn(Cube.FRONT, Cube.L0) ;
    Cube.Turn(Cube.Right, Cube.L0) ;
    Cube.Turn(Cube.Front, Cube.L0) ;
    Cube.Turn(Cube.RIGHT, Cube.L2) ]
;;

let bottom_corner cube =
  let e1 = (Cube.cube_get cube (Cube.coord 1 1 2)).Cube.front and
      e2 = (Cube.cube_get cube (Cube.coord 2 1 1)).Cube.right and
      e3 = (Cube.cube_get cube (Cube.coord 1 0 1)).Cube.bottom in
  
  let fc = find cube e1 e2 e3 in
    match fc.Cube.i, fc.Cube.j, fc.Cube.k with
      | 2, 0, 2 -> []
      | 0, 0, 0 -> rotate_corners_clockwise
      | 2, 0, 0 -> rotate_corners_counterclockwise
      | 0, 0, 2 -> [ Cube.Rotate Cube.AXE_y ] @ rotate_corners_clockwise @ [ Cube.Rotate Cube.AXE_Y ]
      | _ -> raise Error
;;

let get_direction cube =
  let bottom = (Cube.cube_get cube (Cube.coord 1 0 1)).Cube.bottom and
      k = Cube.cube_get cube (Cube.coord 2 0 2) in

    if bottom == k.Cube.right then
      1 (* clockwise *)
    else if bottom == k.Cube.front then
      (- 1) (* counterclockwise *)
    else
      0
;;

let solver_bottom_layer acube =
  let cube = ref acube and
      cmds = ref [] in

    (* make cross *)
    begin
      let t = (bottom_cross !cube) in
	cube := Cube.apply !cube t;
	cmds := !cmds @ t;
    end;

    (* place corners *)
    begin
      let t2 = (bottom_corner !cube) @ [ Cube.Rotate Cube.AXE_Y ] in
	cube := Cube.apply !cube t2;
	cmds := !cmds @ t2;
	let t3 = (bottom_corner !cube) @ [ Cube.Rotate Cube.AXE_y ] in
	  cube := Cube.apply !cube t3;
	  cmds := !cmds @ t3;
    end;

    (* rotate cross *)
    begin
      let t4 =
	begin
	  let tt1 = (Cube.cube_get !cube (Cube.coord 2 1 1)).Cube.right != (Cube.cube_get !cube (Cube.coord 2 0 1)).Cube.right and
	      tt2 = (Cube.cube_get !cube (Cube.coord 1 1 2)).Cube.front != (Cube.cube_get !cube (Cube.coord 1 0 2)).Cube.front and
	      tt3 = (Cube.cube_get !cube (Cube.coord 0 1 1)).Cube.left != (Cube.cube_get !cube (Cube.coord 0 0 1)).Cube.left and
	      tt4 = (Cube.cube_get !cube (Cube.coord 1 1 0)).Cube.back != (Cube.cube_get !cube (Cube.coord 1 0 0)).Cube.back in
	    
	    List.fold_left List.append []
	      (List.map (fun x ->
			   if x then
			     [ Cube.Turn(Cube.Right, Cube.L2) ;
			       Cube.Turn(Cube.Top,   Cube.L1) ;
			       Cube.Turn(Cube.Right, Cube.L2) ;
			       Cube.Turn(Cube.Top,   Cube.L1) ;
			       Cube.Turn(Cube.Right, Cube.L2) ;
			       Cube.Turn(Cube.Top,   Cube.L1) ;
			       Cube.Turn(Cube.Right, Cube.L2) ;
			       Cube.Turn(Cube.Top,   Cube.L1) ;
			       
			       Cube.Turn(Cube.Top, Cube.L0) ]
			   else
			     [ Cube.Turn(Cube.Top, Cube.L0) ])
		 [ tt1 ; tt2; tt3; tt4 ])
	end
      in
	cube := Cube.apply !cube t4;
	cmds := !cmds @ t4;
    end;
    
    (* rotate corners *)
    begin
      let d1 = get_direction !cube and
	  d2 = get_direction (Cube.apply !cube [ Cube.Turn(Cube.Top, Cube.L0) ]) and
	  d3 = get_direction (Cube.apply !cube [ Cube.Turn(Cube.Top, Cube.L0) ; Cube.Turn(Cube.Top, Cube.L0) ]) and
	  d4 = get_direction (Cube.apply !cube [ Cube.Turn(Cube.TOP, Cube.L0) ]) in

      let directions = [ d1 ; d2 ; d3 ; d4 ] in
	
      let clockwise =
	[ Cube.Turn(Cube.RIGHT, Cube.L2) ;
	  Cube.Turn(Cube.Top,   Cube.L2) ;
	  Cube.Turn(Cube.Top,   Cube.L2) ;
	  Cube.Turn(Cube.Right, Cube.L2) ;
	  Cube.Turn(Cube.Front, Cube.L2) ;
	  Cube.Turn(Cube.Top,   Cube.L2) ;
	  Cube.Turn(Cube.Top,   Cube.L2) ;
	  Cube.Turn(Cube.FRONT, Cube.L2) ] and
	  counterclockwise =
	[ Cube.Turn(Cube.Front, Cube.L2) ;
	  Cube.Turn(Cube.Top,   Cube.L2) ;
	  Cube.Turn(Cube.Top,   Cube.L2) ;
	  Cube.Turn(Cube.FRONT, Cube.L2) ;
	  Cube.Turn(Cube.RIGHT, Cube.L2) ;
	  Cube.Turn(Cube.Top,   Cube.L2) ;
	  Cube.Turn(Cube.Top,   Cube.L2) ;
	  Cube.Turn(Cube.Right, Cube.L2) ]
      in

      let t5 = ref
	(List.fold_left List.append []
	   (List.map (fun direction ->
			match direction with
			  | 1   -> clockwise @ [ Cube.Turn(Cube.Top, Cube.L0) ]
			  | - 1 -> counterclockwise @ [ Cube.Turn(Cube.Top, Cube.L0) ]
			  | _   -> [ Cube.Turn(Cube.Top, Cube.L0) ])
	      directions))
      in

      let cc = List.fold_left (+) 0 directions in
	
	if cc < 0 then
	  t5 := !t5 @ clockwise @ clockwise @ clockwise
	else if cc > 0 then
	  t5 := !t5 @ counterclockwise @ counterclockwise @ counterclockwise;
	
	cube := Cube.apply !cube !t5;
	cmds := !cmds @ !t5;
    end;
    !cmds
;;

let solve acube =
  let cube = ref acube and
      cmds = ref [] in
  
    begin
      let t = (solver_top_layer !cube) in
	cube := Cube.apply !cube t;
	cmds := !cmds @ t;
    end;

    begin
      let t2 = (solver_middle_layer !cube) in
	cube := Cube.apply !cube t2;
	cmds := !cmds @ t2;
    end;

    begin
      let t3 = (solver_bottom_layer !cube) in
	cube := Cube.apply !cube t3;
	cmds := !cmds @ t3;
    end;

    !cmds
;;
