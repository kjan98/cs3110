open Graphics
open State
open Camlimages
open Images
open Jpeg
open Command
open Types
open Grid_3d


exception Restart
exception Quit

(* make the black background color for rules*)
let bbblack = rgb 3 3 3

(* make the black colored backround color *)
let bblack = rgb 32 32 32

(* make the color for score text *)
let score_color = rgb 0 255 205

(* make color for rectangles *)
let gray = rgb 151 151 151

(* make color for try*)
let annoying_green = rgb 0 255 0

(* make color for top plane *)
let top_plep = rgb 23 45 183

(* make color for middle plane *)
let mid_plep = rgb 71 0 140

(* make color for bottom plane*)
let bot_plep = rgb 139 37 61

(* [round (x, y) transforms the floating point values of (x, y) into ints. ]*)
let round (x, y) = int_of_float x, int_of_float y


(* [round_list lst transforms the floating point values of lst into ints. ]*)
let rec round_list = function
  | [] -> []
  | h :: t -> (h |> fst |> int_of_float, h |> snd |> int_of_float) :: round_list t

(* [array_of_image img] transforms a given image to a color color array.
   Citation:  Camlimages library
*)
let array_of_image img =
  match img with
  | Images.Index8 bitmap ->
    let w = bitmap.Index8.width
    and h = bitmap.Index8.height
    and colormap = bitmap.Index8.colormap.map in
    let cmap = Array.map (fun {r = r; g = g; b = b} -> Graphics.rgb r g b) colormap in
    if bitmap.Index8.transparent <> -1 then
      cmap.(bitmap.Index8.transparent) <- transp;
    Array.init h (fun i ->
        Array.init w (fun j -> cmap.(Index8.unsafe_get bitmap j i)))
  | Index16 bitmap ->
    let w = bitmap.Index16.width
    and h = bitmap.Index16.height
    and colormap = bitmap.Index16.colormap.map in
    let cmap = Array.map (fun {r = r; g = g; b = b} -> rgb r g b) colormap in
    if bitmap.Index16.transparent <> -1 then
      cmap.(bitmap.Index16.transparent) <- transp;
    Array.init h (fun i ->
        Array.init w (fun j -> cmap.(Index16.unsafe_get bitmap j i)))
  | Rgb24 bitmap ->
    let w = bitmap.Rgb24.width
    and h = bitmap.Rgb24.height in
    Array.init h (fun i ->
        Array.init w (fun j ->
            let {r = r; g = g; b = b} = Rgb24.unsafe_get bitmap j i in
            rgb r g b))
  | Rgba32 _ -> failwith "RGBA not supported"
  | Cmyk32 _ -> failwith "CMYK not supported"

(* [get_img img] returns an image according to input file name. *)
let get_img img =
  Jpeg.load img [] |> array_of_image |> make_image

(* [draw_wait_mgs ()] Tells the player to please wait as the computer calculates the next best move*)
let draw_wait_mgs () =
  (draw_image (get_img "imgs/pls_wait.jpg") 236 0;)

(* [rect_drawn x y width height] method used to draw the rectangle (magenta)
 * with the bottom left corner of the box starting at x and y with provided
 * [width] and [height]*)
let rect_drawn x y width height =
  (set_color magenta;
    set_line_width 9;
   draw_rect x y width height;)

(* [rect_drawn_white x y width height] method used to draw the rectangle (white)
 * with the bottom left corner of the box starting at [x] and [y] with provided
 * [width] and [height]*)
let rect_drawn_white x y width height =
  (set_color white;
   set_line_width 9;
   draw_rect x y width height;)

(* [rect_drawn_cyan x y width height] method used to draw the rectangle (cyan)
 * with the bottom left corner of the box starting at [x] and [y] with provided
 * [width] and [height]*)
let rect_drawn_cyan x y width height =
  (set_color cyan;
   set_line_width 9;
   draw_rect x y width height;)

(* [rect_drawn_bblack x y width height] method used to draw the rectangle (bblack)
 * with the bottom left corner of the box starting at [x] and [y] with provided
 * [width] and [height]*)
let rect_drawn_bblack x y width height =
  (set_color bblack;
  set_line_width 9;
   draw_rect x y width height;)

(* [rect_drawn_gray x y width height] method used to draw the rectangle (gray)
 * with the bottom left corner of the box starting at [x] and [y] with provided
 * [width] and [height]*)
let rect_drawn_gray x y width height =
  (set_color gray;
   set_line_width 2;
   draw_rect x y width height)

(* [rect_drawn_red x y width height] method used to draw the rectangle (red)
 * with the bottom left corner of the box starting at [x] and [y] with provided
 * [width] and [height]*)
let rect_drawn_red x y  =
  (set_color red;
   set_line_width 3;
   draw_rect x y 50 50 ;)

(* [mouse_up ()] helper method that waits until [Button_up] (an event where the
 *  mouse button is released) occurs, and return the status of the mouse *)
let mouse_up () =
let event_lst = [Graphics.Button_up] in
let mouse_status = wait_next_event event_lst  in
let x = mouse_status.mouse_x in
let y = mouse_status.mouse_y in
(x, y)

type choices = {mode : string; level : string; num_p: string; }

(* [get_coordinates_choices c] draws a white rectangle around the welcome page
 * options depending on the choices the user made*)
let get_coordinates_choices c =
  if c = "easy" then rect_drawn_white 310 260 108 44
  else if c = "medium" then rect_drawn_white 450 260 119 43
  else if c = "hard" then rect_drawn_white 600 260 81 43
  else if c = "normal" then rect_drawn_white 380 200 109 43
  else if c = "krazy" then rect_drawn_white 520 200 93 44
  else if c = "single" then rect_drawn_white 395 130 93 44
  else if c = "multi" then rect_drawn_white 520 130 82 44
  else ()

(* [get_choices ch x y] returns the choice of game the user wants to play and
 * the coordinates of the next mouse event *)
let rec get_choices ch x y=
  if ((x >= 310 && x <= (310 + 108)) && (y >= 260 && y <= (260 + 44))) then
    (get_coordinates_choices ch.level;(rect_drawn 310 260 108 44);
     let (xx, yy) = mouse_up () in get_choices {ch with level = "easy"} xx yy )
  else if ((x >= 450 && x <= (450 + 119)) && (y >= 260 && y <= (260 + 43))) then
    (get_coordinates_choices ch.level;(rect_drawn 450 260 119 43);
    let (xx, yy) = mouse_up () in get_choices {ch with level ="medium"} xx yy)
  else if (x >= 600 && x <= (600 + 81)) && (y >= 260 && y <= (260 + 43))then
    (get_coordinates_choices ch.level;(rect_drawn 600 260 81 43);
    let (xx, yy) = mouse_up () in get_choices {ch with level ="hard"} xx yy)
  else if (x >= 380 && x <= (380 + 109)) && (y >= 200 && y <= (200 + 43))then
    (get_coordinates_choices ch.mode;(rect_drawn 380 200 109 43);
    let (xx, yy) = mouse_up() in get_choices {ch with mode ="normal"} xx yy)
  else if (x >= 520 && x <= (520 + 93)) && (y >= 200 && y <= (200 + 44))then
    (get_coordinates_choices ch.mode;(rect_drawn 520 200 93 44);
    let (xx, yy) = mouse_up () in get_choices {ch with mode ="krazy"} xx yy)
  else if (x >= 395 && x <= (395 + 93)) && (y >= 130 && y <= (130 + 44))then
    (get_coordinates_choices ch.num_p;(rect_drawn 395 130 93 44);
    let (xx, yy) = mouse_up () in get_choices{ch with num_p = "single"} xx yy)
  else if ((x >= 520 && x <= (520 + 82)) && (y >= 130 && y <= (130 + 44)))then
    (get_coordinates_choices ch.num_p;(rect_drawn 520 130 82 44);
     let (xx, yy) = mouse_up() in get_choices{ch with num_p ="multi"} xx yy)
  else if ((x >= 700 && x <= (700 + 110)) && (x >= 40 && y <= (89))) then
    (ch, -1, -1)
  else if (not((x >= 380 && x <=(242+380)) && (y>=35 && y<=(35+69))))then
    (let (xx, yy) = mouse_up() in get_choices ch xx yy)
  else
     (ch,x,y)

(* [begin_game x y] draws the game screen*)
let begin_game ()=
  (clear_graph();
  draw_image (get_img "imgs/xxoo.jpg") 0 0;
  draw_image (get_img "imgs/TTTmain.jpg") 250 40;
  draw_image(get_img "imgs/hint.jpg") 800 555;
  draw_image(get_img "imgs/try.jpg") 134 555;
  draw_image(get_img "imgs/quit.jpg") 850 313;
   draw_image(get_img "imgs/restart.jpg") 50 313;)


(* [start_game ch x y] helper method for [init_welcome] gets the choices for kind
 * of game user decides to play and opens the game screen*)
let rec start_game ch x y=
  if(x >= 380 && x <=(242+380)) && (y>=35 && y<=(35+69)) then(
    let play_str = "play " ^ ch.num_p ^ " " ^ "python " ^ ch.level ^ " " ^ ch.mode in
    rect_drawn x y 90 90;
    begin_game ();
    play_str
    ) else if ((x>= 700 && x <= 810 ) &&(y >= 40 && y <= 89))then
    (clear_graph(); exit 0)
  else
    (let (choice, xa, ya) = get_choices ch x y in
     if ((xa >= 380 && xa <=(242+380)) && (ya>=35 && ya<=(35+69))) then
       (let play_str = "play " ^ choice.num_p ^ " " ^ "python " ^ choice.level ^ " " ^ choice.mode in
          rect_drawn x y 90 90;
          begin_game ();
          play_str)
     else if ( xa = -1 && ya = -1) then (clear_graph(); exit 0)
     else
       (let (xx, yy) = mouse_up() in start_game choice xx yy))

(* [init_welcome ()] displays the welcome screen of the game*)
let init_welcome () =
  let event_lst = [Graphics.Button_up] in
  set_color bbblack; draw_rect 0 0 1000 750; fill_rect 0 0 1000 750;
  draw_image (get_img "imgs/rules.jpg") 0 100; let _ = wait_next_event event_lst in
  clear_graph();
  draw_image (get_img "imgs/background_crop.jpeg") 0 0;

  draw_image (get_img "imgs/wilkommen-logo.jpg") 270 330;

  draw_image (get_img "imgs/quit.jpg") 700 40;

  draw_image (get_img "imgs/easy.jpg") 310 260;
  rect_drawn 310 260 108 44;

  draw_image (get_img "imgs/medium.jpg") 450 260;
  rect_drawn_white 450 260 119 43;

  draw_image (get_img "imgs/hard.jpg") 600 260;
  rect_drawn_white 600 260 81 43;

  draw_image (get_img "imgs/g.jpg") 380 200;
  rect_drawn 380 200 109 43;

  draw_image (get_img "imgs/krazy.jpg") 520 200;
  rect_drawn_white 520 200 93 44;

  draw_image (get_img "imgs/single.jpg") 395 130;
  rect_drawn_white 395 130 93 44;

  draw_image (get_img "imgs/multi.jpg") 520 130;
  rect_drawn 520 130 82 44;

  draw_image (get_img "imgs/group.jpg") 380 35;
  let ch = {mode = "normal"; level = "easy"; num_p= "multi"; } in
  let (xx, yy) = mouse_up() in
  start_game ch xx yy

(* [cover_up ()] Causes the warning message (stay inside the lines, can't play in
   (1,1,1), etc ) to disappear *)
let cover_up () =
  draw_image (get_img "imgs/coverup.jpg") 0 11;
  draw_image (get_img "imgs/coverup.jpg") 0 3;
  draw_image (get_img "imgs/coverup.jpg") 500 11;
  (draw_image (get_img "imgs/coverup.jpg")236 3;)

(* [score p1 p2] Draws the current score for each player. The first int is player1's
   score and the second int is player 2's score
*)
let score p1 p2 =
  draw_image (get_img "imgs/eraser.jpg") 330 42;
  draw_image (get_img "imgs/eraser.jpg") 590 42;
  let juan = string_of_int p1 in
  moveto 352 72;
  set_color white;
  Graphics.set_font "-*-fixed-medium-r-semicondensed--55-*-*-*-*-*-iso8859-1";
  draw_string juan;
  let tiu = string_of_int p2 in
  moveto 617 72;
  (draw_string tiu;)

(* [num_try_hint num x y] displays the [num] of hints and tries left at location
   [x],[y]
   REQUIRES: int x and int y that is within range of the GUI window
*)
let num_try_hint num x y =
  let str = string_of_int num in
  draw_image (get_img "imgs/eraser.jpg") (x-24) (y-52);
  moveto x y;
  set_color white;
  Graphics.set_font "-*-fixed-medium-r-semicondensed--35-*-*-*-*-*-iso8859-1";
  (draw_string str;)

  let fst' (y,_,_) = y

  let snd' (_,y,_) = y

  let thd (_,_,y) = y

(* [which_command ()] Returns which a triple where the first element is the string
   version of command depending on whether or not the user has pressed the try button,
   and the second and third element are the x and y coordinates, respectively of
   where the user pressed
*)
let which_command () =
  let (x,y) = mouse_up () in
    if ((x >= 149 && x <= (149 + 69)) && (y >= 627 && y<= (627 + 44))) then
      ( let (xx,yy) = mouse_up () in
        ("try" , xx, yy)) else if ((x >= 801 && x <= 894)&&( y >= 624 && y <= 664)) then
      (  Graphics.auto_synchronize false; draw_wait_mgs();Graphics.remember_mode true;("hint", x, y) )
    else if ((x >= 850 && x <= (850 + 110)) && (y >= 313 && y<= (313 + 49))) then ("quit", x, y)
    else if ((x >= 50 && x <= (50 + 134)) && (y >= 313 && y<= (313 + 61))) then ("restart", x, y)
    else ("place" , x, y)

(* [play_board command x y] will return the concatenated form of "[command] cell"
   and a tuple of coordinates to determine where the picture should be placed.
   Example of commands include: "place", "try"
*)
let play_board command x y =
  if ((x >= 331 && x <=426) && (y >= 651 && y <= 685 )) then
    ((command^" 0,0,0"), (360, 666))
  else if ((x >= 438 && x <= 575) && (y >= 653 && y <= 685 )) then
    ((command^" 0,0,1"), (475, 666))
  else if ((x >= 580 && x <= 696) && (y >= 651 && y <= 684)) then
    ((command^" 0,0,2"), (605, 666))
  else if ((x >= 296 && x <= 414) && (y >= 596 && y <= 645)) then
    ((command^" 0,1,0"), (340, 610))
  else if ((x >= 422 && x <= 574) && (y >= 597 && y <= 644)) then
    ((command^" 0,1,1"), (475, 615))
  else if ((x >= 585 && x <= 697) && (y >= 597 && y <= 645)) then
    ((command^" 0,1,2"), (625, 610))
  else if ((x >= 251 && x <= 401) && (y >= 541 && y <= 590)) then
    ((command^" 0,2,0"), (320, 550))
  else if ((x >= 418 && x <= 576) && (y >= 540 && y <= 590)) then
    ((command^" 0,2,1"), (475, 555))
  else if ((x >= 583 && x <= 760) && (y >= 540 && y <= 590)) then
    ((command^" 0,2,2"), (645, 555))
  else if ((x >= 319 && x <= 428) && (y >= 480 && y <= 520)) then
    ((command^" 1,0,0"), (350, 492))
  else if ((x >= 439 && x <= 575) && (y >= 496 && y <= 524)) then
    ((command^" 1,0,1"), (475, 492))
  else if ((x >= 577 && x <= 670) && (y >= 493 && y <= 528)) then
    ((command^" 1,0,2"), (605, 492))
  else if ((x >= 300 && x <= 414) && (y >= 437 && y <= 489)) then
    ((command^" 1,1,0"), (340, 440))
  else if ((x >= 433 && x <= 575) && (y >= 440 && y <= 490)) then
    ((command^" 1,1,1"), (0,0))
  else if ((x >= 583 && x <= 726) && (y >= 439 && y <= 489)) then
    ((command^" 1,1,2"), (625, 440))
  else if ((x >= 251 && x <= 401) && (y >= 384 && y <= 434)) then
    ((command^" 1,2,0"), (320, 385))
  else if ((x >= 418 && x <= 579) && (y >= 383 && y <= 432)) then
    ((command^" 1,2,1"), (475, 390))
  else if ((x >= 583 && x <= 763) && (y >= 381 && y <= 432)) then
    ((command^" 1,2,2"), (645, 385))
  else if ((x >= 316 && x <= 427) && (y >= 324 && y <= 361)) then
    ((command^" 2,0,0"), (360, 330))
  else if ((x >= 440 && x <= 575) && (y >= 324 && y <= 360)) then
    ((command^" 2,0,1"), (475, 330))
  else if ((x >= 580 && x <= 694) && (y >= 323 && y <= 360)) then
    ((command^" 2,0,2"), (605, 330))
  else if ((x >= 284 && x <= 414) && (y >= 269 && y <= 322)) then
    ((command^" 2,1,0"), (340, 280))
  else if ((x >= 430 && x <= 576) && (y >= 270 && y <= 318)) then
    ((command^" 2,1,1"), (475, 280))
  else if ((x >= 582 && x <= 730) && (y >= 270 && y <= 318)) then
    ((command^" 2,1,2"), (625, 280))
  else if ((x >= 252 && x <= 404) && (y >= 216 && y <= 265)) then
    ((command^" 2,2,0"), (320, 225))
  else if ((x >= 420 && x <= 580) && (y >= 215 && y <= 265)) then
    ((command^" 2,2,1"), (475, 220))
  else if ((x >= 585 && x <= 763) && (y >= 216 && y <= 266)) then
    ((command^" 2,2,2"), (645, 225))
  else
    (command^" 1,1,1", (1,1))

(* [quit_restart_check ()] checks whether or not user clicked on either the
   Quit or restart button *)
let quit_restart_check () = let (x, y) = mouse_up() in
  if ((x >= 225 && x <= 335) && ( y >= 425 && y <= 474)) then
    (clear_graph(); raise Quit)
  else if ((x >= 225 && x <= 359) && ( y >= 325 && y <= 386)) then
    (clear_graph(); raise Restart)
  else ()

(* [repeat_cell x y] will display an error message depending on the [x] and [y]
   coordinates of where the user touched. These messages include: stay in the
   lines, cell already taken, and forbidden cell *)
let repeat_cell x y =
  if x = 0 && y = 0 then (draw_image (get_img "imgs/no_x.jpg") 236 0;)
  else if x = 1 && y = 1 then (draw_image (get_img "imgs/stay.jpg") 236 0;)
  else (sound 440 1000; draw_image (get_img "imgs/msg2.jpg") 236 0;)

(* [responsive_board x y] Draws the python or caml picture (determined by the
   string parameter) at the location specified by [x] and [y]
*)
let responsive_board str x y =
  (let file_name = "imgs/" ^ str ^ ".jpg" in
  draw_image (get_img file_name ) x y;)

(* [choose_letter str] is a helper method that checks whether or not we need to
   draw a python or caml depending on the current player for the Try command *)
let choose_letter str =
  if str = "python" then
    (draw_string "P";)
  else
    (draw_string "C";)

(* [cover_try playerr x y] covers the [playerr]'s try at location [x],[y] when
   the user decides to choose a different spot rather than what they had
   originally tried *)
let cover_try str ex why =
  if (why >= 500 && why <= 666) then
    (moveto (ex+15) (why+4);
     set_color top_plep;
     choose_letter str
    )
  else if (why >= 385 && why <= 492) then
    (moveto (ex+15) (why+4);
     set_color mid_plep;
     choose_letter str
    )
  else if (why >= 220 && why <= 330) then
    (moveto (ex+15) (why+4);
     set_color bot_plep;
     choose_letter str
    )
  else ()

(* [try_responsive_board playerr x y (pl,ex,why)] returns (true, xx, yy) if
   the player accepts the tried move (by either pressing on the accept button or
   the same square)It returns (false, xx yy) if [playerr] decides to not accept
   the location and presses anywhere else. (xx, yy) are the coordinates of where
   the player pressed.
   REQUIRES: string version of a player, int x and int y that is within range
            of the GUI window, a valid cell
*)
let try_responsive_board str x y (pl, ex, why)=
    moveto (x+15) (y+4);
    set_color annoying_green;
    Graphics.set_font "-*-fixed-medium-r-semicondensed--17-*-*-*-*-*-iso8859-1";
     choose_letter str;
    draw_image (get_img "imgs/accept.jpg") 65 22;
    let (xa,ya) = mouse_up () in
    let (sy, (a, b)) = play_board "try" xa ya in
    let c1 = String.index sy ',' in
    let p = int_of_char (String.get sy (c1 - 1)) - 48 in
    let xx = int_of_char (String.get sy (c1 + 1)) - 48 in
    let yy = int_of_char (String.get sy (c1 + 3)) - 48 in
    if ((p = pl && ex = xx && why = yy)||((xa >= 66 && xa <= 198)&& (ya >= 22 && ya <= 78) )) then
      ( if (p = pl && ex = xx && why = yy) then
          (responsive_board str x y;cover_up(); (true, a, b);)
        else
          (responsive_board str x y; cover_up();(true, a, b);))
    else
      ((false, xa, ya);)

(* [hightligh_curr_player playerr] Draws a rectangle around the image of the
   current player [playerr] *)
let highlight_curr_player str =
  if str = "python" then
    (rect_drawn_bblack 596 150 64 60; rect_drawn_cyan 334 145 62 65;)
  else
    (rect_drawn_bblack 334 145 62 65; rect_drawn_cyan 596 150 64 60;)

(* [winner_winner_chicken_appetizer str] helper function for the win/lose/draw
   message display that checks which player won and displays the quit and restart
   buttons for the user *)
let winner_winner_chicken_appetizer str =
  draw_image (get_img str) 200 200;
  draw_image (get_img "imgs/quit.jpg") 225 425;
  (draw_image (get_img "imgs/restart.jpg") 225 325;)

(* [winner_winner_chicken_dinner winner] Displays the win/lose/draw message based
   on what [winner] corresponds to
*)
let winner_winner_chicken_dinner str =
  if str = "win" then
    (winner_winner_chicken_appetizer "imgs/win.jpg";
     quit_restart_check())
  else if str = "draw" then
    (winner_winner_chicken_appetizer "imgs/loss_img.jpg";
     quit_restart_check())
  else if str = "caml" then
    (winner_winner_chicken_appetizer "imgs/caml_wins.jpg";
     quit_restart_check())
  else if str = "python" then (winner_winner_chicken_appetizer "imgs/python_wins.jpg";
                               quit_restart_check())
  else (winner_winner_chicken_appetizer "imgs/draw.jpg";
        quit_restart_check())

(* [cell_coords_to_x_y cell] returns the (x,y) location of where the playerr's
   icon will be placed according to cell [cell]
   REQUIRES: a valid cell
*)
let cell_coords_to_x_y (pl, x, y)=
  match (pl,x,y) with
  | (0,0,0) -> (360, 666)
  | (0,0,1) -> (475, 666)
  | (0,0,2) -> (605, 666)
  | (0,1,0) -> (340, 610)
  | (0,1,1) -> (475, 615)
  | (0,1,2) -> (625, 610)
  | (0,2,0) -> (320, 550)
  | (0,2,1) -> (475, 555)
  | (0,2,2) -> (645, 555)
  | (1,0,0) -> (350, 492)
  | (1,0,1) -> (475, 492)
  | (1,0,2) -> (605, 492)
  | (1,1,0) -> (340, 440)
  | (1,1,1) -> (0,0)
  | (1,1,2) -> (625, 440)
  | (1,2,0) -> (320, 385)
  | (1,2,1) -> (475, 390)
  | (1,2,2) -> (645, 385)
  | (2,0,0) -> (360, 330)
  | (2,0,1) -> (475, 330)
  | (2,0,2) -> (605, 330)
  | (2,1,0) -> (340, 280)
  | (2,1,1) -> (475, 275)
  | (2,1,2) -> (625, 280)
  | (2,2,0) -> (320, 225)
  | (2,2,1) -> (475, 225)
  | (2,2,2) -> (645, 225)
  | _ -> failwith "Nop"


(* [mark_three move_x move_y]draws a red box around the image with lower left
   corner [move_x], [move_y]
*)
let mark_three move_x move_y =
  ( rect_drawn_red move_x move_y;)

(* [draw_three_row most_recent_wins] draws a red box around each cell in
   most_recent_wins
   REQUIRES: most_recent_wins is a list of the cells that make a three in a row
*)
let rec draw_three_row lst =
  match lst with
  | [] -> ();
  | h::t -> (let (move_x, move_y) = cell_coords_to_x_y h in
             mark_three move_x move_y; draw_three_row t;)


(* [iterate lst f] helper method for Gui.draw_three_in_a_row
   REQUIRES: a cell list list to iterate through, a function (specifically,
   draw_three_row)
*)
let rec iterate lst f =
  match lst with
  | [] -> ();
  | h::t -> (f h; iterate t f;)

(* [draw_act_two playerr p1_score p2_score hint_num num_tries recent_wins_lst]
   draws in state specific information onto the GUI window
   REQUIRES: current player: [playerr], scores for both players : [p1_score],
             [p2_score], number of tries remaining for [playerr] : [num_tries],
             number of hints remaining for [playerr] : [hint_num],
             list of the most recent wins : [recent_wins_lst]
*)
let draw_act_two playerr p1_score p2_score hint_num num_tries recent_wins_lst =
  highlight_curr_player playerr;
  score p1_score p2_score;
  rect_drawn_gray 149 627 69 44;
  rect_drawn_gray 800 625 93 40;
  num_try_hint hint_num 836 580;
  num_try_hint num_tries 171 587;
  if not (recent_wins_lst = []) then
    (Graphics.remember_mode false; iterate recent_wins_lst draw_three_row;)

(* [bomb_boom orig x y] switches between the explode, fire, and dead images
   at one second intervals to make the animation for bomb_animation.
   [orig] is the time bomb_animation was called. [x] and [y] are the
   coordinates to draw the images
   REQUIRES: [x], [y] that falls in the range of the GUI window
*)
let rec bomb_boom orig x y =
  if (Unix.time()) = orig +. 1. then
    (responsive_board "explode" x y;
     bomb_boom orig x y;)
  else if (Unix.time()) = orig +. 2. then
    (responsive_board "fire" x y;
     bomb_boom orig x y;)
  else if (Unix.time()) = orig +. 3. then
    (responsive_board "dead" x y;
     bomb_boom orig x y;)
  else if (Unix.time()) = orig +. 4. then ()
  else (bomb_boom orig x y;)

(* [bomb_animation x y] takes in the x and y position of the icon of the clicked
   cell and will call a helper function (bomb_boom) and will animate a bomb
   booming
*)
let bomb_animation () =
  (draw_image (get_img "imgs/bomb.jpg") 250 170;
    let orig = Unix.time()  in
    bomb_boom orig 250 170;)

(* [kray_kray orig x y] switches between the krazy_occur and inverse images
   at one second intervals to make the animation for krazy_ocur_animation.
  [orig] is the time krazy_ocur_animation was called. [x] and [y] are the
  coordinates to draw the images
   REQUIRES: [x], [y] that falls in the range of the GUI window
*)
let rec kray_kray orig x y=
  if (Unix.time()) = orig +. 1. then
    (responsive_board "inverse" x y;
     kray_kray orig x y;)
  else if (Unix.time()) = orig +. 2. then
    (responsive_board "krazy_occur" x y;
     kray_kray orig x y;)
  else if (Unix.time()) = orig +. 3. then
    (responsive_board "krazy_occur" x y;
     kray_kray orig x y;)
  else if (Unix.time()) = orig +. 4. then
    (responsive_board "inverse" x y;
     kray_kray orig x y;)
  else if (Unix.time()) = orig +. 5. then ()
  else (kray_kray orig x y;)

(* [krazy_ocur_animation x y] flashes a message telling the user that something
   crazy happened at location x y
*)
let krazy_ocur_animation () =
  draw_image (get_img "imgs/krazy_occur.jpg") 250 600;
  let orig = Unix.time() in
  kray_kray orig 250 600;
