(* type of game of life grids.  Indexed as grid.(row).(col). *)
type grid = int array array

(* return the width and height of the grid *)
let size grid = 
  let y = Array.length grid in
  let x = Array.length grid.(0) in
  x, y

(* x/y offsets of neighbours *)
let neighbours = 
  [ (* x,y offsets *)
    -1,-1; 0,-1; 1,-1;
    -1, 0;       1, 0;
    -1, 1; 0, 1; 1, 1;
  ]

(* perform wrapping around to opposite edge if access is 
   out of bounds *)
let idx_wrap grid sx sy x y = 
  let rec wrap x m = 
    if x < 0 then wrap (x+m) m
    else if x >= m then wrap (x-m) m
    else x
  in
  let x, y = wrap x sx, wrap y sy in
  Some(x,y)

(* out of bound accesses default to dead *)
let idx_kill grid sx sy x y = 
  if x < 0 || y < 0 || x >= sx || y >= sy then None
  else Some(x,y)

(* update the cell at (x,y) *)
let update_cell grid x y = 
  let sx, sy = size grid in

  let neighbour =
    let wrap = idx_kill grid sx sy in
    (fun x y -> match wrap x y with None -> 0 | Some(x,y) -> grid.(y).(x))
  in

  (* count number of alive neighbours *)
  let count = List.fold_left (fun acc (x',y') -> acc + neighbour (x+x') (y+y')) 
    0 neighbours
  in

  (* live or die *)
  if count = 2 then grid.(y).(x)
  else if count = 3 then 1
  else 0

(* update all cells in the grid *)
let update_grid grid = 
  Array.mapi (fun y -> Array.mapi (fun x _ -> update_cell grid x y)) grid

(* initialise a grid using the given function *)
let init x y f = 
  Array.init y (fun y -> Array.init x (fun x -> f x y))

(* create a grid with random values *)
let random x y = init x y (fun _ _ -> Random.int 2)

(* print a grid *)
let print g = 
  let is_alive x = x = 1 in
  Array.iter (fun y ->
    Array.iter (fun x -> Printf.printf "%c" (if is_alive x then '*' else ' ')) y;
    Printf.printf "\n") g

(* type used to communicate with the visualiser *)
type grid_runner = 
  {
    sx : int;
    sy : int;
    is_alive : int -> int -> bool;
    step : unit -> unit;
  }

(* function which wraps grid updates suitable for the visualiser *)
let runner grid = 
  let sx, sy = size grid in
  let grid = ref grid in
  let is_alive x y = !grid.(y).(x) = 1 in
  let step () = grid := update_grid !grid in
  { sx; sy; is_alive; step }

(* visualisation using lambda-term *)
module Render = struct

  open Lwt
  open LTerm_widget
  open LTerm_geom
  open CamomileLibrary

  let space = UChar.of_char ' '
  let star = UChar.of_char '*'

  (* custom widget for displaying grids *)
  class gameoflife grid = object(self) 

    inherit t "gameoflife" as super

    (*method can_focus = false
    method set_allocation r = super#set_allocation r*)
    val style = 
      LTerm_style.({ none with foreground=Some white; 
                               background=Some black }) 

    method draw ctx focused = 
      let { rows; cols } = LTerm_draw.size ctx in

      let sx, sy = grid.sx, grid.sy in
      let ox = if sx < cols then (cols-sx)/2 else 0 in
      let oy = if sy < rows then (rows-sy)/2 else 0 in

      for y=0 to (min rows sy)-1 do
        for x=0 to (min cols sx)-1 do
          let chr =
            try if grid.is_alive x y then star else space 
            with _ -> space 
          in
          LTerm_draw.draw_char ~style ctx (y+oy) (x+ox) chr
        done
      done

  end

  (* display a sequence of grids side-by-side, and step in unison *)
  let run grids = 

    let top = new frame in
    let generation, incr_gen = 
      let gen = ref 0 in
      let g = new label "0" in
      g, (fun () -> 
            incr gen;
            g#set_text (Printf.sprintf "%i\n" !gen))
    in

    let gols = List.map (fun grid -> new gameoflife grid) grids in
    let hbox = new hbox in
    List.iter (fun gol -> hbox#add ~expand:true gol) gols;
    let stepper _ = 
      incr_gen ();
      List.iter (fun grid -> grid.step ()) grids;
      top#queue_draw
    in

    let waiter, wakener = wait () in

    let exit = new button "exit" in
    exit#on_click (wakeup wakener);
    let step = new button "step" in
    step#on_click stepper;

    let vbox = new vbox in
    vbox#add ~expand:true hbox;
    vbox#add ~expand:false generation;
    vbox#add ~expand:false step;
    vbox#add ~expand:false exit;

    top#set vbox;

    top#on_event (function (* quit with escape key *)
      | LTerm_event.Key{LTerm_key.code=LTerm_key.Escape} -> 
        wakeup wakener (); false 
      | _ -> false);

    let term = 
      Lazy.force LTerm.stdout >>= fun term ->
      LTerm.enable_mouse term >>= fun () ->
      Lwt.finalize 
        (fun () -> run term top waiter)
        (fun () -> LTerm.disable_mouse term)
    in

    Lwt_main.run term

end

