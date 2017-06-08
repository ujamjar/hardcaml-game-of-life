open Life

module Comb(B : HardCaml.Comb.S) = struct

  module C = Cell.Make(B)
  open B

  (* Given a 2d grid of cell states, update every cell in parallel *)
  let update_grid grid = 
    let open Printf in
    let sx, sy = size grid in

    let neighbour =
      let wrap = idx_kill grid sx sy in
      (fun (x, y) -> match wrap x y with None -> gnd | Some(x,y) -> grid.(y).(x))
    in

    let neighbours x y =
         List.map (fun (x',y') -> x+x', y+y') neighbours 
      |> List.map neighbour
    in
    
    let cell x y = C.update_cell ~neighbours:(neighbours x y) ~current_cell:grid.(y).(x) in

    let cells = Array.init sy (fun y -> Array.init sx (fun x -> cell x y)) in

    cells

  let is_alive x = x = vdd

end

module Bits_test = struct

  module C = Comb(HardCaml.Bits.Comb.IntbitsList)
  open HardCaml.Bits.Comb.IntbitsList

  (* combinational test harness for update_grid *)
  let runner grid = 
    let sx, sy = size grid in
    let grid = init sx sy (fun x y -> if grid.(y).(x) = 1 then vdd else gnd) in
    let grid = ref grid in
    let is_alive x y = !grid.(y).(x) = vdd in
    let step () = grid := C.update_grid !grid in
    { sx; sy; is_alive; step }

end

module Seq = struct

  module G = Comb(HardCaml.Signal.Comb)

  module Seq = HardCaml.Signal.Make_seq(struct
    let reg_spec = HardCaml.Signal.Seq.r_sync
    let ram_spec = HardCaml.Signal.Seq.r_none
  end)

  open HardCaml.Signal.Comb

  let cell_name x y = Printf.sprintf "cell_%ix%i" x y

  (* Attach grid to state registers.  Passed an initial grid state 
     used to reset the registers *)
  let grid initial = 
    let x, y = size initial in
    let w = 
      let w x y = wire 1 -- (cell_name x y) in
      Array.init y (fun y -> Array.init x (fun x -> w x y))
    in
    let n = G.update_grid w in
    let r = Array.map2 (Array.map2 (fun c d -> Seq.reg ~cv:c ~e:vdd d)) initial n in
    Array.iter2 (Array.iter2 (<==)) w r;
    w

end

module Seq_test = struct

  module S = HardCaml.Cyclesim.Api
               
  (*module B = HardCaml.Bits.Comb.ArraybitsInt32
  module Cs = HardCaml.Cyclesim.Make(B)*)

  module B = HardCaml.Bits.Raw.Comb.ArraybitsInt32
  module B' = HardCaml.Bits.Raw.ArraybitsInt32
  module Cs = HardCaml.Cyclesim.MakeRaw(B')

  let circuit initial = 
    let open HardCaml.Signal.Comb in
    let sx, sy = size initial in
    let initial = init sx sy (fun x y -> if initial.(y).(x) = 1 then vdd else gnd) in
    let grid = Seq.grid initial in
    HardCaml.Circuit.make "gameoflife"
      (List.concat @@ Array.to_list @@ (Array.map Array.to_list grid))

  let testbench initial = 
    let sx, sy = size initial in
    (* build simulator *)
    let circ = circuit initial in
    let sim = Cs.make circ in
    (* get simulation ports *)
    let state = init sx sy (fun x y -> S.out_port_next sim (Seq.cell_name x y)) in
    let clr = S.in_port sim "clear" in
 
    (* reset *)
    clr := B.vdd;
    S.cycle sim;
    clr := B.gnd;

    (* step *)
    let step () = 
      S.cycle sim;
      init sx sy (fun x y -> B.to_int !(state.(y).(x)))
    in
    step

  let runner grid = 
    let sx, sy = size grid in
    let grid = ref grid in
    let is_alive x y = !grid.(y).(x) = 1 in
    let tb_step = testbench !grid in
    let step () = grid := tb_step () in
    { sx; sy; is_alive; step }

end

