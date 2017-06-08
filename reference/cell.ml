(* Cell implemented as a 512 entry ROM *)
let rec count_bits x = 
  if x = 0 then 0 
  else (x land 1) + count_bits (x lsr 1)

let next_cell_state idx = 
  let cnt = count_bits (idx land 255) in
  let alive = idx >= 256 in
  if alive then cnt=3
  else cnt=2 || cnt=3

let table = Array.init 512 next_cell_state

module Make(B : HardCaml.Comb.S) = struct

  open B

  (* neighbours is a list of 8 1 bit signals.  Return the 4 bit sum
     which ranges from 0..8 (inclusive) *)
  let count_neighbours neighbours = 
    reduce (+:) (List.map (fun x -> uresize x 4) neighbours) 

  (* compute liveness given current state, and neighbours *)
  let update_cell ~neighbours ~current_cell = 
    let count = count_neighbours neighbours in
    mux count 
      [
        gnd; 
        gnd; (* <= 1 *)
        current_cell; (* 2 *)
        vdd; (* 3 *)
        gnd (* >= 4 *)
      ]

end

