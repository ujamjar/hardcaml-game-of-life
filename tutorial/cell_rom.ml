#require "hardcaml";;
#mod_use "reference/cell.ml";;

(* XXX implement me!!! *)
let count_bits x = raise Not_found

(* XXX implement me!!! *)
let next_cell_state idx = raise Not_found

open HardCaml.Api.B

(* compute the ROM values *)
let table = Array.init 512 next_cell_state

let () = assert (table = Cell.table)

let update_cell ~neighbours ~current_cell =
  (* convert array of booleans, to a list of signals *)
  let data = List.map (function true -> vdd | false -> gnd) @@ Array.to_list table in
  (* construct index with current cell state as most significant bit, 
     and neighbours as lower 8 bits *)
  let idx = concat (current_cell :: neighbours) in
  mux idx data
