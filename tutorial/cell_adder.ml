#require "hardcaml";;
#mod_use "reference/cell.ml";;

open HardCaml.Api.B

(* XXX implement me!!! *)
let count_neighbours neighbours = raise Not_found

let update_cell ~neighbours ~current_cell =
  let count = count_neighbours neighbours in
  mux count 
    [
      (* XXX implement me!!! *)
    ]

(* testing *)
let () = 
  let module Ref = Cell.Make(HardCaml.Api.B) in
  for i=0 to 100 do
    let neighbours = Array.to_list @@ Array.init 8 (fun _ -> srand 1) in
    let current_cell = srand 1 in
    assert (update_cell ~neighbours ~current_cell = Ref.update_cell ~neighbours ~current_cell)
  done

