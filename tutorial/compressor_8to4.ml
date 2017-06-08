#require "hardcaml";;
#mod_use "reference/cell.ml";;
#mod_use "tutorial/cell_adder.ml";;

open HardCaml.Api.B

(* XXX implement me!!! *)
let ha a b = raise Not_found

(* XXX implement me!!! *)
let fa a b cin = raise Not_found

(* XXX implement me!!! *)
let compressor_8to4 bits = raise Not_found

(* XXX implement me!!! *)
let () = 
  for i=0 to 255 do
    assert false (* compare Cell_adder.count_neighbours to compressor_8to4 *)
  done

