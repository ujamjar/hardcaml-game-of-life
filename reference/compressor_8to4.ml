open HardCaml.Api.B

let ha a b = a ^: b, a &: b

let fa a b c = 
  let x = a ^: b in
  let sum = x ^: c in
  let cout = (x &: c) ^: (a &: b) in
  sum, cout;;

let compressor_8to4 i = 
  let s0, c0 = ha i.(0) i.(1) in
  let s1, c1 = fa i.(2) i.(3) i.(4) in
  let s2, c2 = fa i.(5) i.(6) i.(7) in
  let sc, cc = fa c0 c1 c2 in
  let x1, cs = fa s0 s1 s2 in
  let x2, ch = ha sc cs in
  let x3, x4 = ha cc ch in
  concat [ x4; x3; x2; x1 ]

(* check the compressor *)

let test () = 
  let sum l = reduce (+:) (List.map (fun x -> uresize x 4) l) in
  let check i = 
    let b = bits (consti 8 i) in 
    sum b = compressor_8to4 (Array.of_list b) 
  in
  for i=0 to 255 do 
    assert (check i) 
  done

