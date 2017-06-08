open HardCamlLife

let runner = 
  match Sys.argv.(1) with
  | "sw" -> Life.runner
  | "bits" -> Grid.Bits_test.runner
  | "sim" -> Grid.Seq_test.runner
  | _ as x -> failwith ("bad argument: " ^ x)
  | exception _ -> failwith ("no argument")

let wrap = true
let grid = Life.random 20 20
let () = Life.Render.run [runner grid]

