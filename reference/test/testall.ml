open HardCamlLife

let runners = [
  Life.runner;
  Grid.Bits_test.runner;
  Grid.Seq_test.runner;
]

let wrap = false
let x, y = 50, 40
let grid = Life.random x y
let () = Life.Render.run (List.map (fun r -> r grid) runners)


