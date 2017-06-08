open HardCaml
open Signal.Comb

module type Cfg = sig
  val x_bits : int
  val y_bits : int
end

module Make(Cfg : Cfg) = struct

  open Cfg

  module Seq = Signal.Make_seq(struct
    let reg_spec = Signal.Seq.r_sync
    let ram_spec = Signal.Seq.r_none
  end)

  module Ram_I = struct
    type 'a t = {
      d : 'a[@bits 1];
      we : 'a[@bits 1];
      addr : 'a[@bits (x_bits+y_bits)];
    }[@@deriving hardcaml]
  end

  let ram_mux2 m0 m1 = 
    let open Ram_I in
    {
      d = mux2 m0.we m0.d m1.d;
      we = mux2 m0.we m0.we m1.we;
      addr = mux2 m0.we m0.addr m1.addr;
    }

  module Addr_sm = struct

    module I = struct
      type 'a t = {
        start : 'a[@bits 1];
        cols : 'a[@bits x_bits];
        rows : 'a[@bits y_bits];
      }[@@deriving hardcaml]
    end

    module O = struct
      type 'a t = {
        addr : 'a[@bits x_bits+y_bits];
        addr_cell : 'a[@bits x_bits+y_bits];
        ready : 'a[@bits 1];
        oob : 'a[@bits 1];
        first : 'a[@bits 1];
        center : 'a[@bits 1];
        last : 'a[@bits 1];
      }[@@deriving hardcaml]
    end

    open I
    open O

    let f' i =
      let open Signal.Guarded in
      let is, switch, next = Signal.Statemachine.statemachine Signal.Seq.r_sync vdd
        [ `idle; `loop ]
      in

      let cx = Seq.g_reg ~e:vdd x_bits in
      let cy = Seq.g_reg ~e:vdd y_bits in
      let dx = Seq.g_reg ~e:vdd 2 in
      let dy = Seq.g_reg ~e:vdd 2 in

      let _ = cx#q -- "cx", cy#q -- "cy",
              dx#q -- "dx", dy#q -- "dy" in

      let loop (vx,sx,ex) next = 
        g_proc [
          vx $== (vx#q +:. 1);
          g_when (vx#q ==: ex) [
            vx $==. sx;
            g_proc next;
          ]
        ]
      in
      let loop2d (vx,sx,ex) (vy,sy,ey) next = 
        loop (vx,sx,ex) [ 
          loop (vy,sy,ey) next 
        ]
      in

      let () = 
        compile [
          switch [
            `idle, [
              cx $==. 0;
              cy $==. 0;
              dx $==. -1;
              dy $==. -1;
              g_when i.start [
                next `loop;
              ];
            ];
            `loop, [
              loop2d (dx,-1,consti 2 1) (dy,-1,consti 2 1) [
                loop2d (cx,0,i.cols) (cy,0,i.rows) [ 
                  next `idle 
                ]
              ];
            ];
          ]
        ]
      in

      let oob, addr = 
        let x = (ue cx#q +: (sresize dx#q (x_bits+1))) -- "x" in
        let y = (ue cy#q +: (sresize dy#q (y_bits+1))) -- "y" in
        let oob = msb x |: msb y in
        oob, (lsbs y @: lsbs x)
      in

      let first, center, last = 
        (dx#q ==:. (-1)) &: (dy#q ==:. (-1)),
        (dx#q ==:.   0 ) &: (dy#q ==:.   0 ),
        (dx#q ==:.   1 ) &: (dy#q ==:.   1 )
      in

      { 
        O.ready = is `idle;
        addr;
        addr_cell = cy#q @: cx#q;
        oob;
        first; center; last;
      }

    let f i =
      let name s (n,_) = s -- ("addr_sm_" ^ n) in
      let o = f' i in
      O.(map2 name o t)

  end

  module Mem_banks = struct
    
    module I = struct
      type 'a t = {
        f : 'a Ram_I.t[@rtlprefix "f_"];
        t : 'a Ram_I.t[@rtlprefix "t_"];
        phase : 'a[@bits 1];
      }[@@deriving hardcaml]
    end
    module O = struct
      type 'a t = {
        qf : 'a[@bits 1];
        qt : 'a[@bits 1];
      }[@@deriving hardcaml]
    end

    let f' i = 
      let open I in
      let open Ram_I in
      let n = 1 lsl (x_bits + y_bits) in
      let swap s a b = mux2 s a b, mux2 s b a in
      let we0,we1 = swap i.phase i.f.we i.t.we in
      let addr0,addr1 = swap i.phase i.f.addr i.t.addr in
      let d0,d1 = swap i.phase i.f.d i.t.d in
      let q0,q1 = 
        Seq.ram_rbw n ~we:we0 ~wa:addr0 ~d:d0 ~re:vdd ~ra:addr0,
        Seq.ram_rbw n ~we:we1 ~wa:addr1 ~d:d1 ~re:vdd ~ra:addr1
      in
      let qf, qt = swap i.phase q0 q1 in
      { O.qf; qt }

    let f i =
      let name s (n,_) = s -- ("mem_banks_" ^ n) in
      let i = I.(map2 name i t) in
      let o = f' i in
      O.(map2 name o t)

  end

  module Cell_ff = struct

    module I = struct
      type 'a t = {
        alive_in : 'a[@bits 1];
        first : 'a[@bits 1];
        center : 'a[@bits 1];
      }[@@deriving hardcaml]
    end

    module O = struct
      type 'a t = {
        alive_out : 'a[@bits 1];
      }[@@deriving hardcaml]
    end
    
    let reg_ff ~e w f = 
      let r' = wire w in
      let r = Seq.reg ~e r' in
      r' <== (f r);
      r'

    let f' i = 
      let open I in

      let cnt = reg_ff ~e:vdd 4 
        (fun cnt ->
          (mux2 i.first (zero 4) cnt) +: 
          (mux2 (i.alive_in &: ~: (i.center)) (one 4) (zero 4)))
      in
      let alive = Seq.reg ~e:i.center i.alive_in in

      { O.alive_out = mux (cnt -- "neighbours") [ gnd; gnd; alive; vdd; gnd ] }

    let f i =
      let name s (n,_) = s -- ("cell_" ^ n) in
      let i = I.(map2 name i t) in
      let o = f' i in
      O.(map2 name o t)

  end

  module Cell = struct

    module I = struct
      type 'a t = {
        alive_in : 'a[@bits 1];
        first : 'a[@bits 1];
        center : 'a[@bits 1];
      }[@@deriving hardcaml]
    end

    module O = struct
      type 'a t = {
        alive_out : 'a[@bits 1];
      }[@@deriving hardcaml]
    end

    module C = Cell.Make(Signal.Comb)

    let f' i = 
      let open I in
      let current_cell = Seq.reg ~e:i.center i.alive_in in
      let rec neighbours n d = 
        if n=0 then []
        else d :: neighbours (n-1) (Seq.reg ~e:(~: (i.center)) d) 
      in
      { O.alive_out = C.update_cell ~neighbours:(neighbours 8 i.alive_in)
                                    ~current_cell }

    let f i =
      let name s (n,_) = s -- ("cell_" ^ n) in
      let i = I.(map2 name i t) in
      let o = f' i in
      O.(map2 name o t)

  end

  module Top = struct

    open Cfg

    module I = struct
      type 'a t = {
        start : 'a[@bits 1];
        cols : 'a[@bits x_bits];
        rows : 'a[@bits y_bits];
        mem : 'a Ram_I.t;
      }[@@deriving hardcaml]
    end

    module O = struct
      type 'a t = {
        ready : 'a[@bits 1];
        valid : 'a[@bits 1];
        q : 'a[@bits 1]; 
      }[@@deriving hardcaml]
    end

    let rec delay ?(n=1) d = 
      if n=0 then d
      else Seq.reg ~e:vdd (delay ~n:(n-1) d) 

    let f i = 
      let open I in
 
      let sm = Addr_sm.f 
          { 
            Addr_sm.I.start = i.start; 
            cols = i.cols; rows = i.rows 
          } 
      in

      let phase = Seq.reg_fb ~e:i.start ~w:1 (fun d -> ~: d) -- "phase" in
      let memwr = 
        {
          Ram_I.d = wire 1;
          we = delay sm.Addr_sm.O.last;
          addr = delay sm.Addr_sm.O.addr_cell;
        }
      in
      let banks = Mem_banks.f 
          { 
            Mem_banks.I.phase;
            f = { Ram_I.we = gnd; addr = sm.Addr_sm.O.addr; d = gnd };
            t = ram_mux2 i.mem memwr;
          }
      in

      let cell = Cell.f 
          {
            Cell.I.alive_in = mux2 (delay sm.Addr_sm.O.oob) gnd banks.Mem_banks.O.qf;
            first = delay sm.Addr_sm.O.first;
            center = delay sm.Addr_sm.O.center;
          }
      in

      let () = memwr.Ram_I.d <== cell.Cell.O.alive_out in

      let ready = 
        let r = sm.Addr_sm.O.ready in
        r &: delay r
      in
      {
        O.ready; 
        valid = memwr.Ram_I.we;
        q = cell.Cell.O.alive_out;
      }

  end

end

module B = Bits.Comb.ArraybitsInt32
module W = HardCamlWaveTerm.Wave.Make(HardCamlWaveTerm.Wave.Bits(B))
module Ws = HardCamlWaveTerm.Sim.Make(B)(W)
module Widget = HardCamlWaveTerm.Widget.Make(B)(W)

let testbench_sm () = 

  (* construct simulator *)
  let x_bits, y_bits = 8, 8 in
  let module M = Make(struct
    let x_bits = x_bits
    let y_bits = y_bits
  end) in
  let module G = Interface.Gen(B)(M.Addr_sm.I)(M.Addr_sm.O) in
  let module S = Cyclesim.Api in
  let _,sim,i,o,n = G.make "life" M.Addr_sm.f in

  let cfg =
    [
      "clock", W.B;
      "clear", W.B;
    ] @
    M.Addr_sm.I.(to_list @@ map (fun (n,b) -> n, if b=1 then W.B else W.H) t) @
    M.Addr_sm.O.(to_list @@ map (fun (n,b) -> n, if b=1 then W.B else W.H) t) @
    [
      "cx", W.S;
      "cy", W.S;
      "dx", W.S;
      "dy", W.S;
      "x", W.S;
      "y", W.S;
    ]
  in
  let sim, waves = Ws.wrap ~cfg sim in

  (* simulation *)
  let open M.Addr_sm.I in
  let open M.Addr_sm.O in

  S.reset sim;
  i.start := B.vdd;
  i.cols := B.consti x_bits 1;
  i.rows := B.consti y_bits 1;
  S.cycle sim;
  i.start := B.gnd;
  while B.to_int !(n.ready) = 0 do
    S.cycle sim;
  done;
  S.cycle sim;

  let waveform = new Widget.waveform () in
  let waves = W.{ cfg={default with wave_width=2}; waves } in
  waveform#set_waves waves;
  Lwt_main.run @@ Widget.run_widget waveform

(*let () = testbench_sm ()*)

let testbench_top ~waves grid = 

  (* construct simulator *)
  let width, height = Life.size grid in
  let x_bits, y_bits = Utils.clog2 width, Utils.clog2 height in
  let module M = Make(struct
    let x_bits = x_bits
    let y_bits = y_bits
  end) in
  let module G = Interface.Gen(B)(M.Top.I)(M.Top.O) in
  let module S = Cyclesim.Api in
  let _,sim,i,o,n = G.make "life" M.Top.f in

  let cfg =
    [
      "clock", W.B;
      "clear", W.B;
    ] @
    M.Top.I.(to_list @@ map (fun (n,b) -> n, if b=1 then W.B else W.H) t) @
    M.Top.O.(to_list @@ map (fun (n,b) -> n, if b=1 then W.B else W.H) t) @
    [
      "cx", W.S;
      "cy", W.S;
      "dx", W.S;
      "dy", W.S;
      "x", W.S;
      "y", W.S;
    ]
  in
  let sim, waves = 
    if waves then 
      let w,s = Ws.wrap ~cfg sim in 
      w, Some s
    else sim, None
  in

  (* simulation *)
  let open M.Top.I in
  let open M.Top.O in
  let open M.Ram_I in

  let cells = ref [] in
  let return_grid () = 
    let rec split n h l = 
      if n=0 then List.rev h, l
      else split (n-1) (List.hd l :: h) (List.tl l)
    in
    let rec row = function
      | [] -> []
      | c -> 
        let r,c = split width [] c in
        r :: row c
    in
    let c = List.rev !cells in
    cells := [];
    Array.of_list @@ List.map Array.of_list @@ row c
  in

  S.reset sim;

  i.mem.we := B.vdd;
  for row=0 to (1 lsl y_bits) - 1 do
    for col=0 to (1 lsl x_bits) - 1 do
      i.mem.addr := B.(consti y_bits row @: consti x_bits col);
      i.mem.d := B.consti 1 (try grid.(row).(col) with _ -> 0);
      S.cycle sim;
    done;
  done;
  i.mem.we := B.gnd;
  i.cols := B.consti x_bits (width-1);
  i.rows := B.consti y_bits (height-1);

  let step () = 
    i.start := B.vdd;
    S.cycle sim;
    i.start := B.gnd;
    while B.to_int !(n.ready) = 0 do
      if B.to_int !(n.valid) = 1 then begin
        cells := B.to_int !(n.q) :: !cells
      end;
      S.cycle sim;
    done;
    S.cycle sim;

    begin match waves with
      | None -> ()
      | Some(waves) ->
        let waveform = new Widget.waveform () in
        let waves = W.{ cfg={default with wave_width=2}; waves } in
        waveform#set_waves waves;
        Lwt_main.run @@ Widget.run_widget waveform;
    end;
    return_grid ()
  in
  step

let test () =
  let open Life in
  let () = Random.self_init () in
  let grid = random 8 8 in
  let tb_grid = testbench_top ~waves:true grid () in
  let sw_grid = update_grid grid in
  let print x = 
    print x;
    Printf.printf "=====================\n"
  in
  print grid;
  print tb_grid;
  print sw_grid;
  Printf.printf "matches = %b\n" (tb_grid = sw_grid)

let runner grid = 
  let open Life in
  let sx, sy = size grid in
  let grid = ref grid in
  let is_alive x y = !grid.(y).(x) = 1 in
  let tb_step = testbench_top ~waves:false !grid in
  let step () = grid := tb_step () in
  { sx; sy; is_alive; step }


