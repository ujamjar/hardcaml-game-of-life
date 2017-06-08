open HardCaml
open Signal.Comb

module type Cfg = sig
  (* max width of grid *)
  val x_bits : int
  (* max height of grid *)
  val y_bits : int
  (* system memory address bits (addressable bytes) *)
  val address_bits : int
  (* system memory bus width, in bits *)
  val bus_width : int
end

module Make(Cfg : Cfg) = struct

  open Cfg

  let log_bus_width = 
    match bus_width with
    | 8 -> 3
    | 16 -> 4
    | 32 -> 5
    | 64 -> 6
    | 128 -> 7
    | 256 -> 8
    | _ -> failwith "unsupported bus width"

  module Seq = Signal.Make_seq(struct
    let reg_spec = Signal.Seq.r_sync
    let ram_spec = Signal.Seq.r_none
  end)

  (* write bus_width bits, read bus_width bits *)
  module Ram_I = struct
    type 'a t = {
      d : 'a[@bits bus_width];
      we : 'a[@bits 1];
      wa : 'a[@bits x_bits-log_bus_width];
      ra : 'a[@bits x_bits-log_bus_width];
    }[@@deriving hardcaml]
  end

  let ram_mux2 sel m0 m1 = 
    let open Ram_I in
    {
      d = mux2 sel m0.d m1.d;
      we = mux2 sel m0.we m1.we;
      ra = mux2 sel m0.ra m1.ra;
      wa = mux2 sel m0.wa m1.wa;
    }

  let split_addr x = 
    (try select x (width x - 1) log_bus_width with _ -> empty),
    select x (log_bus_width - 1) 0

  let ram i = 
    let open Ram_I in
    Seq.ram_rbw 
      (1 lsl (x_bits - log_bus_width)) 
      ~re:vdd ~we:i.we ~d:i.d ~ra:i.ra ~wa:i.wa

  let write_1xn we wa d = 
    let wahi, wa = split_addr wa in
    let d = d @: Seq.reg_fb ~e:we ~w:(bus_width-1) (fun d' -> d @: msbs d') in
    let we = we &: (wa ==:. (-1)) in
    we, wahi, d

  let read_nx1 ra = 
    let ra, ralo = split_addr ra in
    ra, (fun q -> mux (Seq.reg ~e:vdd ralo) (List.rev (bits q)))

  module Line_buffers = struct

    module I = struct
      type 'a t = {
        cycle : 'a[@bits 1];
        mem : 'a Ram_I.t[@rtlprefix "mem_"];
        core_addr : 'a[@bits x_bits];
      }[@@deriving hardcaml]
    end
    module O = struct
      type 'a t = {
        q0 : 'a[@bits 1];
        q1 : 'a[@bits 1];
        q2 : 'a[@bits 1];
      }[@@deriving hardcaml]
    end

    let f i = 
      let open I in
      let open Ram_I in

      let phase = Seq.reg_fb ~e:i.cycle ~w:2 (fun d -> d +:. 1) in
      let mux4 d = 
        ram_mux2 (bit phase 1)
          (ram_mux2 (bit phase 0) d.(3) d.(2))
          (ram_mux2 (bit phase 0) d.(1) d.(0))
      in

      let ra, muxnx1 = read_nx1 i.core_addr in
      let crd = {
        we = gnd; 
        wa = zero (x_bits - log_bus_width); 
        d = zero bus_width;  
        ra = ra;
      } in
      let mwr = i.mem in

      let q0 = muxnx1 @@ ram @@ mux4 [| crd; mwr; crd; crd |] in 
      let q1 = muxnx1 @@ ram @@ mux4 [| crd; crd; mwr; crd |] in
      let q2 = muxnx1 @@ ram @@ mux4 [| crd; crd; crd; mwr |] in
      let q3 = muxnx1 @@ ram @@ mux4 [| mwr; crd; crd; crd |] in

      let q0 = mux phase [ q0; q1; q2; q3 ] in
      let q1 = mux phase [ q1; q2; q3; q0 ] in
      let q2 = mux phase [ q2; q3; q0; q1 ] in

      {
        O.q0;
        q1;
        q2;
      }
      
  end

  module Fifo = struct

    module I = struct
      type 'a t = {
        clr : 'a[@bits 1];
        d : 'a[@bits bus_width];
        we : 'a[@bits 1];
        re : 'a[@bits 1];
      }[@@deriving hardcaml]
    end
    module O = struct
      type 'a t = {
        q : 'a[@bits bus_width];
        full : 'a[@bits 1];
        empty : 'a[@bits 1];
      }[@@deriving hardcaml]
    end

    let log_size = 4
    let size = 1 lsl log_size
    let empty_space = 4

    let f i = 
      let open I in
      let ra = Seq.reg_fb ~c:i.clr ~e:i.re ~w:log_size (fun d -> d +:. 1) -- "fifo_ra" in
      let wa = Seq.reg_fb ~c:i.clr ~e:i.we ~w:log_size (fun d -> d +:. 1) -- "fifo_wa" in
      let level = 
        Seq.reg_fb ~c:i.clr ~e:(i.re |: i.we) ~w:(log_size+1) 
          (fun d ->
             let d = mux2 i.re (d -:. 1) d in (* decrement level on read *)
             let d = mux2 i.we (d +:. 1) d in (* increment level of write *)
             d) -- "fifo_level"
      in
      let q = Seq.ram_rbw ~we:i.we ~re:i.re ~wa ~ra ~d:i.d size in
      let full = (level +:. empty_space) >=:. size in
      let empty = ra ==: wa in
      { O.q; full; empty }

  end

  module Line_sm = struct

    module I = struct
      type 'a t = {
        start : 'a[@bits 1];
        width : 'a[@bits x_bits];
        stall : 'a[@bits 1];
      }[@@deriving hardcaml]
    end

    module O = struct
      type 'a t = {
        count : 'a[@bits x_bits-log_bus_width];
        valid : 'a[@bits 1];
        ready : 'a[@bits 1];
      }[@@deriving hardcaml]
    end

    let f i =
      let open I in
      let is, switch, next = Signal.Statemachine.statemachine Signal.Seq.r_sync vdd 
          [ `idle; `loop ]
      in
      let count = Seq.g_reg ~e:vdd (x_bits) in
      let valid = Seq.g_reg ~e:vdd 1 in
      let ready = is `idle in
      let () = 
        let open Signal.Guarded in
        compile [
          switch [
            `idle, [
              count $==. 0;
              g_when i.start [
                valid $==. 1;
                next `loop;
              ]
            ];
            `loop, [
              g_if i.stall [
                valid $==. 0;
              ] [
                valid $==. 1;
                count $== (count#q +:. 1);
                g_when (count#q ==: i.width) [
                  valid $==. 0;
                  next `idle;
                ]
              ]
            ]
          ]
        ]
      in
      {
        O.count = count#q;
        valid = valid#q;
        ready = ready;
      }

  end

  module Line = struct

    module I = struct
      type 'a t = {
        (* load next line *)
        mem : 'a Ram_I.t;
        (* start line update *)
        start : 'a[@bits 1];
        (* cycle through line buffers *)
        cycle : 'a[@bits 1];
        (* FIFO read enable *)
        re : 'a[@bits 1]; 
        (* grid width *)
        width : 'a[@bits x_bits];
      }[@@deriving hardcaml]
    end

    module O = struct
      type 'a t = {
        (* line done *)
        ready : 'a[@bits 1];
        (* FIFO data out *)
        q : 'a[@bits Cfg.bus_width];
      }[@@deriving hardcaml]
    end

    module C = Cell.Make(Signal.Comb)

    let cell ~e ~q0 ~q1 ~q2 = 
      let q3, q4, q5 = Seq.reg ~e q0, Seq.reg ~e q1, Seq.reg ~e q2 in
      let q6, q7, q8 = Seq.reg ~e q3, Seq.reg ~e q4, Seq.reg ~e q5 in
      C.update_cell 
        ~neighbours:[q0; q1; q2; q3; q5; q6; q7; q8] 
        ~current_cell:q4

    let cell_fifo ~clr ~cell ~we ~re = 
      let we_cell, _, cell =
        let wa = Seq.reg_fb ~c:clr ~e:we ~w:log_bus_width (fun d -> d +:. 1) in
        write_1xn we wa cell
      in
      Fifo.f 
        {
          Fifo.I.clr = clr;
          d = cell;
          we = we_cell;
          re = re;
        }


    let f i = 
      let open I in
      let rec delay ?(n=1) d = 
        if n=0 then d else Seq.reg ~c:i.start ~e:vdd (delay ~n:(n-1) d) 
      in

      (* line state machine *)
      let stall = wire 1 in
      let sm = Line_sm.f 
        {
          Line_sm.I.start = i.start;
          width = i.width;
          stall = stall;
        }
      in

      (* line buffers *)
      let bufs = Line_buffers.f 
        { 
          Line_buffers.I.cycle = i.cycle;
          mem = i.mem;
          core_addr = sm.Line_sm.O.count;
        }
      in

      (* cell logic *)
      let e = delay sm.Line_sm.O.valid in
      let cell = delay Line_buffers.O.(cell ~e ~q0:bufs.q0 ~q1:bufs.q1 ~q2:bufs.q2) in

      (* FIFO *)
      let e = delay e in
      let fifo = cell_fifo ~clr:i.start ~cell ~we:e ~re:i.re in
      let () = stall <== fifo.Fifo.O.full in

      {
        O.ready = sm.Line_sm.O.ready;
        q = fifo.Fifo.O.q;
      }

  end

end

module B = Bits.Comb.ArraybitsInt32
module W = HardCamlWaveTerm.Wave.Make(HardCamlWaveTerm.Wave.Bits(B))
module Ws = HardCamlWaveTerm.Sim.Make(B)(W)
module Widget = HardCamlWaveTerm.Widget.Make(B)(W)

module Cfg = struct
  let x_bits = 8
  let y_bits = 8
  let address_bits = 32
  let bus_width = 32
end

let test_line_buffer () = 
  let module M = Make(Cfg) in
  let module Ml = M.Line_buffers in
  let module G = Interface.Gen(B)(Ml.I)(Ml.O) in
  let module S = Cyclesim.Api in
  let circ,sim,i,o,n = G.make "life" Ml.f in
  let sim, waves = Ws.wrap sim in

  let open Ml.I in
  let open M.Ram_I in
  let abits = Cfg.x_bits - M.log_bus_width in

  S.reset sim;

  let data = 
    Array.init 5 
      (fun _ -> Array.init (1 lsl abits) 
                  (fun _ -> B.srand Cfg.bus_width))
  in

  let read_and_write_lines pass = 
    i.cycle := B.vdd;
    i.core_addr := B.consti Cfg.x_bits 0;
    S.cycle sim;
    i.cycle := B.gnd;

    let f0 (x,_,_) = x in
    let f1 (_,x,_) = x in
    let f2 (_,_,x) = x in
    let q = 
      Array.init (1 lsl abits) 
        (fun k ->
          i.mem.we := B.vdd;
          i.mem.d := data.(pass).(k);
          i.mem.wa := B.consti abits k;
          let x = Array.init Cfg.bus_width (fun j ->
            i.core_addr := B.(!(i.core_addr) +:. 1);
            S.cycle sim;
            i.mem.we := B.gnd;
            !(o.Ml.O.q0),
            !(o.Ml.O.q1),
            !(o.Ml.O.q2))
          in
          let q f = B.concat @@ List.rev @@ Array.to_list @@ Array.map f x in
          q f0, q f1, q f2)
    in
    let q = 
      [|
        Array.map f0 q;
        Array.map f1 q;
        Array.map f2 q;
      |]
    in
    q
  in

  let q = Array.init (Array.length data) (read_and_write_lines) in

  for i=0 to Array.length data - 4 do
    assert ([| data.(i+0); data.(i+1); data.(i+2) |] = q.(i+3))
  done;

  let waveform = new Widget.waveform () in
  let waves = W.{ cfg={default with wave_width=2}; waves } in
  waveform#set_waves waves;
  Lwt_main.run @@ Widget.run_widget waveform;
  data, q

let test_fifo () = 
  let module M' = Make(Cfg) in
  let module M = M'.Fifo in
  let module G = Interface.Gen(B)(M.I)(M.O) in
  let module S = Cyclesim.Api in
  let _,sim,i,o,n = G.make "life" M.f in
  let sim, waves = Ws.wrap sim in

  let open M.I in

  S.reset sim;
  S.cycle sim;
  i.we := B.vdd;
  for j=0 to 12 do
    i.d := B.consti Cfg.bus_width (j+1);
    S.cycle sim;
  done;
  i.we := B.gnd;
  S.cycle sim;
  S.cycle sim;
  i.re := B.vdd;
  for j=0 to 12 do
    S.cycle sim;
  done;
  i.re := B.gnd;
  S.cycle sim;
  S.cycle sim;
  S.cycle sim;

  let waveform = new Widget.waveform () in
  let waves = W.{ cfg={default with wave_width=2}; waves } in
  waveform#set_waves waves;
  Lwt_main.run @@ Widget.run_widget waveform

let test_memory_line_sm () = 
  
  let module M' = Make(Cfg) in
  let module M = M'.Line_sm in
  let module G = Interface.Gen(B)(M.I)(M.O) in
  let module S = Cyclesim.Api in
  let _,sim,i,o,n = G.make "life" M.f in
  let sim, waves = Ws.wrap sim in

  let open M.I in
 
  S.reset sim;

  i.start := B.vdd;
  S.cycle sim;
  i.start := B.gnd;
  i.width := B.consti Cfg.x_bits 7;
  for j=0 to 30 do
    i.stall := B.srand 1;
    S.cycle sim;
  done;

  let waveform = new Widget.waveform () in
  let waves = W.{ cfg={default with wave_width=2}; waves } in
  waveform#set_waves waves;
  Lwt_main.run @@ Widget.run_widget waveform

let test_line () = 

  let module M' = Make(Cfg) in
  let module M = M'.Line in
  let module G = Interface.Gen(B)(M.I)(M.O) in
  let module S = Cyclesim.Api in
  let _,sim,i,o,n = G.make "life" M.f in
  let sim, waves = Ws.wrap sim in

  let open M.I in
  let open M'.Ram_I in
 
  S.reset sim;

  let abits = Cfg.x_bits - M'.log_bus_width in
  let width_words = 3 in
  i.width := B.consti Cfg.x_bits (Cfg.bus_width * width_words);

  let load_line d = 
    i.mem.we := B.vdd;
    for j=0 to width_words-1 do
      i.mem.d := d.(j);
      i.mem.wa := B.consti abits j;
      S.cycle sim;
    done;
    i.mem.we := B.gnd;
  in

  i.start := B.vdd;
  S.cycle sim;
  i.start := B.gnd;
  for j=0 to 30 do
    S.cycle sim;
  done;

  let waveform = new Widget.waveform () in
  let waves = W.{ cfg={default with wave_width=2}; waves } in
  waveform#set_waves waves;
  Lwt_main.run @@ Widget.run_widget waveform

