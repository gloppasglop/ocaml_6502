open Stdio
open Cpu

type file_type =
  | HEX
  | BIN

let pins =
  { data = 0
  ; address = 0
  ; ioports = 0
  ; irq = false
  ; nmi = false
  ; reset = false
  ; rdy = true
  ; rw = true
  ; sync = true
  ; phy1 = true
  ; phy2 = true
  ; aec = true
  }
;;

let mem = Array.make 65536 0xFF
let bus = { address = 0; data = 0 }

let c6502 =
  { pc = 0
  ; a = 0x00
  ; x = 0x00
  ; y = 0x00
  ; sp = 0xFF
  ; ir = 0
  ; sr = 0b0011_0110
  ; adl = 0
  ; adh = 0
  ; tmp_data = 0
  }
;;

let call_stack = Array.make 65536 (0, 0, c6502, pins, bus)
let usage_msg = "append [-verbose] <file1> [<file2>] ... -o <output>"
let hex_file = ref ""
let bin_file = ref ""
let max_cycles_per_frame = ref 16420
let debug = ref false
let pc = ref 0
let anon_fun _ = ()
let dump_start_mem = ref 0
let breakpoint = ref 0x10000

let speclist =
  [ "-f", Arg.Set_string hex_file, "Input hex file name"
  ; "-b", Arg.Set_string bin_file, "Input binary file name"
  ; "--start", Arg.Set_int pc, "Program start"
  ; "--debug", Arg.Set debug, "Program start"
  ; "--cpf", Arg.Set_int max_cycles_per_frame, "Cycles per frame"
  ; "--dsm", Arg.Set_int dump_start_mem, "Dump Start Mem"
  ; "--break", Arg.Set_int breakpoint, "Breakpoint"
  ]
;;

let () = Arg.parse speclist anon_fun usage_msg
let () = Printf.printf "Program: %s\n" !hex_file
let () = Printf.printf "Program: %s\n" !bin_file
let file_type, input_file = if !hex_file <> "" then HEX, hex_file else BIN, bin_file
let c6502 = { c6502 with pc = !pc }
let bus = { bus with address = !pc }
let () = Printf.printf "Start address: %04X\n" c6502.pc

(* TODO: Move to intelhex module *)
let load_hex_pgm pgm mem =
  List.iter
    (fun str ->
      let byte_count = int_of_string ("0x" ^ String.sub str 1 2) in
      let address = int_of_string ("0x" ^ String.sub str 3 4) in
      let _record_type = int_of_string ("0x" ^ String.sub str 7 2) in
      for i = 0 to byte_count - 1 do
        let data = int_of_string ("0x" ^ String.sub str (9 + (2 * i)) 2) in
        mem.(address + i) <- data
      done)
    pgm
;;

let load_bin_pgm pgm mem = Bytes.iteri (fun i byte -> mem.(i) <- Char.code byte) pgm

let () =
  match file_type with
  | HEX ->
    Printf.printf "Reading HEX %s\n" !input_file;
    let pgm = In_channel.read_lines !input_file in
    load_hex_pgm pgm mem
  | BIN ->
    Printf.printf "Reading BIN %s\n" !input_file;
    let pgm = In_channel.read_all !input_file in
    load_bin_pgm (String.to_bytes pgm) mem
;;

(*
   let dump mem = Array.iteri (fun i b -> Printf.printf "%04X: %02X\n" i b) mem
*)

let () =
  Printf.printf
    "%5s %5s %4s %2s %2s %4s %2s %2s %2s\n"
    "SYNC"
    "RW"
    "ADDR"
    "D"
    "IR"
    "PC"
    "A"
    "X"
    "Y"
;;

let pp_int8_bin i =
  (* Just support 8 bits*)
  let s = ref "" in
  for bit = 0 to 7 do
    s := !s ^ if (i lsr (7 - bit)) land 1 = 1 then "1" else "0"
  done;
  !s
;;

(*
   Printf.printf
   "%5b %5b %04X %02X %02X %04X %02X %02X %02X %8s\n"
   pins.sync
   pins.rw
   bus.address
   bus.data
   cpu.ir
   cpu.pc
   cpu.a
   cpu.x
   cpu.y
   (pp_int8_bin cpu.sr);
*)

let width = 1000
let height = 640
let pixel_width = 16
let virtual_screen_width = 16
let is_stepped = !debug

let setup () =
  Raylib.init_window width height "raylib [core] example - basic window";
  Raylib.set_target_fps 60;
  0, is_stepped, !breakpoint, 0, c6502, pins, bus
;;

let rec run is_stepped breakpoint num_cycles cycle cpu pins bus =
  if num_cycles = 0
  then is_stepped, cycle, cpu, pins, bus
  else (
    let is_stepped =
      if breakpoint = cpu.pc
      then (
        Printf.printf "Breakpoint reached\n";
        true)
      else is_stepped
    in
    let num_cycles = if is_stepped then 1 else num_cycles in
    let cycle, cpu, pins, bus =
      if pins.rw
      then
        (* We are reading from memory *)
        Cpu.tick cycle cpu pins { bus with data = mem.(bus.address) }
      else (
        mem.(bus.address) <- bus.data;
        Cpu.tick cycle cpu pins { bus with data = mem.(bus.address) })
    in
    run is_stepped breakpoint (num_cycles - 1) cycle cpu pins bus)
;;

let rec loop (total_ticks, is_stepped, breakpoint, cycle, cpu, pins, bus) =
  let delta = Raylib.get_frame_time () in
  if Raylib.window_should_close ()
  then Raylib.close_window ()
  else (
    let is_stepped =
      if Raylib.is_key_released Raylib.Key.D then not is_stepped else is_stepped
    in
    let cycles_per_frame = if is_stepped then 1 else !max_cycles_per_frame in
    let total_ticks, is_stepped, cycle, cpu, pins, bus =
      if is_stepped
      then
        if Raylib.is_key_released Raylib.Key.S
        then (
          let total_ticks = total_ticks + cycles_per_frame in
          let is_stepped, cycle, cpu, pins, bus =
            run is_stepped breakpoint cycles_per_frame cycle cpu pins bus
          in
          call_stack.(total_ticks mod 65536) <- total_ticks, cycle, cpu, pins, bus;
          total_ticks, is_stepped, cycle, cpu, pins, bus)
        else total_ticks, is_stepped, cycle, cpu, pins, bus
      else (
        let total_ticks = total_ticks + cycles_per_frame in
        let is_stepped, cycle, cpu, pins, bus =
          run is_stepped breakpoint cycles_per_frame cycle cpu pins bus
        in
        call_stack.(total_ticks mod 65536) <- total_ticks, cycle, cpu, pins, bus;
        total_ticks, is_stepped, cycle, cpu, pins, bus)
    in
    let open Raylib in
    let col = Raygui.(get_style (Control.Default `Background_color)) |> get_color in
    begin_drawing ();
    clear_background col;
    for i = 0 to virtual_screen_width do
      for j = 0 to virtual_screen_width do
        let address = 0x0 + i + (virtual_screen_width * j) in
        let color = Color.create 0 0 (mem.(address) * 16) 255 in
        draw_rectangle
          (32 + (i * (pixel_width + 1)))
          (((pixel_width + 1) * j) + 128)
          pixel_width
          pixel_width
          color
      done
    done;
    draw_text
      (Printf.sprintf "FPS : %f \tStepped: %b" delta is_stepped)
      400
      32
      20
      Color.black;
    draw_text (Int.to_string (total_ticks / 985248)) 400 64 20 Color.black;
    for i = 0 to 8 do
      draw_text
        (Printf.sprintf "%02X" mem.(!dump_start_mem + i))
        (90 + (32 * i))
        520
        20
        Color.black
    done;
    for i = 0 to 20 do
      let m = (total_ticks - i) mod 65536 in
      let index = if m < 0 then 65536 + m else m in
      let total_ticks, cycle, cpu, pins, bus = call_stack.(index) in
      let _status =
        Printf.sprintf
          "%20d %02d %5b %5b %04X %02X %02X %04X %02X %02X %02X %02X %8s\n"
          total_ticks
          cycle
          pins.sync
          pins.rw
          bus.address
          bus.data
          cpu.ir
          cpu.pc
          cpu.a
          cpu.x
          cpu.y
          cpu.sp
          (pp_int8_bin cpu.sr)
      in
      draw_text _status 230 (96 + (32 * i)) 20 Color.black
    done;
    end_drawing ();
    loop (total_ticks, is_stepped, breakpoint, cycle, cpu, pins, bus))
;;

let () = setup () |> loop
