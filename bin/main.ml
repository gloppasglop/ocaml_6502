open Stdio
open Cpu

let () = print_endline "Hello, World!"

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
  ; a = 32
  ; x = 1
  ; y = 0x1
  ; sp = 0xFF
  ; ir = 0
  ; sr = 0x00
  ; adl = 0
  ; adh = 0
  ; tmp_data = 0
  }
;;

let usage_msg = "append [-verbose] <file1> [<file2>] ... -o <output>"
let input_file = ref ""
let pc = ref 0
let anon_fun _ = ()

let speclist =
  [ "-f", Arg.Set_string input_file, "Input file name"
  ; "--start", Arg.Set_int pc, "Program start"
  ]
;;

let () = Arg.parse speclist anon_fun usage_msg
let () = Printf.printf "Program: %s\n" !input_file
let pgm = In_channel.read_lines !input_file
let c6502 = { c6502 with pc = !pc }
let bus = { bus with address = !pc }
let () = Printf.printf "Start address: %04X\n" c6502.pc

(* TODO: Move to intelhex module *)
let load_pgm pgm mem =
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

let () = load_pgm pgm mem
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

(* TODO: Move that to another file *)
let pp_int8_bin i =
  (* Just support 8 bits*)
  let s = ref "" in
  for bit = 0 to 7 do
    s := !s ^ if (i lsr (7 - bit)) land 1 = 1 then "1" else "0"
  done;
  !s
;;

let rec run cycle cpu pins bus =
  if pins.rw
  then (
    (* We are reading from memory *)
    let cycle, cpu, pins, bus =
      Cpu.tick cycle cpu pins { bus with data = mem.(bus.address) }
    in
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
    run cycle cpu pins bus)
  else (
    (* we are writing to memory *)
    let cycle, cpu, pins, bus =
      if not pins.rw
      then
        (*
           Printf.printf "Writing %02X to %04X\n" bus.data bus.address;
        *)
        mem.(bus.address) <- bus.data;
      Cpu.tick cycle cpu pins { bus with data = mem.(bus.address) }
    in
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
    run cycle cpu pins bus)
;;

let () = run 0 c6502 pins bus
