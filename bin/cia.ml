type pins =
  { portA : int
  ; portB : int
  ; _pc : int
  ; tod : bool
  ; cnt : bool
  ; sp : bool
  ; rs : int
  ; data : int
  ; phy2 : bool
  ; _flag : bool
  ; _cs : bool
  ; rw : bool
  ; _irq : bool
  }

type chip =
  { pra : int
  ; prb : int
  ; ddra : int
  ; ddrb : int
  ; talo : int
  ; tahi : int
  ; talolatch : int
  ; tahilatch : int
  ; tblo : int
  ; tbhi : int
  ; tblolatch : int
  ; tbhilatch : int
  ; tod10th : int
  ; todsec : int
  ; todmin : int
  ; todhr : int
  ; sdr : int
  ; icr : int
  ; cra : int
  ; crb : int
  }

let execute chip pins =
  let chip, pins =
    match pins._cs, pins.phy2 with
    | false, true ->
      Printf.printf "Respond to signal on rw\n";
      (match pins.rw with
       | true ->
         (* cia is read *)
         (* depending on the register select *)
         (* we output the value of the selected *)
         (* register on the data pins *)
         let chip, pins =
           match pins.rs with
           | 0x0 -> chip, { pins with data = pins.portA }
           | 0x1 -> chip, { pins with data = pins.portB }
           | 0x2 -> chip, { pins with data = chip.ddra }
           | 0x3 -> chip, { pins with data = chip.ddra }
           | 0x4 -> chip, { pins with data = chip.talo }
           | 0x5 -> chip, { pins with data = chip.tahi }
           | 0x6 -> chip, { pins with data = chip.tblo }
           | 0x7 -> chip, { pins with data = chip.tbhi }
           | 0x8 -> chip, { pins with data = chip.tod10th }
           | 0x9 -> chip, { pins with data = chip.todsec }
           | 0xA -> chip, { pins with data = chip.todmin }
           | 0xB -> chip, { pins with data = chip.todhr }
           | 0xC -> chip, { pins with data = chip.sdr }
           | 0xD -> chip, { pins with data = chip.icr }
           | 0xE -> chip, { pins with data = chip.cra }
           | 0xF -> chip, { pins with data = chip.crb }
           | _ -> failwith "Impossible value for RS"
         in
         chip, pins
       | false ->
         (* cia is written *)
         let chip, pins =
           match pins.rs with
           | 0x0 -> { chip with pra = pins.data }, pins
           | 0x1 -> { chip with prb = pins.data }, pins
           | 0x2 -> { chip with ddra = pins.data }, pins
           | 0x3 -> { chip with ddrb = pins.data }, pins
           | 0x4 -> { chip with talo = pins.data }, pins
           | 0x5 -> { chip with tahi = pins.data }, pins
           | 0x6 -> { chip with tblo = pins.data }, pins
           | 0x7 -> { chip with tbhi = pins.data }, pins
           | 0x8 -> { chip with tod10th = pins.data }, pins
           | 0x9 -> { chip with todsec = pins.data }, pins
           | 0xA -> { chip with todmin = pins.data }, pins
           | 0xB -> { chip with todhr = pins.data }, pins
           | 0xC -> { chip with sdr = pins.data }, pins
           | 0xD -> { chip with icr = pins.data }, pins
           | 0xE -> { chip with cra = pins.data }, pins
           | 0xF -> { chip with crb = pins.data }, pins
           | _ -> failwith "Impossible value for RS"
         in
         chip, pins)
    | false, false ->
      Printf.printf "Don not Respond to signal on RW\n";
      chip, pins
    | true, true ->
      Printf.printf "Chip disabled\n";
      chip, pins
    | true, false ->
      Printf.printf "Chip disabled\n";
      chip, pins
  in
  chip, pins
;;
