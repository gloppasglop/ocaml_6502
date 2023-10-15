type addressing_mode =
  | Accumulator
  | Implied
  | Immediate
  | Absolute
  | Zeropage
  | AbsoluteX
  | AbsoluteY
  | ZeropageX
  | ZeropageY
  | Indirect
  | PreIndexIndirect (* X-indexed, indirect.*)
  | PostIndexIndirect (* indirect, Y-indexed.*)
  | Relative

type mnemonic =
  | ADC (* add with carry *)
  | AND (* and (with accumulator) *)
  | ASL (* arithmetic shift left *)
  | BCC (* branch on carry clear *)
  | BCS (* branch on carry set *)
  | BEQ (* branch on equal (zero set) *)
  | BIT (* bit test *)
  | BMI (* branch on minus (negative set) *)
  | BNE (* branch on not equal (zero clear) *)
  | BPL (* branch on plus (negative clear) *)
  | BRK (* break / interrupt *)
  | BVC (* branch on overflow clear *)
  | BVS (* branch on overflow set *)
  | CLC (* clear carry *)
  | CLD (* clear decimal *)
  | CLI (* clear interrupt disable *)
  | CLV (* clear overflow *)
  | CMP (* compare (with accumulator) *)
  | CPX (* compare with X *)
  | CPY (* compare with Y *)
  | DEC (* decrement *)
  | DEX (* decrement X *)
  | DEY (* decrement Y *)
  | EOR (* exclusive or (with accumulator) *)
  | INC (* increment *)
  | INX (* increment X *)
  | INY (* increment Y *)
  | JMP (* jump *)
  | JSR (* jump subroutine *)
  | LDA (* load accumulator *)
  | LDX (* load X *)
  | LDY (* load Y *)
  | LSR (* logical shift right *)
  | NOP (* no operation *)
  | ORA (* or with accumulator *)
  | PHA (* push accumulator *)
  | PHP (* push processor status (SR) *)
  | PLA (* pull accumulator *)
  | PLP (* pull processor status (SR) *)
  | ROL (* rotate left *)
  | ROR (* rotate right *)
  | RTI (* return from interrupt *)
  | RTS (* return from subroutine *)
  | SBC (* subtract with carry *)
  | SEC (* set carry *)
  | SED (* set decimal *)
  | SEI (* set interrupt disable *)
  | STA (* store accumulator *)
  | STX (* store X *)
  | STY (* store Y *)
  | TAX (* transfer accumulator to X *)
  | TAY (* transfer accumulator to Y *)
  | TSX (* transfer stack pointer to X *)
  | TXA (* transfer X to accumulator *)
  | TXS (* transfer X to stack pointer *)
  | TYA (* transfer Y to accumulator *)

type pins =
  { data : int
  ; address : int
  ; ioports : int
  ; irq : bool
  ; nmi : bool
  ; reset : bool
  ; rdy : bool
  ; rw : bool
  ; sync : bool
  ; phy1 : bool
  ; phy2 : bool
  ; aec : bool
  }

type bus =
  { address : int
  ; data : int
  }

type cpu =
  { pc : int (* Program counter*)
  ; a : int (* Accumulator *)
  ; x : int (* X register *)
  ; y : int (* Y register *)
  ; sp : int (* Stack pointer *)
  ; ir : int (* Instruction register *)
  ; sr : int (* Status register *)
  ; adl : int (* Internal PC Low *)
  ; adh : int (* Internal PC high *)
  ; tmp_data : int
  }
[@@deriving show]

type instruction =
  { mnemonic : mnemonic
  ; addressing : addressing_mode
  ; bytes : int
  ; cycles : int
  }

let opcode_to_inst opcode =
  match opcode with
  | 0xEA -> { mnemonic = NOP; addressing = Implied; bytes = 1; cycles = 2 }
  | 0xCA -> { mnemonic = DEX; addressing = Implied; bytes = 1; cycles = 2 }
  | 0x88 -> { mnemonic = DEY; addressing = Implied; bytes = 1; cycles = 2 }
  | 0xE8 -> { mnemonic = INX; addressing = Implied; bytes = 1; cycles = 2 }
  | 0xC8 -> { mnemonic = INY; addressing = Implied; bytes = 1; cycles = 2 }
  | 0x18 -> { mnemonic = CLC; addressing = Implied; bytes = 1; cycles = 2 }
  | 0xD8 -> { mnemonic = CLD; addressing = Implied; bytes = 1; cycles = 2 }
  | 0x58 -> { mnemonic = CLI; addressing = Implied; bytes = 1; cycles = 2 }
  | 0xB8 -> { mnemonic = CLV; addressing = Implied; bytes = 1; cycles = 2 }
  | 0x38 -> { mnemonic = SEC; addressing = Implied; bytes = 1; cycles = 2 }
  | 0xF8 -> { mnemonic = SED; addressing = Implied; bytes = 1; cycles = 2 }
  | 0x78 -> { mnemonic = SEI; addressing = Implied; bytes = 1; cycles = 2 }
  | 0xAA -> { mnemonic = TAX; addressing = Implied; bytes = 1; cycles = 2 }
  | 0xA8 -> { mnemonic = TAY; addressing = Implied; bytes = 1; cycles = 2 }
  | 0xBA -> { mnemonic = TSX; addressing = Implied; bytes = 1; cycles = 2 }
  | 0x8A -> { mnemonic = TXA; addressing = Implied; bytes = 1; cycles = 2 }
  | 0x9A -> { mnemonic = TXS; addressing = Implied; bytes = 1; cycles = 2 }
  | 0x98 -> { mnemonic = TYA; addressing = Implied; bytes = 1; cycles = 2 }
  (* Accumulator *)
  | 0x0A -> { mnemonic = ASL; addressing = Accumulator; bytes = 1; cycles = 2 }
  | 0x4A -> { mnemonic = LSR; addressing = Accumulator; bytes = 1; cycles = 2 }
  | 0x2A -> { mnemonic = ROL; addressing = Accumulator; bytes = 1; cycles = 2 }
  | 0x6A -> { mnemonic = ROR; addressing = Accumulator; bytes = 1; cycles = 2 }
  (* Immediate *)
  | 0x69 -> { mnemonic = ADC; addressing = Immediate; bytes = 2; cycles = 2 }
  | 0x29 -> { mnemonic = AND; addressing = Immediate; bytes = 2; cycles = 2 }
  | 0xC9 -> { mnemonic = CMP; addressing = Immediate; bytes = 2; cycles = 2 }
  | 0xE0 -> { mnemonic = CPX; addressing = Immediate; bytes = 2; cycles = 2 }
  | 0xC0 -> { mnemonic = CPY; addressing = Immediate; bytes = 2; cycles = 2 }
  | 0x49 -> { mnemonic = EOR; addressing = Immediate; bytes = 2; cycles = 2 }
  | 0xA9 -> { mnemonic = LDA; addressing = Immediate; bytes = 2; cycles = 2 }
  | 0xA2 -> { mnemonic = LDX; addressing = Immediate; bytes = 2; cycles = 2 }
  | 0xA0 -> { mnemonic = LDY; addressing = Immediate; bytes = 2; cycles = 2 }
  | 0x09 -> { mnemonic = ORA; addressing = Immediate; bytes = 2; cycles = 2 }
  | 0xE9 -> { mnemonic = SBC; addressing = Immediate; bytes = 2; cycles = 2 }
  (* Absolute *)
  | 0x4C -> { mnemonic = JMP; addressing = Absolute; bytes = 3; cycles = 3 }
  (* Absolute Read*)
  | 0x6D -> { mnemonic = ADC; addressing = Absolute; bytes = 3; cycles = 4 }
  | 0x2D -> { mnemonic = AND; addressing = Absolute; bytes = 3; cycles = 4 }
  | 0x2C -> { mnemonic = BIT; addressing = Absolute; bytes = 3; cycles = 4 }
  | 0xCD -> { mnemonic = CMP; addressing = Absolute; bytes = 3; cycles = 4 }
  | 0xEC -> { mnemonic = CPY; addressing = Absolute; bytes = 3; cycles = 4 }
  | 0xCC -> { mnemonic = CPX; addressing = Absolute; bytes = 3; cycles = 4 }
  | 0x4D -> { mnemonic = EOR; addressing = Absolute; bytes = 3; cycles = 4 }
  | 0xAD -> { mnemonic = LDA; addressing = Absolute; bytes = 3; cycles = 4 }
  | 0xAE -> { mnemonic = LDX; addressing = Absolute; bytes = 3; cycles = 4 }
  | 0xAC -> { mnemonic = LDY; addressing = Absolute; bytes = 3; cycles = 4 }
  | 0x0D -> { mnemonic = ORA; addressing = Absolute; bytes = 3; cycles = 4 }
  | 0xED -> { mnemonic = SBC; addressing = Absolute; bytes = 3; cycles = 4 }
  (* Absolute Read-Modify-Write instructions *)
  | 0x0E -> { mnemonic = ASL; addressing = Absolute; bytes = 3; cycles = 6 }
  | 0xCE -> { mnemonic = DEC; addressing = Absolute; bytes = 3; cycles = 6 }
  | 0xEE -> { mnemonic = INC; addressing = Absolute; bytes = 3; cycles = 6 }
  | 0x4E -> { mnemonic = LSR; addressing = Absolute; bytes = 3; cycles = 6 }
  | 0x2E -> { mnemonic = ROL; addressing = Absolute; bytes = 3; cycles = 6 }
  | 0x6E -> { mnemonic = ROR; addressing = Absolute; bytes = 3; cycles = 6 }
  (* Absolute Write instructions*)
  | 0x8D -> { mnemonic = STA; addressing = Absolute; bytes = 3; cycles = 4 }
  | 0x8E -> { mnemonic = STX; addressing = Absolute; bytes = 3; cycles = 4 }
  | 0x8C -> { mnemonic = STY; addressing = Absolute; bytes = 3; cycles = 4 }
  (* ZeroPage *)
  (* ZeroPage Read *)
  | 0x65 -> { mnemonic = ADC; addressing = Zeropage; bytes = 2; cycles = 3 }
  | 0x25 -> { mnemonic = AND; addressing = Zeropage; bytes = 2; cycles = 3 }
  | 0x24 -> { mnemonic = BIT; addressing = Zeropage; bytes = 2; cycles = 3 }
  | 0xC5 -> { mnemonic = CMP; addressing = Zeropage; bytes = 2; cycles = 3 }
  | 0x45 -> { mnemonic = EOR; addressing = Zeropage; bytes = 2; cycles = 3 }
  | 0xA5 -> { mnemonic = LDA; addressing = Zeropage; bytes = 2; cycles = 3 }
  | 0xA6 -> { mnemonic = LDX; addressing = Zeropage; bytes = 2; cycles = 3 }
  | 0xA4 -> { mnemonic = LDY; addressing = Zeropage; bytes = 2; cycles = 3 }
  | 0x05 -> { mnemonic = ORA; addressing = Zeropage; bytes = 2; cycles = 3 }
  | 0xE5 -> { mnemonic = SBC; addressing = Zeropage; bytes = 2; cycles = 3 }
  (* Zeropage Read-Modify-Write instructions *)
  | 0x06 -> { mnemonic = ASL; addressing = Absolute; bytes = 2; cycles = 5 }
  | 0xC6 -> { mnemonic = DEC; addressing = Absolute; bytes = 2; cycles = 5 }
  | 0xE6 -> { mnemonic = INC; addressing = Absolute; bytes = 2; cycles = 5 }
  | 0x46 -> { mnemonic = LSR; addressing = Absolute; bytes = 2; cycles = 5 }
  | 0x26 -> { mnemonic = ROL; addressing = Absolute; bytes = 2; cycles = 5 }
  | 0x66 -> { mnemonic = ROR; addressing = Absolute; bytes = 2; cycles = 5 }
  | _ -> failwith (Printf.sprintf "Opcode %02X Unimplemented" opcode)
;;

(* TODO: Use binary arithmetic *)
let set_nz v sr =
  let sr = if v = 0 then sr lor 0b0000_0010 else sr land 0b1111_1101 in
  if v land 0b1000_0000 = 0b1000_0000 then sr lor 0b1000_0000 else sr land 0b0111_1111
;;

let inst_adc x y sr =
  (* TODO : Handle decimal mode *)
  let r = x + y + (sr land 0b0000_0001) in
  (* overflow if signe of a and m are the same but sum  result is deifferent sign *)
  let overflow =
    if lnot (x lxor y) land (r lxor x land 0x80) = 0 then 0 else 0b0100_0000
  in
  let sr =
    set_nz (0xFF land r) sr
    land 0b1111_1110
    lor ((0x100 land r) lsr 8)
    land 0b1011_1111
    lor overflow
  in
  r land 0xFF, sr
;;

let inst_sbc x y sr =
  (* TODO : Handle decimal mode *)
  let r = x + (0xFF lxor y) + (sr land 0b0000_0001 land 0xFF) in
  (* overflow if signe of a and m are the same but sum  result is deifferent sign *)
  let overflow =
    if lnot (x lxor y) land (r lxor x land 0x80) = 0 then 0 else 0b0100_0000
  in
  let sr =
    set_nz (0xFF land r) sr
    land 0b1111_1110
    lor ((0x100 land r) lsr 8)
    land 0b1011_1111
    lor overflow
  in
  r land 0xFF, sr
;;

let inst_and x y sr =
  let r = x land y in
  let sr = set_nz r sr in
  r, sr
;;

let inst_eor x y sr =
  let r = x lxor y in
  let sr = set_nz r sr in
  r, sr
;;

let inst_ora x y sr =
  let r = x lor y in
  let sr = set_nz r sr in
  r, sr
;;

let inst_cmp x y sr =
  let r = x - y in
  let c = if x >= y then 1 else 0 in
  let sr = set_nz (0xFF land r) sr land 0b1111_1110 lor c in
  sr
;;

let inst_asl x sr =
  let r = x lsl 1 in
  let sr = set_nz (0xFF land r) sr land 0b1111_1110 lor ((0x100 land r) lsr 8) in
  r land 0xFF, sr
;;

let inst_lsr x sr =
  let bit0 = x land 0b000_0001 in
  let r = x lsr 1 in
  let sr = set_nz r sr land 0b1111_1110 lor bit0 in
  r land 0xFF, sr
;;

let inst_rol x sr =
  let c = sr land 0b0000_0001 in
  let r = (x lsl 1) lor c in
  let sr = set_nz (0xFF land r) sr land 0b1111_1110 lor ((0x100 land r) lsr 8) in
  r land 0xFF, sr
;;

let inst_ror x sr =
  let bit0 = x land 0b000_0001 in
  let c = sr land 0b0000_0001 in
  let r = (x lsr 1) lor (c lsl 7) in
  let sr = set_nz r sr land 0b1111_1110 lor bit0 in
  r land 0xFF, sr
;;

let inst_bit x y sr =
  let r = x land y in
  let sr = set_nz r sr land 0b0011_1111 lor (0b1100_0000 land y) in
  sr
;;

let execute inst cycle cpu pins bus =
  match inst.addressing with
  | Implied ->
    let next_cycle = cycle + 1 in
    (match cycle with
     | 0 ->
       ( next_cycle
       , { cpu with pc = cpu.pc + 1 }
       , { pins with rw = true }
       , { bus with address = cpu.pc + 1 } )
     | 1 ->
       let next_cycle = 0 in
       let cpu', pins', bus' =
         cpu, { pins with rw = true; sync = true }, { bus with address = cpu.pc }
       in
       (match inst.mnemonic with
        | NOP -> next_cycle, cpu', pins', bus'
        | BRK -> failwith "BRK not implemented"
        | CLC -> next_cycle, { cpu' with sr = cpu'.sr land 0b1111_1110 }, pins', bus'
        | CLD -> next_cycle, { cpu' with sr = cpu'.sr land 0b1111_0111 }, pins', bus'
        | CLI -> next_cycle, { cpu' with sr = cpu'.sr land 0b1111_1011 }, pins', bus'
        | CLV -> next_cycle, { cpu' with sr = cpu'.sr land 0b1011_1111 }, pins', bus'
        | DEX ->
          let v = 0xFF land (cpu'.x - 1) in
          let sr = set_nz v cpu'.sr in
          next_cycle, { cpu' with x = v; sr }, pins', bus'
        | DEY ->
          let v = 0xFF land (cpu'.y - 1) in
          let sr = set_nz v cpu'.sr in
          next_cycle, { cpu' with y = v; sr }, pins', bus'
        | INX ->
          let v = 0xFF land (cpu'.x + 1) in
          let sr = set_nz v cpu'.sr in
          next_cycle, { cpu' with x = v; sr }, pins', bus'
        | INY ->
          let v = 0xFF land (cpu'.y + 1) in
          let sr = set_nz v cpu'.sr in
          next_cycle, { cpu' with y = v; sr }, pins', bus'
        | PHA -> failwith "PHA not implemented"
        | PHP -> failwith "PHP not implemented"
        | PLA -> failwith "PLA not implemented"
        | PLP -> failwith "PLP not implemented"
        | RTI -> failwith "RTI not implemented"
        | RTS -> failwith "RTS not implemented"
        | SEC -> next_cycle, { cpu' with sr = cpu'.sr lor 0b0000_0001 }, pins', bus'
        | SED -> next_cycle, { cpu' with sr = cpu'.sr lor 0b0000_1000 }, pins', bus'
        | SEI -> next_cycle, { cpu' with sr = cpu'.sr lor 0b0000_0100 }, pins', bus'
        | TAX ->
          let a = cpu'.a in
          let sr = set_nz a cpu'.sr in
          next_cycle, { cpu' with x = a; sr }, pins', bus'
        | TAY ->
          let a = cpu'.a in
          let sr = set_nz a cpu'.sr in
          next_cycle, { cpu' with y = a; sr }, pins', bus'
        | TSX ->
          let s = cpu'.sr in
          let sr = set_nz s cpu'.sr in
          next_cycle, { cpu' with x = s; sr }, pins', bus'
        | TXA ->
          let x = cpu'.x in
          let sr = set_nz x cpu'.sr in
          next_cycle, { cpu' with a = x; sr }, pins', bus'
        | TXS ->
          let x = cpu'.x in
          next_cycle, { cpu' with sr = x }, pins', bus'
        | TYA ->
          let y = cpu'.y in
          let sr = set_nz y cpu'.sr in
          next_cycle, { cpu' with a = y; sr }, pins', bus'
        | _ -> failwith "Invalid Implied opcode")
     | _ -> failwith "op cycle too large")
  | Accumulator ->
    let next_cycle = cycle + 1 in
    (match cycle with
     | 0 ->
       ( next_cycle
       , { cpu with pc = cpu.pc + 1 }
       , { pins with rw = true }
       , { bus with address = cpu.pc + 1 } )
     | 1 ->
       let next_cycle = 0 in
       let cpu', pins', bus' =
         cpu, { pins with rw = true; sync = true }, { bus with address = cpu.pc }
       in
       (match inst.mnemonic with
        | ASL ->
          let a, sr = inst_asl cpu'.a cpu'.sr in
          next_cycle, { cpu' with a; sr }, pins', bus'
        | LSR ->
          let a, sr = inst_lsr cpu'.a cpu'.sr in
          next_cycle, { cpu' with a; sr }, pins', bus'
        | ROL ->
          let a, sr = inst_rol cpu'.a cpu'.sr in
          next_cycle, { cpu' with a; sr }, pins', bus'
        | ROR ->
          let a, sr = inst_ror cpu'.a cpu'.sr in
          next_cycle, { cpu' with a; sr }, pins', bus'
        | _ -> failwith "Invalid Accumulator opcode")
     | _ -> failwith "op cycle too large")
  | Immediate ->
    let next_cycle = cycle + 1 in
    (match cycle with
     | 0 ->
       ( next_cycle
       , { cpu with pc = cpu.pc + 1 }
       , { pins with rw = true }
       , { bus with address = cpu.pc + 1 } )
     | 1 ->
       let next_cycle = 0 in
       let cpu', pins', bus' =
         ( { cpu with pc = cpu.pc + 1 }
         , { pins with rw = true; sync = true }
         , { bus with address = cpu.pc + 1 } )
       in
       (match inst.mnemonic with
        | ADC ->
          (* TODO : Handle decimal mode *)
          let a, sr = inst_adc cpu'.a bus'.data cpu'.sr in
          next_cycle, { cpu' with a; sr }, pins', bus'
        | AND ->
          let a, sr = inst_and cpu'.a bus'.data cpu'.sr in
          next_cycle, { cpu' with a; sr }, pins', bus'
        | CMP ->
          let sr = inst_cmp cpu'.a bus'.data cpu'.sr in
          next_cycle, { cpu' with sr }, pins', bus'
        | CPX ->
          let sr = inst_cmp cpu'.x bus'.data cpu'.sr in
          next_cycle, { cpu' with sr }, pins', bus'
        | CPY ->
          let sr = inst_cmp cpu'.y bus'.data cpu'.sr in
          next_cycle, { cpu' with sr }, pins', bus'
        | EOR ->
          let a, sr = inst_eor cpu'.a bus'.data cpu'.sr in
          next_cycle, { cpu' with a; sr }, pins', bus'
        | LDA ->
          let v = bus'.data in
          let sr = set_nz v cpu'.sr in
          next_cycle, { cpu' with a = v; sr }, pins', bus'
        | LDX ->
          let v = bus'.data in
          let sr = set_nz v cpu'.sr in
          next_cycle, { cpu' with x = v; sr }, pins', bus'
        | LDY ->
          let v = bus'.data in
          let sr = set_nz v cpu'.sr in
          next_cycle, { cpu' with y = v; sr }, pins', bus'
        | ORA ->
          let a, sr = inst_ora cpu'.a bus'.data cpu'.sr in
          next_cycle, { cpu' with a; sr }, pins', bus'
        | SBC ->
          let a, sr = inst_sbc cpu'.a bus'.data cpu'.sr in
          next_cycle, { cpu' with a; sr }, pins', bus'
        | _ -> failwith "Invalid Immediate opcode")
     | _ -> failwith "op cycle too large")
  | Absolute ->
    let next_cycle = cycle + 1 in
    (match cycle with
     | 0 ->
       ( next_cycle
       , { cpu with pc = cpu.pc + 1 }
       , { pins with rw = true }
       , { bus with address = cpu.pc + 1 } )
     | 1 ->
       ( next_cycle
       , { cpu with adl = bus.data; pc = cpu.pc + 1 }
       , { pins with rw = true }
       , { bus with address = cpu.pc + 1 } )
     | 2 ->
       (match inst.mnemonic with
        | JMP ->
          ( 0
          , { cpu with pc = cpu.adl lor (bus.data lsl 8) }
          , { pins with rw = true; sync = true }
          , { bus with address = cpu.adl lor (bus.data lsl 8) } )
        | STA ->
          ( next_cycle
          , { cpu with adh = bus.data; pc = cpu.pc + 1 }
          , { pins with rw = false }
          , { address = cpu.adl lor (bus.data lsl 8); data = cpu.a } )
        | STX ->
          ( next_cycle
          , { cpu with adh = bus.data; pc = cpu.pc + 1 }
          , { pins with rw = false }
          , { address = cpu.adl lor (bus.data lsl 8); data = cpu.x } )
        | STY ->
          ( next_cycle
          , { cpu with adh = bus.data; pc = cpu.pc + 1 }
          , { pins with rw = false }
          , { address = cpu.adl lor (bus.data lsl 8); data = cpu.y } )
        | _ ->
          ( next_cycle
          , { cpu with adh = bus.data; pc = cpu.pc + 1 }
          , { pins with rw = true }
          , { bus with address = cpu.adl lor (bus.data lsl 8) } ))
     | 3 ->
       (match inst.mnemonic with
        (* Read instructions *)
        | ADC ->
          (* TODO : Handle decimal mode *)
          let a, sr = inst_adc cpu.a bus.data cpu.sr in
          ( 0
          , { cpu with a; sr }
          , { pins with rw = true; sync = true }
          , { bus with address = cpu.pc } )
        | AND ->
          let a, sr = inst_and cpu.a bus.data cpu.sr in
          ( 0
          , { cpu with a; sr }
          , { pins with rw = true; sync = true }
          , { bus with address = cpu.pc } )
        | BIT ->
          let sr = inst_bit cpu.a bus.data cpu.sr in
          ( 0
          , { cpu with sr }
          , { pins with rw = true; sync = true }
          , { bus with address = cpu.pc } )
        | CMP ->
          let sr = inst_cmp cpu.a bus.data cpu.sr in
          ( 0
          , { cpu with sr }
          , { pins with rw = true; sync = true }
          , { bus with address = cpu.pc } )
        | CPX ->
          let sr = inst_cmp cpu.x bus.data cpu.sr in
          ( 0
          , { cpu with sr }
          , { pins with rw = true; sync = true }
          , { bus with address = cpu.pc } )
        | CPY ->
          let sr = inst_cmp cpu.y bus.data cpu.sr in
          ( 0
          , { cpu with sr }
          , { pins with rw = true; sync = true }
          , { bus with address = cpu.pc } )
        | EOR ->
          let a, sr = inst_eor cpu.a bus.data cpu.sr in
          ( 0
          , { cpu with a; sr }
          , { pins with rw = true; sync = true }
          , { bus with address = cpu.pc } )
        | LDA ->
          let a = bus.data in
          let sr = set_nz a cpu.sr in
          ( 0
          , { cpu with a; sr }
          , { pins with rw = true; sync = true }
          , { bus with address = cpu.pc } )
        | LDX ->
          let x = bus.data in
          let sr = set_nz x cpu.sr in
          ( 0
          , { cpu with x; sr }
          , { pins with rw = true; sync = true }
          , { bus with address = cpu.pc } )
        | LDY ->
          let y = bus.data in
          let sr = set_nz y cpu.sr in
          ( 0
          , { cpu with y; sr }
          , { pins with rw = true; sync = true }
          , { bus with address = cpu.pc } )
        | ORA ->
          let a, sr = inst_ora cpu.a bus.data cpu.sr in
          ( 0
          , { cpu with a; sr }
          , { pins with rw = true; sync = true }
          , { bus with address = cpu.pc } )
        | SBC ->
          (* TODO : Handle decimal mode *)
          let a, sr = inst_sbc cpu.a bus.data cpu.sr in
          ( 0
          , { cpu with a; sr }
          , { pins with rw = true; sync = true }
          , { bus with address = cpu.pc } )
        | STA | STX | STY ->
          0, cpu, { pins with rw = true; sync = true }, { bus with address = cpu.pc }
        | ASL | DEC | INC | LSR | ROL | ROR ->
          next_cycle, { cpu with tmp_data = bus.data }, { pins with rw = false }, bus
        | _ -> failwith "Not implemented")
     | 4 ->
       (match inst.mnemonic with
        | ASL ->
          let r, sr = inst_asl cpu.tmp_data cpu.sr in
          next_cycle, { cpu with sr }, { pins with rw = false }, { bus with data = r }
        | DEC ->
          let r = 0xFF land (cpu.tmp_data - 1) in
          let sr = set_nz r cpu.sr in
          next_cycle, { cpu with sr }, { pins with rw = false }, { bus with data = r }
        | INC ->
          let r = 0xFF land (cpu.tmp_data + 1) in
          let sr = set_nz r cpu.sr in
          next_cycle, { cpu with sr }, { pins with rw = false }, { bus with data = r }
        | LSR ->
          let r, sr = inst_lsr cpu.tmp_data cpu.sr in
          next_cycle, { cpu with sr }, { pins with rw = false }, { bus with data = r }
        | ROL ->
          let r, sr = inst_rol cpu.tmp_data cpu.sr in
          next_cycle, { cpu with sr }, { pins with rw = false }, { bus with data = r }
        | ROR ->
          let r, sr = inst_ror cpu.tmp_data cpu.sr in
          next_cycle, { cpu with sr }, { pins with rw = false }, { bus with data = r }
        | _ -> failwith "Cycle 4 Absolute not implemented")
     | 5 ->
       (match inst.mnemonic with
        | ASL | DEC | INC | LSR | ROL | ROR ->
          0, cpu, { pins with rw = true; sync = true }, { bus with address = cpu.pc }
        | _ -> failwith "Cycle 5 Absolute not implemented")
     | _ -> failwith "op cycle too large")
  | Zeropage ->
    let next_cycle = cycle + 1 in
    (match cycle with
     | 0 ->
       ( next_cycle
       , { cpu with pc = cpu.pc + 1 }
       , { pins with rw = true }
       , { bus with address = cpu.pc + 1 } )
     | 1 ->
       ( next_cycle
       , { cpu with pc = cpu.pc + 1 }
       , { pins with rw = true }
       , { bus with address = bus.data } )
     | 2 ->
       (match inst.mnemonic with
        (* Read instructions *)
        | ADC ->
          (* TODO : Handle decimal mode *)
          let a, sr = inst_adc cpu.a bus.data cpu.sr in
          ( 0
          , { cpu with a; sr }
          , { pins with rw = true; sync = true }
          , { bus with address = cpu.pc } )
        | AND ->
          let a, sr = inst_and cpu.a bus.data cpu.sr in
          ( 0
          , { cpu with a; sr }
          , { pins with rw = true; sync = true }
          , { bus with address = cpu.pc } )
        | BIT ->
          let sr = inst_bit cpu.a bus.data cpu.sr in
          ( 0
          , { cpu with sr }
          , { pins with rw = true; sync = true }
          , { bus with address = cpu.pc } )
        | CMP ->
          let sr = inst_cmp cpu.a bus.data cpu.sr in
          ( 0
          , { cpu with sr }
          , { pins with rw = true; sync = true }
          , { bus with address = cpu.pc } )
        | EOR ->
          let a, sr = inst_eor cpu.a bus.data cpu.sr in
          ( 0
          , { cpu with a; sr }
          , { pins with rw = true; sync = true }
          , { bus with address = cpu.pc } )
        | LDA ->
          let a = bus.data in
          let sr = set_nz a cpu.sr in
          ( 0
          , { cpu with a; sr }
          , { pins with rw = true; sync = true }
          , { bus with address = cpu.pc } )
        | LDX ->
          let x = bus.data in
          let sr = set_nz x cpu.sr in
          ( 0
          , { cpu with x; sr }
          , { pins with rw = true; sync = true }
          , { bus with address = cpu.pc } )
        | LDY ->
          let y = bus.data in
          let sr = set_nz y cpu.sr in
          ( 0
          , { cpu with y; sr }
          , { pins with rw = true; sync = true }
          , { bus with address = cpu.pc } )
        | ORA ->
          let a, sr = inst_ora cpu.a bus.data cpu.sr in
          ( 0
          , { cpu with a; sr }
          , { pins with rw = true; sync = true }
          , { bus with address = cpu.pc } )
        | SBC ->
          (* TODO : Handle decimal mode *)
          let a, sr = inst_sbc cpu.a bus.data cpu.sr in
          ( 0
          , { cpu with a; sr }
          , { pins with rw = true; sync = true }
          , { bus with address = cpu.pc } )
        | _ -> failwith "Invalid Zeropage opcode")
     | _ -> failwith "op cycle too large")
  | _ -> failwith "Addressing Not implemented"
;;

let fetch_and_decode cycle cpu pins bus =
  if pins.sync
  then (
    let cpu' = { cpu with ir = bus.data } in
    let pins' = { pins with rw = true; sync = false } in
    let cycle, cpu, pins, bus = execute (opcode_to_inst cpu'.ir) cycle cpu' pins' bus in
    cycle, cpu, pins, bus)
  else (
    let cycle, cpu, pins, bus = execute (opcode_to_inst cpu.ir) cycle cpu pins bus in
    cycle, cpu, pins, bus)
;;

let tick cpu bus = fetch_and_decode cpu bus
