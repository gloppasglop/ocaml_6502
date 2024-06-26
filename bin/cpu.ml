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
  | 0x00 -> { mnemonic = BRK; addressing = Implied; bytes = 1; cycles = 7 }
  | 0x48 -> { mnemonic = PHA; addressing = Implied; bytes = 1; cycles = 3 }
  | 0x68 -> { mnemonic = PLA; addressing = Implied; bytes = 1; cycles = 4 }
  | 0x28 -> { mnemonic = PLP; addressing = Implied; bytes = 1; cycles = 4 }
  | 0x08 -> { mnemonic = PHP; addressing = Implied; bytes = 1; cycles = 3 }
  | 0x40 -> { mnemonic = RTI; addressing = Implied; bytes = 1; cycles = 6 }
  | 0x60 -> { mnemonic = RTS; addressing = Implied; bytes = 1; cycles = 6 }
  | 0x20 -> { mnemonic = JSR; addressing = Implied; bytes = 3; cycles = 6 }
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
  | 0xEC -> { mnemonic = CPX; addressing = Absolute; bytes = 3; cycles = 4 }
  | 0xCC -> { mnemonic = CPY; addressing = Absolute; bytes = 3; cycles = 4 }
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
  | 0xE4 -> { mnemonic = CPX; addressing = Zeropage; bytes = 2; cycles = 3 }
  | 0xC4 -> { mnemonic = CPY; addressing = Zeropage; bytes = 2; cycles = 3 }
  | 0x45 -> { mnemonic = EOR; addressing = Zeropage; bytes = 2; cycles = 3 }
  | 0xA5 -> { mnemonic = LDA; addressing = Zeropage; bytes = 2; cycles = 3 }
  | 0xA6 -> { mnemonic = LDX; addressing = Zeropage; bytes = 2; cycles = 3 }
  | 0xA4 -> { mnemonic = LDY; addressing = Zeropage; bytes = 2; cycles = 3 }
  | 0x05 -> { mnemonic = ORA; addressing = Zeropage; bytes = 2; cycles = 3 }
  | 0xE5 -> { mnemonic = SBC; addressing = Zeropage; bytes = 2; cycles = 3 }
  (* Zeropage Read-Modify-Write instructions *)
  | 0x06 -> { mnemonic = ASL; addressing = Zeropage; bytes = 2; cycles = 5 }
  | 0xC6 -> { mnemonic = DEC; addressing = Zeropage; bytes = 2; cycles = 5 }
  | 0xE6 -> { mnemonic = INC; addressing = Zeropage; bytes = 2; cycles = 5 }
  | 0x46 -> { mnemonic = LSR; addressing = Zeropage; bytes = 2; cycles = 5 }
  | 0x26 -> { mnemonic = ROL; addressing = Zeropage; bytes = 2; cycles = 5 }
  | 0x66 -> { mnemonic = ROR; addressing = Zeropage; bytes = 2; cycles = 5 }
  (* Zeropage Write instructions*)
  | 0x85 -> { mnemonic = STA; addressing = Zeropage; bytes = 2; cycles = 3 }
  | 0x86 -> { mnemonic = STX; addressing = Zeropage; bytes = 2; cycles = 3 }
  | 0x84 -> { mnemonic = STY; addressing = Zeropage; bytes = 2; cycles = 3 }
  (* ZeropageX*)
  (* ZeroPageX Read *)
  | 0x75 -> { mnemonic = ADC; addressing = ZeropageX; bytes = 2; cycles = 4 }
  | 0x35 -> { mnemonic = AND; addressing = ZeropageX; bytes = 2; cycles = 4 }
  | 0xD5 -> { mnemonic = CMP; addressing = ZeropageX; bytes = 2; cycles = 4 }
  | 0x55 -> { mnemonic = EOR; addressing = ZeropageX; bytes = 2; cycles = 4 }
  | 0xB5 -> { mnemonic = LDA; addressing = ZeropageX; bytes = 2; cycles = 4 }
  | 0xB4 -> { mnemonic = LDY; addressing = ZeropageX; bytes = 2; cycles = 4 }
  | 0x15 -> { mnemonic = ORA; addressing = ZeropageX; bytes = 2; cycles = 4 }
  | 0xF5 -> { mnemonic = SBC; addressing = ZeropageX; bytes = 2; cycles = 4 }
  (* ZeropageX Read-Modify-Write instructions *)
  | 0x16 -> { mnemonic = ASL; addressing = ZeropageX; bytes = 2; cycles = 6 }
  | 0xD6 -> { mnemonic = DEC; addressing = ZeropageX; bytes = 2; cycles = 6 }
  | 0xF6 -> { mnemonic = INC; addressing = ZeropageX; bytes = 2; cycles = 6 }
  | 0x56 -> { mnemonic = LSR; addressing = ZeropageX; bytes = 2; cycles = 6 }
  | 0x36 -> { mnemonic = ROL; addressing = ZeropageX; bytes = 2; cycles = 6 }
  | 0x76 -> { mnemonic = ROR; addressing = ZeropageX; bytes = 2; cycles = 6 }
  (* ZeropageX Write instructions*)
  | 0x95 -> { mnemonic = STA; addressing = ZeropageX; bytes = 2; cycles = 4 }
  | 0x94 -> { mnemonic = STY; addressing = ZeropageX; bytes = 2; cycles = 4 }
  (* ZeropageY Read *)
  | 0xB6 -> { mnemonic = LDX; addressing = ZeropageY; bytes = 2; cycles = 4 }
  (* ZeropageY Write instructions*)
  | 0x96 -> { mnemonic = STX; addressing = ZeropageY; bytes = 2; cycles = 4 }
  (* AbsoluteX*)
  (* AbsoluteX Read*)
  | 0x7D -> { mnemonic = ADC; addressing = AbsoluteX; bytes = 3; cycles = 4 }
  | 0x3D -> { mnemonic = AND; addressing = AbsoluteX; bytes = 3; cycles = 4 }
  | 0xDD -> { mnemonic = CMP; addressing = AbsoluteX; bytes = 3; cycles = 4 }
  | 0x5D -> { mnemonic = EOR; addressing = AbsoluteX; bytes = 3; cycles = 4 }
  | 0xBD -> { mnemonic = LDA; addressing = AbsoluteX; bytes = 3; cycles = 4 }
  | 0xBC -> { mnemonic = LDY; addressing = AbsoluteX; bytes = 3; cycles = 4 }
  | 0x1D -> { mnemonic = ORA; addressing = AbsoluteX; bytes = 3; cycles = 4 }
  | 0xFD -> { mnemonic = SBC; addressing = AbsoluteX; bytes = 3; cycles = 4 }
  (* AbsoluteX Read-Modify-Write instructions *)
  | 0x1E -> { mnemonic = ASL; addressing = AbsoluteX; bytes = 3; cycles = 7 }
  | 0xDE -> { mnemonic = DEC; addressing = AbsoluteX; bytes = 3; cycles = 7 }
  | 0xFE -> { mnemonic = INC; addressing = AbsoluteX; bytes = 3; cycles = 7 }
  | 0x5E -> { mnemonic = LSR; addressing = AbsoluteX; bytes = 3; cycles = 7 }
  | 0x3E -> { mnemonic = ROL; addressing = AbsoluteX; bytes = 3; cycles = 7 }
  | 0x7E -> { mnemonic = ROR; addressing = AbsoluteX; bytes = 3; cycles = 7 }
  (* AbsoluteX Write instructions*)
  | 0x9D -> { mnemonic = STA; addressing = AbsoluteX; bytes = 3; cycles = 5 }
  (* AbsoluteY*)
  (* AbsoluteY Read*)
  | 0x79 -> { mnemonic = ADC; addressing = AbsoluteY; bytes = 3; cycles = 4 }
  | 0x39 -> { mnemonic = AND; addressing = AbsoluteY; bytes = 3; cycles = 4 }
  | 0xD9 -> { mnemonic = CMP; addressing = AbsoluteY; bytes = 3; cycles = 4 }
  | 0x59 -> { mnemonic = EOR; addressing = AbsoluteY; bytes = 3; cycles = 4 }
  | 0xB9 -> { mnemonic = LDA; addressing = AbsoluteY; bytes = 3; cycles = 4 }
  | 0xBE -> { mnemonic = LDX; addressing = AbsoluteY; bytes = 3; cycles = 4 }
  | 0x19 -> { mnemonic = ORA; addressing = AbsoluteY; bytes = 3; cycles = 4 }
  | 0xF9 -> { mnemonic = SBC; addressing = AbsoluteY; bytes = 3; cycles = 4 }
  (* AbsoluteY Read-Modify-Write instructions *)
  (* AbsoluteY Write instructions*)
  | 0x99 -> { mnemonic = STA; addressing = AbsoluteY; bytes = 3; cycles = 5 }
  (* Relative *)
  | 0x90 -> { mnemonic = BCC; addressing = Relative; bytes = 2; cycles = 2 }
  | 0xB0 -> { mnemonic = BCS; addressing = Relative; bytes = 2; cycles = 2 }
  | 0xF0 -> { mnemonic = BEQ; addressing = Relative; bytes = 2; cycles = 2 }
  | 0x30 -> { mnemonic = BMI; addressing = Relative; bytes = 2; cycles = 2 }
  | 0xD0 -> { mnemonic = BNE; addressing = Relative; bytes = 2; cycles = 2 }
  | 0x10 -> { mnemonic = BPL; addressing = Relative; bytes = 2; cycles = 2 }
  | 0x50 -> { mnemonic = BVC; addressing = Relative; bytes = 2; cycles = 2 }
  | 0x70 -> { mnemonic = BVS; addressing = Relative; bytes = 2; cycles = 2 }
  (* PreIndexIndirect*)
  (* PreIndexIndirect Read*)
  | 0x61 -> { mnemonic = ADC; addressing = PreIndexIndirect; bytes = 2; cycles = 6 }
  | 0x21 -> { mnemonic = AND; addressing = PreIndexIndirect; bytes = 2; cycles = 6 }
  | 0xC1 -> { mnemonic = CMP; addressing = PreIndexIndirect; bytes = 2; cycles = 6 }
  | 0x41 -> { mnemonic = EOR; addressing = PreIndexIndirect; bytes = 2; cycles = 6 }
  | 0xA1 -> { mnemonic = LDA; addressing = PreIndexIndirect; bytes = 2; cycles = 6 }
  | 0x01 -> { mnemonic = ORA; addressing = PreIndexIndirect; bytes = 2; cycles = 6 }
  | 0xE1 -> { mnemonic = SBC; addressing = PreIndexIndirect; bytes = 2; cycles = 6 }
  (* PreIndexIndirect Write instructions*)
  | 0x81 -> { mnemonic = STA; addressing = PreIndexIndirect; bytes = 2; cycles = 4 }
  (* PostIndexIndirect*)
  (* PostIndexIndirect Read*)
  | 0x71 -> { mnemonic = ADC; addressing = PostIndexIndirect; bytes = 2; cycles = 5 }
  | 0x31 -> { mnemonic = AND; addressing = PostIndexIndirect; bytes = 2; cycles = 5 }
  | 0xD1 -> { mnemonic = CMP; addressing = PostIndexIndirect; bytes = 2; cycles = 5 }
  | 0x51 -> { mnemonic = EOR; addressing = PostIndexIndirect; bytes = 2; cycles = 5 }
  | 0xB1 -> { mnemonic = LDA; addressing = PostIndexIndirect; bytes = 2; cycles = 5 }
  | 0x11 -> { mnemonic = ORA; addressing = PostIndexIndirect; bytes = 2; cycles = 5 }
  | 0xF1 -> { mnemonic = SBC; addressing = PostIndexIndirect; bytes = 2; cycles = 5 }
  (* AbsoluteY Read-Modify-Write instructions *)
  (* AbsoluteY Write instructions*)
  | 0x91 -> { mnemonic = STA; addressing = PostIndexIndirect; bytes = 2; cycles = 5 }
  (* Indirect instructions*)
  | 0x6C -> { mnemonic = JMP; addressing = Indirect; bytes = 3; cycles = 5 }
  | _ -> failwith (Printf.sprintf "Opcode %02X Unimplemented" opcode)
;;

(* TODO: Use binary arithmetic *)
let set_nz v sr =
  let sr = if v = 0 then sr lor 0b0000_0010 else sr land 0b1111_1101 in
  if v land 0b1000_0000 = 0b1000_0000 then sr lor 0b1000_0000 else sr land 0b0111_1111
;;

let inst_adc x y sr =
  (* TODO : Handle decimal mode *)
  let r, sr =
    if sr land 0b0000_1000 = 0
    then (
      let r = x + y + (sr land 0b0000_0001) in
      (* overflow if signe of a and m are the same but sum  result is deifferent sign *)
      let overflow =
        if lnot (x lxor y) land (r lxor x land 0x80) = 0 then 0 else 0b0100_0000
      in
      let sr1 =
        set_nz (0xFF land r) sr
        land 0b1111_1110
        lor ((0x100 land r) lsr 8)
        land 0b1011_1111
        lor overflow
      in
      r land 0xFF, sr1)
    else (
      let highx, lowx, highy, lowy =
        (x land 0xF0) lsr 4, x land 0x0F, (y land 0xF0) lsr 4, y land 0x0F
      in
      let tmplowr = lowx + lowy + (sr land 0b0000_0001) in
      let lowr, lowc = if tmplowr <= 9 then tmplowr, 0 else tmplowr - 10, 1 in
      let tmphighr = highx + highy + lowc in
      let highr, carry = if tmphighr <= 9 then tmphighr, 0 else tmphighr - 10, 1 in
      let r = (highr lsl 4) lor lowr in
      let sr1 = set_nz r (sr land 0b1111_1110 lor carry) in
      r, sr1)
  in
  r, sr
;;

let inst_sbc x y sr =
  let r, sr =
    if sr land 0b0000_1000 = 0
    then (
      let r, sr1 = inst_adc x (lnot y land 0xFF) sr in
      r, sr1)
    else (
      let highx, lowx, highy, lowy =
        (x land 0xF0) lsr 4, x land 0x0F, (y land 0xF0) lsr 4, y land 0x0F
      in
      let tmplowr = lowx - lowy - (0b0000_00001 land lnot (sr land 0b0000_0001)) in
      let lowr, lowc = if tmplowr >= 0 then tmplowr, 0 else tmplowr + 10, 1 in
      let tmphighr = highx - highy - lowc in
      let highr, carry = if tmphighr >= 0 then tmphighr, 1 else tmphighr + 10, 0 in
      let r = (highr lsl 4) lor lowr in
      let sr1 = set_nz r (sr land 0b1111_1110 lor carry) in
      r, sr1)
  in
  r, sr
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
       let sync = true in
       let rw = true in
       let address = cpu.pc in
       (match inst.mnemonic with
        | NOP -> 0, cpu, { pins with rw; sync }, { bus with address }
        | BRK ->
          ( next_cycle
          , { cpu with pc = cpu.pc + 1; sp = (cpu.sp - 1) land 0xFF }
          , { pins with rw = false; sync = false }
          , { address = 0x0100 lor cpu.sp; data = (cpu.pc land 0xFF00) lsr 8 } )
        | CLC ->
          ( 0
          , { cpu with sr = cpu.sr land 0b1111_1110 }
          , { pins with rw; sync }
          , { bus with address } )
        | CLD ->
          ( 0
          , { cpu with sr = cpu.sr land 0b1111_0111 }
          , { pins with rw; sync }
          , { bus with address } )
        | CLI ->
          ( 0
          , { cpu with sr = cpu.sr land 0b1111_1011 }
          , { pins with rw; sync }
          , { bus with address } )
        | CLV ->
          ( 0
          , { cpu with sr = cpu.sr land 0b1011_1111 }
          , { pins with rw; sync }
          , { bus with address } )
        | DEX ->
          let v = 0xFF land (cpu.x - 1) in
          let sr = set_nz v cpu.sr in
          0, { cpu with x = v; sr }, { pins with rw; sync }, { bus with address }
        | DEY ->
          let v = 0xFF land (cpu.y - 1) in
          let sr = set_nz v cpu.sr in
          0, { cpu with y = v; sr }, { pins with rw; sync }, { bus with address }
        | INX ->
          let v = 0xFF land (cpu.x + 1) in
          let sr = set_nz v cpu.sr in
          0, { cpu with x = v; sr }, { pins with rw; sync }, { bus with address }
        | INY ->
          let v = 0xFF land (cpu.y + 1) in
          let sr = set_nz v cpu.sr in
          0, { cpu with y = v; sr }, { pins with rw; sync }, { bus with address }
        | PHA ->
          ( next_cycle
          , cpu
          , { pins with rw = false; sync = false }
          , { address = 0x0100 lor cpu.sp; data = cpu.a } )
        | PHP ->
          ( next_cycle
          , cpu
          , { pins with rw = false; sync = false }
          , { address = 0x0100 lor cpu.sp; data = cpu.sr lor 0b0011_0000 } )
        | PLA ->
          ( next_cycle
          , cpu
          , { pins with rw = true; sync = false }
          , { bus with address = 0x0100 lor cpu.sp } )
        | PLP ->
          ( next_cycle
          , cpu
          , { pins with rw = true; sync = false }
          , { bus with address = 0x0100 lor cpu.sp } )
        | RTI ->
          ( next_cycle
          , cpu
          , { pins with rw = false; sync = false }
          , { bus with address = 0x0100 lor cpu.sp } )
        | RTS ->
          ( next_cycle
          , cpu
          , { pins with rw = false; sync = false }
          , { bus with address = 0x0100 lor cpu.sp } )
        | JSR ->
          ( next_cycle
          , { cpu with adl = bus.data; pc = cpu.pc + 1 }
          , { pins with rw = false; sync = false }
          , { bus with address = 0x0100 lor cpu.sp } )
        | SEC ->
          ( 0
          , { cpu with sr = cpu.sr lor 0b0000_0001 }
          , { pins with rw; sync }
          , { bus with address } )
        | SED ->
          ( 0
          , { cpu with sr = cpu.sr lor 0b0000_1000 }
          , { pins with rw; sync }
          , { bus with address } )
        | SEI ->
          ( 0
          , { cpu with sr = cpu.sr lor 0b0000_0100 }
          , { pins with rw; sync }
          , { bus with address } )
        | TAX ->
          let a = cpu.a in
          let sr = set_nz a cpu.sr in
          0, { cpu with x = a; sr }, { pins with rw; sync }, { bus with address }
        | TAY ->
          let a = cpu.a in
          let sr = set_nz a cpu.sr in
          0, { cpu with y = a; sr }, { pins with rw; sync }, { bus with address }
        | TSX ->
          let s = cpu.sp in
          let sr = set_nz s cpu.sr in
          0, { cpu with x = s; sr }, { pins with rw; sync }, { bus with address }
        | TXA ->
          let x = cpu.x in
          let sr = set_nz x cpu.sr in
          0, { cpu with a = x; sr }, { pins with rw; sync }, { bus with address }
        | TXS ->
          let x = cpu.x in
          0, { cpu with sp = x }, { pins with rw; sync }, { bus with address }
        | TYA ->
          let y = cpu.y in
          let sr = set_nz y cpu.sr in
          0, { cpu with a = y; sr }, { pins with rw; sync }, { bus with address }
        | _ -> failwith "Invalid Implied opcode")
     | 2 ->
       (match inst.mnemonic with
        | BRK ->
          ( next_cycle
          , { cpu with sp = (cpu.sp - 1) land 0xFF }
          , { pins with rw = false; sync = false }
          , { address = 0x0100 lor cpu.sp; data = cpu.pc land 0xFF } )
        | PHA ->
          ( 0
          , { cpu with sp = (cpu.sp - 1) land 0xFF }
          , { pins with rw = true; sync = true }
          , { bus with address = cpu.pc } )
        | PLA ->
          ( next_cycle
          , { cpu with sp = (cpu.sp + 1) land 0xFF }
          , { pins with rw = true; sync = false }
          , { bus with address = 0x100 lor ((cpu.sp + 1) land 0xFF) } )
        | PLP ->
          ( next_cycle
          , { cpu with sp = (cpu.sp + 1) land 0xFF }
          , { pins with rw = true; sync = false }
          , { bus with address = 0x100 lor ((cpu.sp + 1) land 0xFF) } )
        | PHP ->
          ( 0
          , { cpu with sp = (cpu.sp - 1) land 0xFF }
          , { pins with rw = true; sync = true }
          , { bus with address = cpu.pc } )
        | RTI ->
          ( next_cycle
          , { cpu with sp = (cpu.sp + 1) land 0xFF }
          , { pins with rw = true; sync = false }
          , { bus with address = 0x0100 lor ((cpu.sp + 1) land 0xFF) } )
        | RTS ->
          ( next_cycle
          , { cpu with sp = (cpu.sp + 1) land 0xFF }
          , { pins with rw = true; sync = false }
          , { bus with address = 0x0100 lor ((cpu.sp + 1) land 0xFF) } )
        | JSR ->
          ( next_cycle
          , cpu
          , { pins with rw = false; sync = false }
          , { bus with data = (cpu.pc land 0xFF00) lsr 8 } )
        | _ -> failwith "not iplemented")
     | 3 ->
       (match inst.mnemonic with
        | BRK ->
          ( next_cycle
          , { cpu with sp = (cpu.sp - 1) land 0xFF; sr = cpu.sr lor 0b0011_0100 }
          , { pins with rw = false; sync = false }
          , { address = 0x0100 lor cpu.sp; data = cpu.sr lor 0b0011_0000 } )
        | PLA ->
          let a = bus.data in
          let sr = set_nz a cpu.sr in
          ( 0
          , { cpu with a; sr }
          , { pins with rw = true; sync = true }
          , { bus with address = cpu.pc } )
        | PLP ->
          ( 0
          , { cpu with sr = bus.data land 0b1100_1111 }
          , { pins with rw = true; sync = true }
          , { bus with address = cpu.pc } )
        | RTI ->
          ( next_cycle
          , { cpu with sr = bus.data; sp = (cpu.sp + 1) land 0xFF }
          , { pins with rw = true; sync = false }
          , { bus with address = 0x0100 lor ((cpu.sp + 1) land 0xFF) } )
        | RTS ->
          ( next_cycle
          , { cpu with pc = cpu.pc land 0xFF00 lor bus.data; sp = (cpu.sp + 1) land 0xFF }
          , { pins with rw = true; sync = false }
          , { bus with address = 0x0100 lor ((cpu.sp + 1) land 0xFF) } )
        | JSR ->
          ( next_cycle
          , { cpu with sp = (cpu.sp - 1) land 0xFF }
          , { pins with rw = false; sync = false }
          , { address = 0x0100 lor ((cpu.sp - 1) land 0xFF); data = cpu.pc land 0xFF } )
        | _ -> failwith "not iplemented")
     | 4 ->
       (match inst.mnemonic with
        | BRK ->
          ( next_cycle
          , cpu
          , { pins with rw = true; sync = false }
          , { bus with address = 0xFFFE } )
        | RTI ->
          ( next_cycle
          , { cpu with pc = cpu.pc land 0xFF00 lor bus.data; sp = (cpu.sp + 1) land 0xFF }
          , { pins with rw = true; sync = false }
          , { bus with address = 0x0100 lor ((cpu.sp + 1) land 0xFF) } )
        | RTS ->
          let pc = cpu.pc land 0xFF lor (bus.data lsl 8) in
          ( next_cycle
          , { cpu with pc }
          , { pins with rw = true; sync = false }
          , { bus with address = pc } )
        | JSR ->
          ( next_cycle
          , { cpu with sp = (cpu.sp - 1) land 0xFF }
          , { pins with rw = true; sync = false }
          , { bus with address = cpu.pc } )
        | _ -> failwith "not iplemented")
     | 5 ->
       (match inst.mnemonic with
        | BRK ->
          ( next_cycle
          , { cpu with pc = bus.data }
          , { pins with rw = true; sync = false }
          , { bus with address = 0xFFFF } )
        | RTI ->
          let pc = cpu.pc land 0xFF lor (bus.data lsl 8) in
          ( 0
          , { cpu with pc }
          , { pins with rw = true; sync = true }
          , { bus with address = pc } )
        | RTS ->
          let pc = cpu.pc + 1 in
          ( 0
          , { cpu with pc }
          , { pins with rw = true; sync = true }
          , { bus with address = pc } )
        | JSR ->
          let address = cpu.adl lor (bus.data lsl 8) in
          ( 0
          , { cpu with pc = address }
          , { pins with rw = true; sync = true }
          , { bus with address } )
        | _ -> failwith "not iplemented")
     | 6 ->
       (match inst.mnemonic with
        | BRK ->
          let pc = (bus.data lsl 8) lor cpu.pc in
          ( 0
          , { cpu with pc }
          , { pins with rw = true; sync = true }
          , { bus with address = pc } )
        | _ -> failwith "not iplemented")
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
       (match inst.mnemonic with
        | STA ->
          ( next_cycle
          , { cpu with pc = cpu.pc + 1 }
          , { pins with rw = false }
          , { address = bus.data; data = cpu.a } )
        | STX ->
          ( next_cycle
          , { cpu with pc = cpu.pc + 1 }
          , { pins with rw = false }
          , { address = bus.data; data = cpu.x } )
        | STY ->
          ( next_cycle
          , { cpu with pc = cpu.pc + 1 }
          , { pins with rw = false }
          , { address = bus.data; data = cpu.y } )
        | _ ->
          ( next_cycle
          , { cpu with pc = cpu.pc + 1 }
          , { pins with rw = true }
          , { bus with address = bus.data } ))
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
        | ASL | DEC | INC | LSR | ROL | ROR ->
          next_cycle, { cpu with tmp_data = bus.data }, { pins with rw = false }, bus
        | STA | STX | STY ->
          0, cpu, { pins with rw = true; sync = true }, { bus with address = cpu.pc }
        | _ -> failwith "Invalid Zeropage opcode")
     | 3 ->
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
     | 4 ->
       (match inst.mnemonic with
        | ASL | DEC | INC | LSR | ROL | ROR ->
          0, cpu, { pins with rw = true; sync = true }, { bus with address = cpu.pc }
        | _ -> failwith "Cycle 5 Absolute not implemented")
     | _ -> failwith "op cycle too large")
  (* AbxoluteX*)
  | AbsoluteX ->
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
       ( next_cycle
       , { cpu with
           adh = bus.data + (((cpu.adl + cpu.x) land 0x100) lsr 8)
         ; adl = (cpu.adl + cpu.x) land 0xFF
         ; pc = cpu.pc + 1
         }
       , { pins with rw = true }
       , { bus with address = (bus.data lsl 8) + ((cpu.adl + cpu.x) land 0xFF) } )
     | 3 ->
       (* check if we crossed page boundary *)
       (* In that case the MSB of address does not match the real MSB of address*)
       (* Only for read instruction *)
       let sync =
         match inst.mnemonic with
         | STA | ASL | DEC | INC | LSR | ROL | ROR -> false
         | _ -> if (bus.address land 0xFF00) lsr 8 = cpu.adh then true else false
       in
       let address = if sync then cpu.pc else (cpu.adh lsl 8) + cpu.adl in
       let next_cycle = if sync then 0 else next_cycle in
       (match inst.mnemonic with
        (* Read instructions *)
        | ADC ->
          let a, sr = inst_adc cpu.a bus.data cpu.sr in
          ( next_cycle
          , (if sync then { cpu with a; sr } else cpu)
          , { pins with rw = true; sync }
          , { bus with address } )
        | AND ->
          let a, sr = inst_and cpu.a bus.data cpu.sr in
          ( next_cycle
          , (if sync then { cpu with a; sr } else cpu)
          , { pins with rw = true; sync }
          , { bus with address } )
        | CMP ->
          let sr = inst_cmp cpu.a bus.data cpu.sr in
          ( next_cycle
          , (if sync then { cpu with sr } else cpu)
          , { pins with rw = true; sync }
          , { bus with address } )
        | EOR ->
          let a, sr = inst_eor cpu.a bus.data cpu.sr in
          ( next_cycle
          , (if sync then { cpu with a; sr } else cpu)
          , { pins with rw = true; sync }
          , { bus with address } )
        | LDA ->
          let a = bus.data in
          let sr = set_nz a cpu.sr in
          ( next_cycle
          , (if sync then { cpu with a; sr } else cpu)
          , { pins with rw = true; sync }
          , { bus with address } )
        | LDY ->
          let y = bus.data in
          let sr = set_nz y cpu.sr in
          ( next_cycle
          , (if sync then { cpu with y; sr } else cpu)
          , { pins with rw = true; sync }
          , { bus with address } )
        | ORA ->
          let a, sr = inst_ora cpu.a bus.data cpu.sr in
          ( next_cycle
          , (if sync then { cpu with a; sr } else cpu)
          , { pins with rw = true; sync }
          , { bus with address } )
        | SBC ->
          let a, sr = inst_sbc cpu.a bus.data cpu.sr in
          ( next_cycle
          , (if sync then { cpu with a; sr } else cpu)
          , { pins with rw = true; sync }
          , { bus with address } )
        (* TODO Check if correcr*)
        | STA ->
          next_cycle, cpu, { pins with rw = false; sync }, { address; data = cpu.a }
        | ASL | DEC | INC | LSR | ROL | ROR ->
          ( next_cycle
          , { cpu with tmp_data = bus.data }
          , { pins with rw = true }
          , { bus with address } )
        | _ -> failwith "Cycle 4 AbsoluteX Not implemented")
     | 4 ->
       let sync =
         match inst.mnemonic with
         | ASL | DEC | INC | LSR | ROL | ROR -> false
         | _ -> true
       in
       let address =
         match inst.mnemonic with
         | ASL | DEC | INC | LSR | ROL | ROR -> (cpu.adh lsl 8) + cpu.adl
         | _ -> cpu.pc
       in
       let rw =
         match inst.mnemonic with
         | ASL | DEC | INC | LSR | ROL | ROR -> false
         | _ -> true
       in
       let next_cycle = if sync then 0 else next_cycle in
       (match inst.mnemonic with
        (* Read instructions *)
        | ADC ->
          let a, sr = inst_adc cpu.a bus.data cpu.sr in
          next_cycle, { cpu with a; sr }, { pins with rw; sync }, { bus with address }
        | AND ->
          let a, sr = inst_and cpu.a bus.data cpu.sr in
          next_cycle, { cpu with a; sr }, { pins with rw; sync }, { bus with address }
        | CMP ->
          let sr = inst_cmp cpu.a bus.data cpu.sr in
          next_cycle, { cpu with sr }, { pins with rw; sync }, { bus with address }
        | EOR ->
          let a, sr = inst_eor cpu.a bus.data cpu.sr in
          next_cycle, { cpu with a; sr }, { pins with rw; sync }, { bus with address }
        | LDA ->
          let a = bus.data in
          let sr = set_nz a cpu.sr in
          next_cycle, { cpu with a; sr }, { pins with rw; sync }, { bus with address }
        | LDY ->
          let y = bus.data in
          let sr = set_nz y cpu.sr in
          next_cycle, { cpu with y; sr }, { pins with rw; sync }, { bus with address }
        | ORA ->
          let a, sr = inst_ora cpu.a bus.data cpu.sr in
          next_cycle, { cpu with a; sr }, { pins with rw; sync }, { bus with address }
        | SBC ->
          let a, sr = inst_sbc cpu.a bus.data cpu.sr in
          next_cycle, { cpu with a; sr }, { pins with rw; sync }, { bus with address }
        (* TODO Check if correcr*)
        | STA -> next_cycle, cpu, { pins with rw; sync }, { address; data = cpu.a }
        | ASL | DEC | INC | LSR | ROL | ROR ->
          ( next_cycle
          , { cpu with tmp_data = bus.data }
          , { pins with rw; sync }
          , { bus with address } )
        | _ -> failwith "Cycle 4 AbsoluteX Not implemented")
     | 5 ->
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
        | _ -> failwith "Cycle 5 AbsoluteX not implemented")
     | 6 ->
       (match inst.mnemonic with
        | ASL | DEC | INC | LSR | ROL | ROR ->
          0, cpu, { pins with rw = true; sync = true }, { bus with address = cpu.pc }
        | _ -> failwith "Cycle 6 Absolute not implemented")
     | _ -> failwith "op cycle too large")
  (* AbsoluteY*)
  | AbsoluteY ->
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
       ( next_cycle
       , { cpu with
           adh = bus.data + (((cpu.adl + cpu.y) land 0x100) lsr 8)
         ; adl = (cpu.adl + cpu.y) land 0xFF
         ; pc = cpu.pc + 1
         }
       , { pins with rw = true }
       , { bus with address = (bus.data lsl 8) + ((cpu.adl + cpu.y) land 0xFF) } )
     | 3 ->
       (* check if we crossed page boundary *)
       (* In that case the MSB of address does not match the real MSB of address*)
       (* Only for read instruction *)
       let sync =
         match inst.mnemonic with
         | STA -> false
         | _ -> if (bus.address land 0xFF00) lsr 8 = cpu.adh then true else false
       in
       let address = if sync then cpu.pc else (cpu.adh lsl 8) + cpu.adl in
       let next_cycle = if sync then 0 else next_cycle in
       (match inst.mnemonic with
        (* Read instructions *)
        | ADC ->
          let a, sr = inst_adc cpu.a bus.data cpu.sr in
          ( next_cycle
          , (if sync then { cpu with a; sr } else cpu)
          , { pins with rw = true; sync }
          , { bus with address } )
        | AND ->
          let a, sr = inst_and cpu.a bus.data cpu.sr in
          ( next_cycle
          , (if sync then { cpu with a; sr } else cpu)
          , { pins with rw = true; sync }
          , { bus with address } )
        | CMP ->
          let sr = inst_cmp cpu.a bus.data cpu.sr in
          ( next_cycle
          , (if sync then { cpu with sr } else cpu)
          , { pins with rw = true; sync }
          , { bus with address } )
        | EOR ->
          let a, sr = inst_eor cpu.a bus.data cpu.sr in
          ( next_cycle
          , (if sync then { cpu with a; sr } else cpu)
          , { pins with rw = true; sync }
          , { bus with address } )
        | LDA ->
          let a = bus.data in
          let sr = set_nz a cpu.sr in
          ( next_cycle
          , (if sync then { cpu with a; sr } else cpu)
          , { pins with rw = true; sync }
          , { bus with address } )
        | LDX ->
          let x = bus.data in
          let sr = set_nz x cpu.sr in
          ( next_cycle
          , (if sync then { cpu with x; sr } else cpu)
          , { pins with rw = true; sync }
          , { bus with address } )
        | ORA ->
          let a, sr = inst_ora cpu.a bus.data cpu.sr in
          ( next_cycle
          , (if sync then { cpu with a; sr } else cpu)
          , { pins with rw = true; sync }
          , { bus with address } )
        | SBC ->
          let a, sr = inst_sbc cpu.a bus.data cpu.sr in
          ( next_cycle
          , (if sync then { cpu with a; sr } else cpu)
          , { pins with rw = true; sync }
          , { bus with address } )
        (* TODO Check if correcr*)
        | STA ->
          ( next_cycle
          , cpu
          , { pins with rw = false; sync = false }
          , { address; data = cpu.a } )
        | _ -> failwith "Cycle 4 AbsoluteX Not implemented")
     | 4 ->
       let sync = true in
       let address = cpu.pc in
       let rw = true in
       let next_cycle = 0 in
       (match inst.mnemonic with
        (* Read instructions *)
        | ADC ->
          let a, sr = inst_adc cpu.a bus.data cpu.sr in
          next_cycle, { cpu with a; sr }, { pins with rw; sync }, { bus with address }
        | AND ->
          let a, sr = inst_and cpu.a bus.data cpu.sr in
          next_cycle, { cpu with a; sr }, { pins with rw; sync }, { bus with address }
        | CMP ->
          let sr = inst_cmp cpu.a bus.data cpu.sr in
          next_cycle, { cpu with sr }, { pins with rw; sync }, { bus with address }
        | EOR ->
          let a, sr = inst_eor cpu.a bus.data cpu.sr in
          next_cycle, { cpu with a; sr }, { pins with rw; sync }, { bus with address }
        | LDA ->
          let a = bus.data in
          let sr = set_nz a cpu.sr in
          next_cycle, { cpu with a; sr }, { pins with rw; sync }, { bus with address }
        | LDX ->
          let x = bus.data in
          let sr = set_nz x cpu.sr in
          next_cycle, { cpu with x; sr }, { pins with rw; sync }, { bus with address }
        | ORA ->
          let a, sr = inst_ora cpu.a bus.data cpu.sr in
          next_cycle, { cpu with a; sr }, { pins with rw; sync }, { bus with address }
        | SBC ->
          let a, sr = inst_sbc cpu.a bus.data cpu.sr in
          next_cycle, { cpu with a; sr }, { pins with rw; sync }, { bus with address }
        (* TODO Check if correcr*)
        | STA -> next_cycle, cpu, { pins with rw; sync }, { bus with address }
        | _ -> failwith "Cycle 4 AbsoluteX Not implemented")
     | _ -> failwith "op cycle too large")
  (* Relative*)
  | Relative ->
    let next_cycle = cycle + 1 in
    let rw = true in
    (match cycle with
     | 0 ->
       ( next_cycle
       , { cpu with pc = cpu.pc + 1 }
       , { pins with rw }
       , { bus with address = cpu.pc + 1 } )
     | 1 ->
       (* TODO: checp pc logic*)
       let pc = cpu.pc + 1 in
       ( next_cycle
       , { cpu with pc; tmp_data = bus.data }
       , { pins with rw; sync = false }
       , { bus with address = pc } )
     | 2 ->
       let branch_taken =
         match inst.mnemonic with
         | BCC -> not (cpu.sr land 0b0000_0001 = 0b0000_0001)
         | BCS -> cpu.sr land 0b0000_0001 = 0b0000_0001
         | BEQ -> cpu.sr land 0b0000_0010 = 0b0000_0010
         | BNE -> not (cpu.sr land 0b0000_0010 = 0b0000_0010)
         | BMI -> cpu.sr land 0b1000_0000 = 0b1000_0000
         | BPL -> not (cpu.sr land 0b1000_0000 = 0b1000_0000)
         | BVC -> not (cpu.sr land 0b0100_0000 = 0b0100_0000)
         | BVS -> cpu.sr land 0b0100_0000 = 0b0100_0000
         | _ -> failwith "Not implemented"
       in
       let offset =
         if branch_taken
         then
           if cpu.tmp_data land 0b1000_0000 = 0b1000_0000
           then cpu.tmp_data - 256 - 1
           else cpu.tmp_data - 1
         else 0
       in
       let pc = if branch_taken then cpu.pc else cpu.pc in
       let next_cycle = if branch_taken then next_cycle else 0 in
       let sync = if branch_taken then false else true in
       ( next_cycle
       , { cpu with pc; adh = (pc land 0xFF00) lsr 8; adl = (pc land 0xFF) + offset }
       , { pins with rw; sync }
       , { bus with address = pc } )
     | 3 ->
       let rw = true in
       let page_crossed = cpu.adl < 0 || cpu.adl > 255 in
       let sync = not page_crossed in
       let next_cycle = if page_crossed then next_cycle else 0 in
       let pc =
         if page_crossed
         then
           if cpu.adl < 0
           then ((cpu.adh - 1) lsl 8) lor (256 + cpu.adl)
           else ((cpu.adh + 1) lsl 8) lor (cpu.adl - 256)
         else ((cpu.adh lsl 8) lor cpu.adl) + 1
       in
       let address = pc in
       next_cycle, { cpu with pc }, { pins with rw; sync }, { bus with address }
     | 4 ->
       let rw = true in
       let sync = true in
       ( 0
       , { cpu with pc = cpu.pc + 1 }
       , { pins with rw; sync }
       , { bus with address = cpu.pc + 1 } )
     | _ -> failwith "op cycle too large")
  (* PreIndexIndirect*)
  | PreIndexIndirect ->
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
       ( next_cycle
       , cpu
       , { pins with rw = true }
       , { bus with address = (bus.address + cpu.x) land 0xFF } )
     | 3 ->
       ( next_cycle
       , { cpu with adl = bus.data }
       , { pins with rw = true }
       , { bus with address = (bus.address + 1) land 0xFF } )
     | 4 ->
       let address = (bus.data lsl 8) lor cpu.adl in
       (match inst.mnemonic with
        | ADC | AND | CMP | EOR | LDA | ORA | SBC ->
          ( next_cycle
          , { cpu with adh = bus.data }
          , { pins with rw = true }
          , { bus with address } )
        | STA ->
          ( next_cycle
          , { cpu with adh = bus.data }
          , { pins with rw = false }
          , { address; data = cpu.a } )
        | _ -> failwith "Invalid mnemonic")
     | 5 ->
       let next_cycle = 0 in
       let sync = true in
       let address = cpu.pc in
       (match inst.mnemonic with
        (* Read instructions *)
        | ADC ->
          let a, sr = inst_adc cpu.a bus.data cpu.sr in
          next_cycle, { cpu with a; sr }, { pins with sync }, { bus with address }
        | AND ->
          let a, sr = inst_and cpu.a bus.data cpu.sr in
          next_cycle, { cpu with a; sr }, { pins with sync }, { bus with address }
        | CMP ->
          let sr = inst_cmp cpu.a bus.data cpu.sr in
          next_cycle, { cpu with sr }, { pins with sync }, { bus with address }
        | EOR ->
          let a, sr = inst_eor cpu.a bus.data cpu.sr in
          next_cycle, { cpu with a; sr }, { pins with sync }, { bus with address }
        | LDA ->
          let a = bus.data in
          let sr = set_nz a cpu.sr in
          next_cycle, { cpu with a; sr }, { pins with sync }, { bus with address }
        | ORA ->
          let a, sr = inst_ora cpu.a bus.data cpu.sr in
          next_cycle, { cpu with a; sr }, { pins with sync }, { bus with address }
        | SBC ->
          let a, sr = inst_sbc cpu.a bus.data cpu.sr in
          next_cycle, { cpu with a; sr }, { pins with sync }, { bus with address }
        (* TODO Check if correcr*)
        | STA -> next_cycle, cpu, { pins with rw = true; sync }, { bus with address }
        | _ -> failwith "Cycle 4 AbsoluteX Not implemented")
     | _ -> failwith "op cycle too large")
  (* PostIndexIndirect *)
  | PostIndexIndirect ->
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
       ( next_cycle
       , { cpu with adl = bus.data }
       , { pins with rw = true }
       , { bus with address = (bus.address + 1) land 0xFF } )
     | 3 ->
       ( next_cycle
       , { cpu with adh = bus.data; adl = cpu.adl + cpu.y }
       , { pins with rw = true }
       , { bus with address = (bus.data lsl 8) lor ((cpu.adl + cpu.y) land 0xFF) } )
     | 4 ->
       (* check if we crossed page boundary *)
       (* In that case the MSB of address does not match the real MSB of address*)
       (* Only for read instruction *)
       let page_crossed = cpu.adl land 0x100 = 0x100 in
       let next_cycle, sync, address =
         match inst.mnemonic with
         | STA ->
           ( next_cycle
           , false
           , if page_crossed then (bus.address + 0x100) land 0xFFFF else bus.address )
         | _ ->
           if page_crossed
           then next_cycle, false, (bus.address + 0x100) land 0xFFFF
           else 0, true, cpu.pc
       in
       let rw = true in
       (match inst.mnemonic with
        (* Read instructions *)
        | ADC ->
          let a, sr = inst_adc cpu.a bus.data cpu.sr in
          ( next_cycle
          , (if page_crossed then cpu else { cpu with a; sr })
          , { pins with rw; sync }
          , { bus with address } )
        | AND ->
          let a, sr = inst_and cpu.a bus.data cpu.sr in
          ( next_cycle
          , (if page_crossed then cpu else { cpu with a; sr })
          , { pins with rw; sync }
          , { bus with address } )
        | CMP ->
          let sr = inst_cmp cpu.a bus.data cpu.sr in
          ( next_cycle
          , (if page_crossed then cpu else { cpu with sr })
          , { pins with rw; sync }
          , { bus with address } )
        | EOR ->
          let a, sr = inst_eor cpu.a bus.data cpu.sr in
          ( next_cycle
          , (if page_crossed then cpu else { cpu with a; sr })
          , { pins with rw; sync }
          , { bus with address } )
        | LDA ->
          let a = bus.data in
          let sr = set_nz a cpu.sr in
          ( next_cycle
          , (if page_crossed then cpu else { cpu with a; sr })
          , { pins with rw; sync }
          , { bus with address } )
        | LDX ->
          let x = bus.data in
          let sr = set_nz x cpu.sr in
          ( next_cycle
          , (if page_crossed then cpu else { cpu with x; sr })
          , { pins with rw; sync }
          , { bus with address } )
        | ORA ->
          let a, sr = inst_ora cpu.a bus.data cpu.sr in
          ( next_cycle
          , (if page_crossed then cpu else { cpu with a; sr })
          , { pins with rw; sync }
          , { bus with address } )
        | SBC ->
          let a, sr = inst_sbc cpu.a bus.data cpu.sr in
          ( next_cycle
          , (if page_crossed then cpu else { cpu with a; sr })
          , { pins with rw; sync }
          , { bus with address } )
        (* TODO Check if correcr*)
        | STA ->
          next_cycle, cpu, { pins with rw = false; sync }, { address; data = cpu.a }
        | _ -> failwith "Cycle 4 AbsoluteX Not implemented")
     | 5 ->
       let sync = true in
       let next_cycle = 0 in
       let rw = true in
       let address = cpu.pc in
       (match inst.mnemonic with
        (* Read instructions *)
        | ADC ->
          let a, sr = inst_adc cpu.a bus.data cpu.sr in
          next_cycle, { cpu with a; sr }, { pins with rw; sync }, { bus with address }
        | AND ->
          let a, sr = inst_and cpu.a bus.data cpu.sr in
          next_cycle, { cpu with a; sr }, { pins with rw; sync }, { bus with address }
        | CMP ->
          let sr = inst_cmp cpu.a bus.data cpu.sr in
          next_cycle, { cpu with sr }, { pins with rw; sync }, { bus with address }
        | EOR ->
          let a, sr = inst_eor cpu.a bus.data cpu.sr in
          next_cycle, { cpu with a; sr }, { pins with rw; sync }, { bus with address }
        | LDA ->
          let a = bus.data in
          let sr = set_nz a cpu.sr in
          next_cycle, { cpu with a; sr }, { pins with rw; sync }, { bus with address }
        | LDX ->
          let x = bus.data in
          let sr = set_nz x cpu.sr in
          next_cycle, { cpu with x; sr }, { pins with rw; sync }, { bus with address }
        | ORA ->
          let a, sr = inst_ora cpu.a bus.data cpu.sr in
          next_cycle, { cpu with a; sr }, { pins with rw; sync }, { bus with address }
        | SBC ->
          let a, sr = inst_sbc cpu.a bus.data cpu.sr in
          next_cycle, { cpu with a; sr }, { pins with rw; sync }, { bus with address }
        (* TODO Check if correcr*)
        | STA -> next_cycle, cpu, { pins with rw; sync }, { bus with address }
        | _ -> failwith "Cycle 5 PostIndexIndirect Not implemented")
     | _ -> failwith "op cycle too large")
  | Indirect ->
    (match inst.mnemonic with
     | JMP ->
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
          ( next_cycle
          , { cpu with adh = bus.data; pc = cpu.pc + 1 }
          , { pins with rw = true }
          , { bus with address = (bus.data lsl 8) + cpu.adl } )
        | 3 ->
          ( next_cycle
          , { cpu with adl = bus.data }
          , { pins with rw = true }
          , { bus with
              address =
                (bus.address land 0xFF00) + (((bus.address land 0xFF) + 1) land 0xFF)
            } )
        | 4 ->
          let pc = (bus.data lsl 8) + cpu.adl in
          ( 0
          , { cpu with pc }
          , { pins with rw = true; sync = true }
          , { bus with address = pc } )
        | _ -> failwith "op cycle too large")
     | _ -> failwith "Invalid Indirect instruction")
  | ZeropageX ->
    (* ZeropageX *)
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
        | STY ->
          ( next_cycle
          , cpu
          , { pins with rw = false }
          , { data = cpu.y; address = (bus.address + cpu.x) land 0xFF } )
        | STA ->
          ( next_cycle
          , cpu
          , { pins with rw = false }
          , { data = cpu.a; address = (bus.address + cpu.x) land 0xFF } )
        | _ ->
          ( next_cycle
          , cpu
          , { pins with rw = true }
          , { bus with address = (bus.address + cpu.x) land 0xFF } ))
     | 3 ->
       let sync =
         match inst.mnemonic with
         | ASL | INC | DEC | LSR | ROL | ROR -> false
         | _ -> true
       in
       let rw = true in
       let next_cycle, address = if sync then 0, cpu.pc else next_cycle, bus.address in
       (match inst.mnemonic with
        (* Read instructions *)
        | ADC ->
          let a, sr = inst_adc cpu.a bus.data cpu.sr in
          next_cycle, { cpu with a; sr }, { pins with rw; sync }, { bus with address }
        | AND ->
          let a, sr = inst_and cpu.a bus.data cpu.sr in
          next_cycle, { cpu with a; sr }, { pins with rw; sync }, { bus with address }
        | CMP ->
          let sr = inst_cmp cpu.a bus.data cpu.sr in
          next_cycle, { cpu with sr }, { pins with rw; sync }, { bus with address }
        | EOR ->
          let a, sr = inst_eor cpu.a bus.data cpu.sr in
          next_cycle, { cpu with a; sr }, { pins with rw; sync }, { bus with address }
        | LDA ->
          let a = bus.data in
          let sr = set_nz a cpu.sr in
          next_cycle, { cpu with a; sr }, { pins with rw; sync }, { bus with address }
        | LDY ->
          let y = bus.data in
          let sr = set_nz y cpu.sr in
          next_cycle, { cpu with y; sr }, { pins with rw; sync }, { bus with address }
        | ORA ->
          let a, sr = inst_ora cpu.a bus.data cpu.sr in
          next_cycle, { cpu with a; sr }, { pins with rw; sync }, { bus with address }
        | SBC ->
          let a, sr = inst_sbc cpu.a bus.data cpu.sr in
          next_cycle, { cpu with a; sr }, { pins with rw; sync }, { bus with address }
        (* TODO Check if correcr*)
        | STY | STA ->
          next_cycle, cpu, { pins with sync; rw }, { bus with address = cpu.pc }
        | ASL | DEC | INC | LSR | ROL | ROR ->
          ( next_cycle
          , { cpu with tmp_data = bus.data }
          , { pins with rw = false }
          , { address; data = bus.data } )
        | _ -> failwith "Cycle 3 ZeropageX Not implemented")
     | 4 ->
       let rw = false in
       (match inst.mnemonic with
        (* Read-modify-write instructions *)
        | ASL ->
          let r, sr = inst_asl cpu.tmp_data cpu.sr in
          next_cycle, { cpu with sr }, { pins with rw }, { bus with data = r }
        | DEC ->
          let r = 0xFF land (cpu.tmp_data - 1) in
          let sr = set_nz r cpu.sr in
          next_cycle, { cpu with sr }, { pins with rw }, { bus with data = r }
        | INC ->
          let r = 0xFF land (cpu.tmp_data + 1) in
          let sr = set_nz r cpu.sr in
          next_cycle, { cpu with sr }, { pins with rw }, { bus with data = r }
        | LSR ->
          let r, sr = inst_lsr cpu.tmp_data cpu.sr in
          next_cycle, { cpu with sr }, { pins with rw }, { bus with data = r }
        | ROL ->
          let r, sr = inst_rol cpu.tmp_data cpu.sr in
          next_cycle, { cpu with sr }, { pins with rw }, { bus with data = r }
        | ROR ->
          let r, sr = inst_ror cpu.tmp_data cpu.sr in
          next_cycle, { cpu with sr }, { pins with rw }, { bus with data = r }
        | _ -> failwith "Cycle 4 ZeroPageX Not implemented")
     | 5 ->
       let sync = true in
       let rw = true in
       (match inst.mnemonic with
        | ASL | DEC | INC | LSR | ROL | ROR ->
          0, cpu, { pins with rw; sync }, { bus with address = cpu.pc }
        | _ -> failwith "Cycle 5 AbsoluteX not implemented")
     | _ -> failwith "op cycle too large")
  | ZeropageY ->
    (* ZeropageY *)
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
        | STX ->
          ( next_cycle
          , cpu
          , { pins with rw = false }
          , { data = cpu.x; address = (bus.address + cpu.y) land 0xFF } )
        | LDX ->
          ( next_cycle
          , cpu
          , { pins with rw = true }
          , { bus with address = (bus.address + cpu.y) land 0xFF } )
        | _ -> failwith "ZeropageY Impossible")
     | 3 ->
       let sync = true in
       let rw = true in
       let next_cycle = 0 in
       (match inst.mnemonic with
        (* Read instructions *)
        | LDX ->
          let x = bus.data in
          let sr = set_nz x cpu.sr in
          ( next_cycle
          , { cpu with x; sr }
          , { pins with sync; rw }
          , { bus with address = cpu.pc } )
        (* TODO Check if correcr*)
        | STX -> next_cycle, cpu, { pins with sync; rw }, { bus with address = cpu.pc }
        | _ -> failwith "Cycle 3 ZeropageY Not implemented")
     | _ -> failwith "op cycle too large")
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
