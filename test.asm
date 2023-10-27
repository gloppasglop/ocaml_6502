* = $0600
  NOP
  NOP
  NOP ; 0xEA
  DEX ; 0xCA
  DEX ; 0xCA
  DEX ; 0xCA
  DEY ; 0x88
  DEY ; 0x88
  DEY ; 0x88
  NOP ; 0xEA
  INX ; 0xE8
  INX ; 0xE8
  INX ; 0xE8
  NOP ; 0xEA
  SEC ; 0x38
  SED ; 0xF8
  SEI ; 0x78
  INY ; 0xC8
  INY ; 0xC8
  INY ; 0xC8
  CLC ; 0x18
  CLD ; 0xD8
  CLI ; 0x58
  CLV ; 0xB8
  NOP ; 0xEA
  NOP ; 0xEA
  NOP ; 0xEA
  TAX ; 0xAA
  TAY ; 0xA8
  TXA ; 0x8A
  TSX ; 0xBA
  TSX ; 0xBA
  TXA ; 0x8A
  TAY ; 0xA8
  ASL A ; 0x0A
  ASL A ; 0x0A
  ASL A ; 0x0A
  ASL A ; 0x0A
  ASL A ; 0x0A
  ASL A ; 0x0A
  ASL A ; 0x0A
  ASL A ; 0x0A
  TAX ; 0xAA
  CLC ; 0x18
  DEX ; 0xCA
  TXA ; 0x8A
  LSR A; 0x4A
  LSR A; 0x4A
  LSR A; 0x4A
  LSR A; 0x4A
  LSR A; 0x4A
  LSR A; 0x4A
  LSR A; 0x4A
  LSR A; 0x4A
  LSR A; 0x4A
  TYA ; 0x98
  CLC ; 0x18
  ROL A; 0x2A
  ROL A; 0x2A
  ROL A; 0x2A
  ROL A; 0x2A
  ROL A; 0x2A
  ROL A; 0x2A
  ROL A; 0x2A
  ROL A; 0x2A
  ROL A; 0x2A
  ROL A; 0x2A
  ROL A; 0x2A
  ROL A; 0x2A
  ROL A; 0x2A
  ROL A; 0x2A
  ROL A; 0x2A
  ROR A; 0x6A
  ROR A; 0x6A
  ROR A; 0x6A
  ROR A; 0x6A
  ROR A; 0x6A
  ROR A; 0x6A
  ROR A; 0x6A
  ROR A; 0x6A
  ROR A; 0x6A
  ROR A; 0x6A
  ROR A; 0x6A
  ROR A; 0x6A
  ROR A; 0x6A
  ROR A; 0x6A
  ADC #0x10  ; 0x69 0x10
  ADC #$10   ; 0x69 0x10
  ADC #$80   ; 0x69 0x80
  ADC #$50   ; 0x69 0x50
  ADC #$10   ; 0x69 0x10
  ADC #$10   ; 0x69 0x10
  ADC #$2B   ; 0x69 0x2B
  ADC #$40   ; 0x69 0x40
  ADC #$40   ; 0x69 0x40
  AND #$FF   ; 0x29 0xFF
  AND #$7F   ; 0x29 0x7F
  CMP #$040  ; 0xC9 0x40
  CMP #$050  ; 0xC9 0x50
  CMP #$EF   ; 0xC9 0xEF
  TAX        ; 0xAA
  CPX #$30   ; 0xE0 0x30
  CPX #$40   ; 0xE0 0x40
  CPX #$50   ; 0xE0 0x50
  CPX #$A8   ; 0xE0 0xA8
  CPX #$30   ; 0xE0 0x30
  CPX #$40   ; 0xE0 0x40
  CPX #$50   ; 0xE0 0x50
  CPX #$EF   ; 0xE0 0xEF
  LDA #$00   ; 0xA9 0x00
  LDA #$10   ; 0xA9 0x10
  LDA #$90   
  LDX #$00
  LDX #$10
  LDX #$90 
  LDY #$00
  LDY #$10
  LDY #$20
  ORA #$00
  ORA #$FF
  SEC ; 0x38
  LDA #$64
  SBC #$10; 0xE9
  CLC ; 0x18
  SEC ; 0x38
  LDA 10
  SBC 8
  SEC ; 0x38
  LDA 10
  SBC 10
  SEC 
  LDA 10
  SBC 11
  SEC
  LDA #$FD
  SBC 2
  NOP ; 0xEA
  NOP ; 0xEA
  LDA #$00
  LDA $FFFE
  LDA $0003
  NOP ; 0xEA
  NOP ; 0xEA
  NOP ; 0xEA
  NOP ; 0xEA
  CLC ; 0x18
  LDA #$10
  ADC $0003
  LDA #$7F
  AND $0300
  LDA #$10
  BIT $C000
  LDA #$CA
  CMP $0003
  CPX $0003
  CPY $C003
  LDA #$01
  STA $C001
  LDX #$02
  STX $C002
  LDY #$03
  STY $C003
  LDA #$FF
  LDX #$FF
  LDY #$FF
  LDA $C001
  LDX $C002
  LDY $C003
  CLV ; 0xB8 (* CLV *)
  ASL $C001
  LDA $C001
  ASL $C001
  LDA $C001
  ASL $C001
  LDA $C001
  ASL $C001
  LDA $C001
  ASL $C001
  LDA $C001
  ASL $C001
  LDA $C001
  ASL $C001
  LDA $C001
  ASL $C001
  LDA $C001
  ASL $C001
  LDA $C001
  DEC $C001
  DEC $C001
  DEC $C001
  DEC $C001
  DEC $C001
  LDA $C001
  INC $C001
  INC $C001
  INC $C001
  INC $C001
  INC $C001
  INC $C001
  LDA $C001
  LSR $C001
  LDA $C001
  LSR $C001
  LDA $C001
  LSR $C001
  CLC ; 0x18 (* CLC *)
  LDA $C001
  LDA $C003
  NOP
  LDA #$03
  NOP ; 0xEA
  LDA #$11
  ADC $01
  LDA #$0F
  AND $01
  LDA #$0F
  BIT $01
  LDA #$EB
  CMP $01
  LDA #$FF
  EOR $01
  LDX $01
  LDY $03
  LDA #$F0
  ORA $01
  SEC ; 0x38 (*  SEC *)
  LDA #$E9
  ADC $01
  DEC $01
  LDX $01
  INC $01
  LDY $01
  LSR $01
  LDY $01
  ROL $01
  LDY $01
  ROR $01
  LDY $01
  LDA #$AC
  STA $05
  LDY $05
  LDX #$BC
  STX $05
  LDA $05
  LDY #$BF
  STY $05
  LDA $05
  
  ; Test LDA AbsoluteX
  LDA #$10
  STA $C010
  LDA #$12
  STA $C012
  LDA #$FF
  LDX #$10
  LDA $C000,X
  LDA #$FF
  LDX #$12
  LDA $C000,X
  ; Test page boundary
  LDA #$10
  STA $C0F0
  LDA #$20
  STA $C100
  LDA #$FF
  LDX #$10
  LDA $C0E0,X
  LDA #$FF
  LDX #$20
  LDA $C0E0,X
  
  ; Test ADC AbsoluteX
  LDA #$10
  STA $C010
  LDA #$12
  STA $C012
  CLC
  LDA #$30
  LDX #$10
  ADC $C000,X
  CLC
  LDA #$30
  LDX #$12
  ADC $C000,X
  ; Test page boundary
  LDA #$10
  STA $C0F0
  LDA #$20
  STA $C100
  CLC
  LDA #$30
  LDX #$10
  ADC $C0E0,X
  CLC
  LDA #$30
  LDX #$20
  ADC $C0E0,X
   
  ; Test AND AbsoluteX
  LDA #$10
  STA $C010
  LDA #$12
  STA $C012
  
  LDA #$0F
  LDX #$10
  AND $C000,X
  
  LDA #$0F
  LDX #$12
  AND $C000,X
  
  ; Test page boundary
  LDA #$10
  STA $C0F0
  LDA #$12
  STA $C100
  
  LDA #$0F
  LDX #$10
  ADC $C0E0,X
  
  LDA #$0F
  LDX #$20
  AND $C0E0,X
     
  ; Test CMP AbsoluteX
  LDA #$10
  STA $C010
  LDA #$20
  STA $C012
  
  LDA #$09
  LDX #$10
  CMP $C000,X
  
  LDA #$10
  LDX #$10
  CMP $C000,X

  LDA #$11
  LDX #$10
  CMP $C000,X
  
  ; Test page boundary
  LDA #$10
  STA $C100
  
  LDA #$09
  LDX #$20
  CMP $C0E0,X

 
  LDA #$10
  LDX #$20
  CMP $C0E0,X

  LDA #$11
  LDX #$20
  CMP $C0E0,X
     
  ; Test EOR AbsoluteX
  LDA #$10
  STA $C010
  
  LDA #$FF
  LDX #$10
  EOR $C000,X
  
  ; Test page boundary
  LDA #$10
  STA $C100
  
  LDA #$FF
  LDX #$20
  EOR $C0E0,X
  
  ; Test LDY AbsoluteX
  LDA #$10
  STA $C010
  
  LDX #$10
  LDY $C000,X
  
  ; Test page boundary
  LDA #$20
  STA $C100
  
  LDX #$20
   
  ; Test ORA AbsoluteX
  LDA #$10
  STA $C010
  
  LDA #$FF
  LDX #$10
  ORA $C000,X
  
  ; Test page boundary
  LDA #$10
  STA $C100
  
  LDA #$FF
  LDX #$20
  ORA $C0E0,X
  
  ; Test SBC AbsoluteX
  LDA #$50
  STA $C010
  
  SEC
  LDA #$55
  LDX #$10
  SBC $C000,X
  
  ; Test page boundary
  LDA #$55
  STA $C100
  
  SEC
  LDA #$65
  LDX #$20
  SBC $C0E0,X

  ; Test STA AbsoluteX
  LDA #$50
  LDX #$10
  STA $C000,X
  LDY #$00
  LDY $C010
  
  ; Test page boundary
  LDA #$50
  LDX #$20
  STA $C0E0,X
  LDY #$00
  LDY $C100

  ; Test ASL AbsoluteX
  CLC
  LDA #$01
  LDX #$10
  STA $C000,X
  
  ASL $C000,X
  LDA $C000,X
  ASL $C000,X
  LDA $C000,X
  ASL $C000,X
  LDA $C000,X
   
  
  ; Test page boundary
  CLC
  LDA #$AA
  STA $C000
  LDA #$01
  LDX #$20
  STA $C0E0,X
  
  ASL $C0E0,X
  LDA $C0E0,X
  ASL $C0E0,X
  LDA $C0E0,X
  ASL $C0E0,X
  LDA $C0E0,X
   
  ; Test DEC AbsoluteX
  CLC
  LDA #$70
  LDX #$10
  STA $C000,X
  
  DEC $C000,X
  LDA $C000,X
  DEC $C000,X
  LDA $C000,X
  DEC $C000,X
  LDA $C000,X
   
  
  ; Test page boundary
  CLC
  LDA #$AA
  STA $C000
  LDA #$70
  LDX #$20
  STA $C0E0,X
  
  DEC $C0E0,X
  LDA $C0E0,X
  DEC $C0E0,X
  LDA $C0E0,X
  DEC $C0E0,X
  LDA $C0E0,X
   
  ; Test INC AbsoluteX
  CLC
  LDA #$70
  LDX #$10
  STA $C000,X
  
  INC $C000,X
  LDA $C000,X
  INC $C000,X
  LDA $C000,X
  INC $C000,X
  LDA $C000,X
   
  
  ; Test page boundary
  CLC
  LDA #$AA
  STA $C000
  LDA #$70
  LDX #$20
  STA $C0E0,X
  
  INC $C0E0,X
  LDA $C0E0,X
  INC $C0E0,X
  LDA $C0E0,X
  INC $C0E0,X
  LDA $C0E0,X
   
  ; Test ROL AbsoluteX
  CLC
  LDA #$70
  LDX #$10
  STA $C000,X
  
  ROL $C000,X
  LDA $C000,X
  ROL $C000,X
  LDA $C000,X
  ROL $C000,X
  LDA $C000,X
   
  
  ; Test page boundary
  CLC
  LDA #$AA
  STA $C000
  LDA #$70
  LDX #$20
  STA $C0E0,X
  
  ROL $C0E0,X
  LDA $C0E0,X
  ROL $C0E0,X
  LDA $C0E0,X
  ROL $C0E0,X
  LDA $C0E0,X
   
  ; Test ROR AbsoluteX
  CLC
  LDA #$70
  LDX #$10
  STA $C000,X
  
  ROR $C000,X
  LDA $C000,X
  ROR $C000,X
  LDA $C000,X
  ROR $C000,X
  LDA $C000,X
   
  
  ; Test page boundary
  CLC
  LDA #$AA
  STA $C000
  LDA #$70
  LDX #$20
  STA $C0E0,X
  
  ROR $C0E0,X
  LDA $C0E0,X
  ROR $C0E0,X
  LDA $C0E0,X
  ROR $C0E0,X
  LDA $C0E0,X
   
  ; Test LDA AbsoluteY
  LDA #$10
  STA $C010
  LDA #$FF
  LDY #$10
  LDA $C000,Y
  
  ; Test page boundary
  LDA #$20
  STA $C100
  LDA #$FF
  LDY #$20
  LDA $C0E0,Y
  
  ; Test ADC AbsoluteX
  LDA #$10
  STA $C010
  CLC
  LDA #$30
  LDY #$10
  ADC $C000,Y
  
  ; Test page boundary
  LDA #$20
  STA $C100
  CLC
  LDA #$30
  LDY #$20
  ADC $C0E0,Y
   
  ; Test AND AbsoluteX
  LDA #$10
  STA $C010
  
  LDA #$0F
  LDY #$10
  AND $C000,Y
  
  ; Test page boundary
  LDA #$12
  STA $C100
  
  LDA #$0F
  LDY #$20
  AND $C0E0,Y
     
  ; Test CMP AbsoluteY
  LDA #$10
  STA $C010
  
  LDA #$09
  LDY #$10
  CMP $C000,Y
  
  LDA #$10
  LDY #$10
  CMP $C000,Y

  LDA #$11
  LDY #$10
  CMP $C000,Y
  
  ; Test page boundary
  LDA #$10
  STA $C100
  
  LDA #$09
  LDY #$20
  CMP $C0E0,Y

 
  LDA #$10
  LDY #$20
  CMP $C0E0,Y

  LDA #$11
  LDY #$20
  CMP $C0E0,Y
     
  ; Test EOR AbsoluteX
  LDA #$10
  STA $C010
  
  LDA #$FF
  LDY #$10
  EOR $C000,Y
  
  ; Test page boundary
  LDA #$10
  STA $C100
  
  LDA #$FF
  LDY #$20
  EOR $C0E0,Y
  
  ; Test LDY AbsoluteX
  LDA #$10
  STA $C010
  
  LDY #$10
  LDX $C000,Y
  
  ; Test page boundary
  LDA #$20
  STA $C100
  
  LDY #$20
   
  ; Test ORA AbsoluteX
  LDA #$10
  STA $C010
  
  LDA #$FF
  LDY #$10
  ORA $C000,Y
  
  ; Test page boundary
  LDA #$10
  STA $C100
  
  LDA #$FF
  LDY #$20
  ORA $C0E0,Y
  
  ; Test SBC AbsoluteX
  LDA #$50
  STA $C010
  
  SEC
  LDA #$55
  LDY #$10
  SBC $C000,Y
  
  ; Test page boundary
  LDA #$55
  STA $C100
  
  SEC
  LDA #$65
  LDY #$20
  SBC $C0E0,Y


VAR1 = $C000
  ; Test STA AbsoluteX
  LDA #$50
  LDY #$10
  STA $C000,Y
  LDX #$00
  LDX $C010
  
  ; Test page boundary
  LDA #$50
  LDY #$20
  STA $C0E0,Y
  LDX #$00
  LDX $C100

  LDY #$0
  LDX #$0
  LDA #$10
  BEQ zero
  LDY #$33
  NOP
  NOP
zero:
  LDX #$44
  NOP

  JMP beqnegcross
beq1:
  JMP beqnegnocross
beq2:
  JMP beqposnocross
beq3:
  JMP beqposcross
cont:

* = $1000
  DEX
  DEX
  DEX
  DEX
  DEX
  DEX
  DEX
  DEX
zeronegnocross:
  DEX
  DEX
  DEX
  JMP beq2
  NOP

beqnegnocross:
  LDY #$0
  LDX #$0
  LDA #$00
  BEQ zeronegnocross
  LDY #$33
  NOP
  NOP
  
  INX
  INX
  INX
* = $20F0
  DEX
  DEX
  DEX
  DEX
  DEX
  DEX
  DEX
  DEX
  DEX
  DEX
zeronegcross:
  DEX
  JMP beq1
  NOP

beqnegcross:
  LDY #$0
  LDX #$0
  LDA #$10
  BEQ zeronegcross
  LDY #$33
  NOP
  NOP
  
  INX
  INX
  INX
  JMP beq1

 * = $3000
beqposnocross:
  LDY #$0
  LDX #$0
  LDA #$10
  BEQ zeroposnocross
  LDY #$33
  NOP
  NOP
  
  INX
  INX
  INX
  JMP beq3

zeroposnocross:
  DEX
  DEX
  DEX
  JMP beq3
  NOP

 * = $31f0
beqposcross:
  LDY #$0
  LDX #$0
  LDA #$10
  BEQ zeroposcross
  LDY #$33
  NOP
  NOP
  
  INX
  INX
  INX
  JMP cont

zeroposcross:
  DEX
  DEX
  DEX
  JMP cont
  NOP