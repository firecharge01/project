.segment "HEADER"
.byte $4e, $45, $53, $1a ; Magic string that always begins an iNES header
.byte $02        ; Number of 16KB PRG-ROM banks
.byte $01        ; Number of 8KB CHR-ROM banks
.byte $01, $00  ; Horizontal mirroring, no save RAM, no mapper
.byte %00000000  ; No special-case flags set, no mapper
.byte $00        ; No PRG-RAM present
.byte $00        ; NTSC format


.segment "ZEROPAGE"
player_x: .res 1
player_y: .res 1
player_dir: .res 1
player_look: .res 1
player_state: .res 1
pad1: .res 1  ; task 3 specific
data: .res 1; tile data
backx: .res 1  ;  not currently in use the idea was to use as ppuadrress
backy: .res 1
back: .res 1 ; byte
dummy: .res 1  ; copy of byte
megatilecount: .res 2
index: .res 2
yb: .res 1
xb: .res 1
relative: .res 1
storex: .res 1
level: .res 1
scrollvalue: .res 1
transformer: .res 1


; constants
PPUCTRL   = $2000
PPUMASK   = $2001
PPUSTATUS = $2002
PPUADDR   = $2006
PPUDATA   = $2007
PPUSCROLL = $2005
OAMADDR   = $2003
OAMDMA    = $4014

;  registro del control
CONTROLLER1 = $4016

; bits para cada boton que planeo usar
BTN_RIGHT   = %00000001
BTN_LEFT    = %00000010
BTN_DOWN   = %00000100
BTN_UP      = %00001000
BTN_START   = %00010000
BTN_SELECT  = %00100000
BTN_B       = %01000000
BTN_A       = %10000000

floor = %00000000
wall1 = %00000001
wall2 = %00000010
seethru = %00000011


.segment "CODE"

.proc irq_handler
  RTI
.endproc

.proc nmi_handler
  LDA #$00
  STA OAMADDR
  LDA #$02
  STA OAMDMA


  ;updates tiles AFTER dma transfer

LDA #$00
STA $2005
STA $2005

 jsr read_controller1
 jsr update_level

  LDA scrollvalue
  STA PPUSCROLL ; set X scroll
  LDA #$00
  STA PPUSCROLL ; set Y scroll

  LDA level
  CMP transformer
  BEQ wegood
  LDA level
  STA transformer

  JSR main
  wegood:

  RTI
.endproc

.export reset_handler
.proc reset_handler
  SEI
  CLD
  LDX #$40
  STX $4017
  LDX #$FF
  TXS
  INX
  STX $2000
  STX $2001
  STX $4010
  BIT $2002
vblankwait:
  BIT $2002
  BPL vblankwait

LDX #$00
LDA #$FF
clear_oam:
STA $0200,X ; set sprite y-positions off the screen
INX
INX
INX
INX
BNE clear_oam

vblankwait2:
  BIT $2002
  BPL vblankwait2
  JMP main
.endproc


.export main
.proc main


  ; write a palette
  LDX PPUSTATUS
  LDX #$3f
  STX PPUADDR
  LDX #$00
  STX PPUADDR
load_palettes:
  LDA palettes,X
  STA PPUDATA
  INX
  CPX #$20
  BNE load_palettes


; THIS MIGHT BE RESONSIBLE FOR it only doing like the first loop
load_address:
  LDA PPUSTATUS
  LDA #$00
  STA backx

  LDA PPUSTATUS
  LDA #$20
  STA backy

  LDA PPUSTATUS
  LDA #$00
  STA yb

  LDA PPUSTATUS
  LDA #$00
  STA xb

 
  LDA PPUSTATUS
  LDA #$00
  STA relative

  LDA #$00
  STA megatilecount

  LDx #$00
 

LoadBackgroundLoop1:

    lda megatilecount
    sta yb
    lda yb
    lsr yb
    lsr yb
    asl yb
    asl yb
    asl yb
    asl yb
    asl yb
    asl yb

;00000001
;00001000

    lda megatilecount
    sta xb
    lda xb
    and #$03
    sta xb
    asl xb
    asl xb
    asl xb
    clc
    lda #$00
    adc yb
    adc xb
    sta index

  LDA level
  CMP #$00
  BNE secondary

  lda background, x
  STA dummy
  lda #$00
  sta relative
  JSR tileset1
  asl dummy
  asl dummy
  lda #$02
  sta relative
  JSR tileset1
  asl dummy
  asl dummy
  lda #$04
  sta relative
  JSR tileset1
  asl dummy
  asl dummy
  lda #$06
  sta relative
  JSR tileset1
  jmp skipper

  secondary:
  lda background2, x
  STA dummy
  lda #$00
  sta relative
  JSR tileset2
  asl dummy
  asl dummy
  lda #$02
  sta relative
  JSR tileset2
  asl dummy
  asl dummy
  lda #$04
  sta relative
  JSR tileset2
  asl dummy
  asl dummy
  lda #$06
  sta relative
  JSR tileset2


  skipper:
  stx storex
  lda storex
  cmp #$3b
  ; beq loadsecond
  ; cmp #$77
  beq BACKdone

  ;loadsecond
  ; LDA #$24
  ; STA backy
  ; LDA #$00
  ; STA backx

  lda index
  cmp #$D8
  bne continue

  inc backy
  lda #$00
  sta megatilecount
  lda #$00
  sta index
 
  jumpa:
  inx
  jmp LoadBackgroundLoop1
  continue:
  inc megatilecount
  ;inx
  lda #$10
  cmp megatilecount

  bne jumpa
 
  BACKdone:

vblankwait:       ; wait for another vblank before continuing
  BIT PPUSTATUS
  BPL vblankwait

  LDA #%10000000  ; turn on NMIs, sprites use first pattern table
  STA PPUCTRL
  LDA #%00011110  ; turn on screen
  STA PPUMASK

forever:
  jmp forever
.endproc

.proc tileset1
  PHP
PHA
TXA
PHA
TYA
PHA

  lda dummy
  AND #%11000000
  sta back

  check_seethru:
  lda back
  cmp #%11000000
  bne check_wall1
  jmp s1
  check_wall1:
  lda back
  CMP #%01000000
  bne check_wall2
  jmp s2
  check_wall2:
  lda back
  CMP #%10000000
  bne check_floor
  jmp s3
  check_floor:
  LDA PPUSTATUS
  LDA #$00; for later thisll likely have to be this + stage offset
  STA data
  jmp place_tile

  s1:
  LDA PPUSTATUS
  LDA #$40
  STA data
  jmp place_tile
  s2:
  LDA PPUSTATUS
  LDA #$2c
  STA data
  jmp place_tile
  s3:
  LDA PPUSTATUS
  LDA #$2e
  STA data
  jmp place_tile

  ;NAMETABLE
  ; add something here that will manipulate data based on read value
  ;lets say for now  00 =60, 01  = 61, 10  = 62, 11 = 63


place_tile:

    placement:

      LDA PPUSTATUS
      ;;adress
      LDA backy
      STA PPUADDR
      clc
      LDA backx
      adc index
      adc relative
      STA PPUADDR
      LDA data
      STA PPUDATA



  ;2
      clc
      LDA PPUSTATUS
      LDA backy

      STA PPUADDR
      clc
      LDA backx
     
      adc index
      adc relative
      adc #$01
      STA PPUADDR
      LDA data
      CLC
      ADC #$01
      STA PPUDATA
      ; inc backx
      ; inc backx
      ;inc data


  ;3

      CLC
      ; inc backx
      LDA PPUSTATUS     ;basic loading background form
      LDA backy
 
      STA PPUADDR
      clc
      LDA backx
      clc
      adc index
      adc relative
      ADC #$20
      STA PPUADDR
      LDA data        ;
      CLC
      ADC #$10
      STA PPUDATA
      ; inc backx
      ; inc backx

  ;4

      CLC
      LDA PPUSTATUS     ;basic loading background form
      LDA backy
      ;ADC #$21
      STA PPUADDR
      clc
      LDA backx
      clc
      adc index
      adc relative
      ADC #$21
      STA PPUADDR
      LDA data        
      CLC
      ADC #$11
      STA PPUDATA

PLA
TAY
PLA
TAX
PLA
PLP
RTS ;rts ends subroutine
.endproc

.proc tileset2
  PHP
PHA
TXA
PHA
TYA
PHA

  lda dummy
  AND #%11000000
  sta back

  check_seethru:
  lda back
  cmp #%11000000
  bne check_wall1
  jmp s1
  check_wall1:
  lda back
  CMP #%01000000
  bne check_wall2
  jmp s2
  check_wall2:
  lda back
  CMP #%10000000
  bne check_floor
  jmp s3
  check_floor:
  LDA PPUSTATUS
  LDA #$00; for later thisll likely have to be this + stage offset
  STA data
  jmp place_tile



  s1:
  LDA PPUSTATUS
  LDA #$48
  STA data
  jmp place_tile
  s2:
  LDA PPUSTATUS
  LDA #$44
  STA data
  jmp place_tile
  s3:
  LDA PPUSTATUS
  LDA #$46
  STA data
  jmp place_tile

  ;NAMETABLE
  ; add something here that will manipulate data based on read value
  ;lets say for now  00 =60, 01  = 61, 10  = 62, 11 = 63


place_tile:

    placement:

      LDA PPUSTATUS
      ;;adress
      LDA backy
      STA PPUADDR
      clc
      LDA backx
      adc index
      adc relative
      STA PPUADDR
      LDA data
      STA PPUDATA



  ;2
      clc
      LDA PPUSTATUS
      LDA backy

      STA PPUADDR
      clc
      LDA backx
     
      adc index
      adc relative
      adc #$01
      STA PPUADDR
      LDA data
      CLC
      ADC #$01
      STA PPUDATA
      ; inc backx
      ; inc backx
      ;inc data


  ;3

      CLC
      ; inc backx
      LDA PPUSTATUS     ;basic loading background form
      LDA backy
 
      STA PPUADDR
      clc
      LDA backx
      clc
      adc index
      adc relative
      ADC #$20
      STA PPUADDR
      LDA data        ;
      CLC
      ADC #$10
      STA PPUDATA
      ; inc backx
      ; inc backx

  ;4

      CLC
      LDA PPUSTATUS     ;basic loading background form
      LDA backy
      ;ADC #$21
      STA PPUADDR
      clc
      LDA backx
      clc
      adc index
      adc relative
      ADC #$21
      STA PPUADDR
      LDA data        
      CLC
      ADC #$11
      STA PPUDATA

PLA
TAY
PLA
TAX
PLA
PLP
RTS ;rts ends subroutine
.endproc

 .proc update_level
  PHP  ; Start by saving registers,
  PHA  ; as usual.
  TXA
  PHA
  TYA
  PHA

  LDA pad1
  cmp $00
  BNE check_left
  jmp done_checking
check_left:
  ldx #$00
  stx move 
  LDA pad1        ; Load button presses
  AND #BTN_LEFT   ; Filter out all but Left
  BEQ check_right 
  LDA scrollvalue
  SBC #$01
  STA scrollvalue
  ;scroll it left
  ; If result is zero, left not pressed
  ; If the branch is not taken, move player left
check_right:
  LDA pad1
  AND #BTN_RIGHT
  BEQ check_A 
  LDA scrollvalue
  ADC #$01
  STA scrollvalue
  ;scroll it right
check_A:
  LDA pad1
  AND #BTN_A
  BEQ done_checking
  LDA #$00
  CMP level
  BNE backtofirstlvl
  LDA #$01
  STA level
  jmp done_checking
  backtofirstlvl:
  LDA #$00
  STA level
  done_checking:


  PLA ; Done with updates, restore registers
  TAY ; and return to where we called this
  PLA
  TAX
  PLA
  PLP
  RTS
  .endproc

.proc read_controller1
  PHA
  TXA
  PHA
  PHP

  ; write a 1, then a 0, to CONTROLLER1
  ; to latch button states
  LDA #$01
  STA CONTROLLER1
  LDA #$00
  STA CONTROLLER1

  LDA #%00000001
  STA pad1

get_buttons:
  LDA CONTROLLER1 ; Read next button's state
  LSR A           ; Shift button state right, into carry flag
  ROL pad1        ; Rotate button state from carry flag
                  ; onto right side of pad1
                  ; and leftmost 0 of pad1 into carry flag
  BCC get_buttons ; Continue until original "1" is in carry flag

  PLP
  PLA
  TAX
  PLA
  RTS
.endproc

.segment "VECTORS"
.addr nmi_handler, reset_handler, irq_handler

.segment "RODATA"
palettes:

  .byte $0f, $2d, $1a, $0C
  .byte $0f, $37, $28, $30 ; bg palettes
  .byte $0f, $38, $2D, $3D
  .byte $0f, $09, $1a, $24


.byte $0f, $0C, $21, $32 ;
.byte $0f, $19, $09, $29
.byte $0f, $19, $09, $29
.byte $0f, $19, $09, $29

sprites:
;suouth movement sprites
;#1
.byte $20, $05, $01, $70 ; position y, sprite, palette, position x
.byte $20, $06, $01, $78
.byte $28, $15, $01, $70
.byte $28, $16, $01, $78

;# neutral
background:
.byte %00000000, %00000000, %00000000, %00000000    ;cave1
.byte %10101010, %10101010, %10101010, %10101010
.byte %01010000, %00000000, %00000000, %00000100
.byte %01010000, %01110000, %00010001, %01010100
.byte %01010000, %01110101, %01010000, %00000100
.byte %00000000, %01110100,	%00010101, %01000101
.byte %00000000, %01110101,	%00000000, %01000000
.byte %01010000, %01110101, %00010000, %00000100
.byte %01010000, %01111111, %11010100, %01010101
.byte %01010000, %01010101, %00010000, %01000000
.byte %01010011, %11111101, %01010101, %01000101
.byte %01010101, %01011101, %00000000, %00000100
.byte %01010011, %11111101, %01010000, %01010100
.byte %01010000, %11111111, %11111111, %01000000
.byte %10101010, %10101010, %10101010, %10101010

.byte %00000000,%00000000,%00000000,%00000000   ;cave2
.byte %10101010,%10101010,%10101010,%10101010
.byte %00000000,%00000000,%00010000,%00010001
.byte %01000000,%01111101,%00111101,%00000001
.byte %01000101,%01111101,%01111111,%01010001
.byte %01000100,%11111101,%01110111,%00000101
.byte %00000100,%01010101,%01000111,%01000001
.byte %00000000,%00000000,%01000100,%01010001
.byte %01010101,%01000100,%00000100,%01000001
.byte %00000000,%01000101,%01010100,%01010101
.byte %01010100,%01110000,%00000000,%00010000
.byte %00000000,%01110100,%01010101,%01010001
.byte %00010101,%01110111,%11011111,%11010001
.byte %01010000,%00110111,%11111101,%11111101
.byte %10101010,%10101010,%10101010,%10101010

background2:

.byte %00000000,%00000000,%00000000,%00000000   ;desert1
.byte %10101010,%10101010,%10101010,%10101010
.byte %01000000,%00000111,%11110000,%00000100
.byte %01000101,%01000111,%01110101,%01000100
.byte %01000000,%01111111,%01111111,%01000101
.byte %01010100,%01110101,%01010111,%01000011
.byte %01000000,%01111101,%11111111,%01010101
.byte %00000100,%01011101,%11010101,%01011111
.byte %01000100,%00011111,%11010000,%00111101
.byte %01010101,%00010101,%11010001,%01011101
.byte %01000000,%00011111,%11111100,%00011101
.byte %01000101,%01011101,%01011101,%00010000
.byte %01000000,%01001111,%01111101,%00010001
.byte %01000100,%00000111,%11110101,%00000001
.byte %10101010,%10101010,%10101010,%10101010

.byte %00000000,%00000000,%00000000,%00000000   ;desert2
.byte %10101010,%10101010,%10101010,%10101010
.byte %11110000,%01010011,%11011111,%11000001
.byte %11110100,%00000001,%11111101,%01010001
.byte %01110101,%01010101,%11011101,%00010001
.byte %11111101,%00000011,%11011111,%00010001
.byte %01011101,%01010001,%11011101,%00010001
.byte %11010000,%00000001,%11111101,%00000001
.byte %11010001,%01010101,%01010101,%00010101
.byte %11000011,%11110111,%00000001,%00111101
.byte %01010111,%01110111,%01010001,%00011101
.byte %00011111,%01111111,%11010001,%00010000
.byte %00011101,%01010101,%11010101,%00010001
.byte %00001111,%11111111,%11010000,%00010001
.byte %10101010,%10101010,%10101010,%10101010
;wrtir down bytes for map
; look at palettes to see how i load them up
;then i work on the indexing

.segment "CHARS"
.incbin "project.chr"