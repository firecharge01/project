;;Sebastián Torrez Segarra 
;;Cruz Diaz Rivera
.segment "HEADER"
.byte $4e, $45, $53, $1a ; Magic string that always begins an iNES header
.byte $02        ; Number of 16KB PRG-ROM banks
.byte $01        ; Number of 8KB CHR-ROM banks
.byte $01, $00  ; Horizontal mirroring, no save RAM, no mapper
.byte %00000000  ; No special-case flags set, no mapper
.byte $00        ; No PRG-RAM present
.byte $00        ; NTSC format

.segment "VECTORS"
  ;; When an NMI happens (once per frame if enabled) the label nmi:
  .addr nmi
  ;; When the processor first turns on or is reset, it will jump to the label reset:
  .addr reset
  ;; External interrupt IRQ (unused)
  .addr 0

; "nes" linker config requires a STARTUP section, even if it's empty
.segment "STARTUP"

.segment "ZEROPAGE"
player_x: .res 1
player_y: .res 1
playerstate: .res 1
timer: .res 1
player_dir: .res 1
pad1: .res 1
move: .res 1
xover64: .res 1
yover16: .res 1
finbyte: .res 1
finblock: .res 1
stage2: .res 1
data: .res 1; tile data
backx: .res 1  ;  not currently in use the idea was to use as ppuadrress
backy: .res 1
back: .res 1 ; byte
dummy: .res 1  ; copy of byte
megatilecount: .res 1
index: .res 1
yb: .res 1
xb: .res 1
relative: .res 1
storex: .res 1
level: .res 1
transformer: .res 1
nametableoffset: .res 1


; Main code segment for the program
.segment "CODE"


.proc irq_handler
  RTI
.endproc


.proc nmi
nmi:
  ldx #$00 	; Set SPR-RAM address to 0
  stx $2003
  ldx #$02
  stx $4014

jsr read_controller1
jsr update_player

sprite:
ldx player_dir
cpx #$00
beq roll0
cpx #$01
beq roll1
cpx #$02
beq roll2
cpx #$03
beq roll3
roll0:
jsr rollout
jmp postsprite
roll1:
jsr rollout2
jmp postsprite
roll2:
jsr rollout3
jmp postsprite
roll3:
jsr rollout4


postsprite:
  LDA #$00        ;;tell the ppu there is no background scrolling
  STA $2005
  STA $2005

  rti
.endproc
PPUCTRL   = $2000
PPUMASK   = $2001
PPUSTATUS = $2002
PPUSCROLL = $2005
PPUADDR   = $2006
PPUDATA   = $2007
OAMADDR   = $2003
OAMDMA    = $4014

CONTROLLER1 = $4016
CONTROLLER2 = $4017

BTN_RIGHT   = %00000001
BTN_LEFT    = %00000010
BTN_DOWN   = %00000100
BTN_UP      = %00001000
BTN_START   = %00010000
BTN_SELECT  = %00100000
BTN_B       = %01000000
BTN_A       = %10000000
.proc reset
  sei		; disable IRQs
  cld		; disable decimal mode
  ldx #$40
  stx $4017	; disable APU frame IRQ
  ldx #$ff 	; Set up stack
  txs		;  .
  inx		; now X = 0
  stx $2000	; disable NMI
  stx $2001 	; disable rendering
  stx $4010 	; disable DMC IRQs
ldx #$01
stx player_x
ldx #80
stx player_y


;; first wait for vblank to make sure PPU is read


clear_memory:
  lda #$00
  sta $0000, x
  sta $0100, x
  sta $0200, x
  sta $0300, x
  sta $0400, x
  sta $0500, x
  sta $0600, x
  sta $0700, x
  inx
  bne clear_memory

  vblankwait1:
  bit $2002
  bpl vblankwait1
.endproc

;; second wait for vblank, PPU is ready after this





.proc main
load_palettes:
  lda $2002
  lda #$3f
  sta $2006
  lda #$00
  sta $2006
  ldx #$00
@loop:
  lda palettes, x
  sta $2007
  inx
  cpx #$20
  bne @loop

.endproc

.proc leveld
lda level
cmp #$00
beq runitback

  LoadAttribute:
  LDA $2002             ; read PPU status to reset the high/low latch
  LDA #$27
  STA $2006             ; write the high byte of $23C0 address
  LDA #$c0
  STA $2006             ; write the low byte of $23C0 address
  LDX #$00              ; start out at 0
LoadAttributeLoop:
  LDA #%01010101      ; load data from address (attribute + the value in x)
  STA $2007             ; write to PPU
  INX                   ; X = X + 1
  CPX #$40     ; Compare X to hex $08, decimal 8 - copying 8 bytes
  BNE LoadAttributeLoop
  LoadAttribute2:
  LDA $2002             ; read PPU status to reset the high/low latch
  LDA #$23
  STA $2006             ; write the high byte of $23C0 address
  LDA #$c0
  STA $2006             ; write the low byte of $23C0 address
  LDX #$00              ; start out at 0
LoadAttributeLoop2:
  LDA #%01010101      ; load data from address (attribute + the value in x)
  STA $2007             ; write to PPU
  INX                   ; X = X + 1
  CPX #$40     ; Compare X to hex $08, decimal 8 - copying 8 bytes
  BNE LoadAttributeLoop2




  runitback:
  LDA #$01
  CMP transformer
  BNE allg
  LDX #$00
  STX transformer
  allg:

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

  clc
  LDA level
  CMP #$00
  bne secondary

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
  lda #$00
  sta nametableoffset
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

  LDA #$04
  STA nametableoffset
  LDA #$01
  CMP transformer
  BNE allg2
  LDX #$00
  STX transformer
  allg2:
  ; write a palette
  LDX PPUSTATUS
  LDX #$3f
  STX PPUADDR
  LDX #$00
  STX PPUADDR
load_palettes2:
  LDA palettes,X
  STA PPUDATA
  INX
  CPX #$20
  BNE load_palettes2


; THIS MIGHT BE RESONSIBLE FOR it only doing like the first loop
load_address2:
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
 

LoadBackgroundLoop2:

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
  BNE secondary2

  lda background1b, x
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
  jmp skipper2

  secondary2:
  LDA #$04
  STA nametableoffset
  lda background2b, x
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


  skipper2:
  stx storex
  lda storex
  cmp #$3b
  ; beq loadsecond
  ; cmp #$77
  beq BACKdone2

  ;loadsecond
  ; LDA #$24
  ; STA backy
  ; LDA #$00
  ; STA backx

  lda index
  cmp #$D8
  bne continue2

  inc backy
  lda #$00
  sta megatilecount
  lda #$00
  sta index
 
  jumpa2:
  inx
  jmp LoadBackgroundLoop2
  continue2:
  inc megatilecount
  ;inx
  lda #$10
  cmp megatilecount

  bne jumpa2
 
  BACKdone2:
  ; LDA #$00
  ; STA nametableoffset
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
      CLC 
      ADC nametableoffset
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
      ADC nametableoffset
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
      ADC nametableoffset
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
      ADC nametableoffset
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
      CLC
      LDA PPUSTATUS
      ;;adress
      LDA backy
      ADC nametableoffset
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
      adc nametableoffset
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
      adc nametableoffset
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
      adc nametableoffset
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

  .proc update_player
  PHP  ; Start by saving registers,
  PHA  ; as usual.
  TXA
  PHA
  TYA
  PHA
  LDA pad1
  cmp $00
  BNE check_left
  ldx #$01
  stx move
  jmp done_checking
check_left:
  clc
  ldx #$00
  stx move 
  LDA pad1        ; Load button presses
  AND #BTN_LEFT   ; Filter out all but Left
  BEQ check_right 
  ldx #$01
  stx player_dir; If result is zero, left not pressed
  DEC player_x 
  jsr check_collision1
  beq check_right1
  jsr check_collision4
  bne check_right
  check_right1:
  inc player_x 
check_right:
clc
  LDA pad1
  AND #BTN_RIGHT
  BEQ check_up
  ldx #$00
  stx player_dir
  INC player_x
  jsr check_collision2
  beq check_up1
  jsr check_collision3
  bne check_up
  check_up1:
  dec player_x
check_up:
clc
  LDA pad1
  AND #BTN_UP
  BEQ check_down
  ldx #$02
  stx player_dir
  DEC player_y
  jsr check_collision1
  beq check_down1
  jsr check_collision2
  bne check_down
  check_down1:
  inc player_y
check_down:
clc
  LDA pad1
  AND #BTN_DOWN
  BEQ check_A
  ldx #$03
  stx player_dir
  INC player_y
  jsr check_collision3
  beq done_checkingc
  jsr check_collision4
  bne check_A
  done_checkingc:
  dec player_y
check_A:
  LDA pad1
  AND #BTN_A
  BEQ doney
  ; LDA #$00
  ; CMP level
  ; BNE backtofirstlvl
  LDA #$01
  STA level

  LDA #%00  ; turn on NMIs, sprites use first pattern table
  STA PPUCTRL
  sta PPUMASK

  LDA #$01
  STA transformer
  jmp leveld
  doney:
  jmp done_checking

.proc check_collision1

LDA player_x
clc
adc #$01
  ror A
  clc
  ror A
  clc
  ror A
  clc
  ror A
  clc
  ror A
  clc 
  ror A
  clc
  sta xover64
  lda player_y
  clc
  adc #$02
  ror A
  clc
  ror A
  clc
  ror A
  clc
  ror A
  clc
  asl
  asl
  sta yover16
  

  lda xover64
  clc
  adc yover16
  clc
  sta finbyte

  lda finbyte
  LDA player_x
  clc
  adc #$01
  ror A
  clc
  ror A
  clc
  ror A
  clc
  ror A
  clc
  and #$03
  sta finblock
  ldy finbyte
  lda stage2
  cmp #$01
  bne next
  lda finbyte
  clc
  adc #$3c
  clc
  sta finbyte
  TAY
  next:
  ldx finblock
  lda backgroundc, y
  and collisionmap, X
  cmp typebit1, X
  beq end
  cmp typebit2, x

 end:
 RTS
.endproc

.proc check_collision2
LDA player_x
clc
adc #$0e
  ror A
  clc
  ror A
  clc
  ror A
  clc
  ror A
  clc
  ror A
  clc 
  ror A
  clc
  sta xover64
  lda player_y
  clc
  adc #$02
  ror A
  clc
  ror A
  clc
  ror A
  clc
  ror A
  clc
  asl
  asl
  sta yover16
  

  lda xover64
  clc
  adc yover16
  clc
  sta finbyte

  lda finbyte
  LDA player_x
  clc
  adc #$0e
  ror A
  clc
  ror A
  clc
  ror A
  clc
  ror A
  clc
  and #$03
  sta finblock
  ldy finbyte
  lda stage2
  cmp #$01
  bne next
  lda finbyte
  clc
  adc #$3c
  clc
  sta finbyte
  TAY
  next:
  ldx finblock
  lda backgroundc, y
  and collisionmap, X
  cmp typebit1, X
  beq end
  cmp typebit2, x

 end:
 RTS
.endproc

.proc check_collision3
LDA player_x
clc
adc #$0e
  ror A
  clc
  ror A
  clc
  ror A
  clc
  ror A
  clc
  ror A
  clc 
  ror A
  clc
  sta xover64
  lda player_y
  clc
  adc #$0f
  ror A
  clc
  ror A
  clc
  ror A
  clc
  ror A
  clc
  asl
  asl
  sta yover16
  

  lda xover64
  clc
  adc yover16
  clc
  sta finbyte

  lda finbyte
  LDA player_x
  clc
  adc #$0e
  ror A
  clc
  ror A
  clc
  ror A
  clc
  ror A
  clc
  and #$03
  sta finblock
  ldy finbyte
  lda stage2
  cmp #$01
  bne next
  lda finbyte
  clc
  adc #$3c
  sta finbyte
  TAY
  next:
  ldx finblock
  lda backgroundc, y
  and collisionmap, X
  cmp typebit1, X
  beq end
  cmp typebit2, x

 end:
 RTS
.endproc
.proc check_collision4
LDA player_x
clc
adc #$01
  ror A
  clc
  ror A
  clc
  ror A
  clc
  ror A
  clc
  ror A
  clc 
  ror A
  clc
  sta xover64
  lda player_y
  clc
  adc #$0f
  ror A
  clc
  ror A
  clc
  ror A
  clc
  ror A
  clc
  asl
  asl
  sta yover16
  

  lda xover64
  clc
  adc yover16
  clc
  sta finbyte

  lda finbyte
  LDA player_x
  adc #$01
  ror A
  clc
  ror A
  clc
  ror A
  clc
  ror A
  clc
  and #$03
  sta finblock
  ldy finbyte
  lda stage2
  cmp #$01
  bne next
  lda finbyte
  clc
  adc #$3c
  clc
  sta finbyte
  TAY
  next:
  ldx finblock
  lda backgroundc, y
  and collisionmap, X
  cmp typebit1, X
  beq end
  cmp typebit2, x

 end:
 RTS
.endproc

done_checking:

  lda player_x
  adc #$0f
  cmp #$ff
  bne done
  lda #$02
  sta player_x
  lda #97
  sta player_y
  lda #$01
  sta stage2
  lda #%10000001	; Enable NMI and change background center
  sta $2000


  done:
  PLA ; Done with updates, restore registers
  TAY ; and return to where we called this
  PLA
  TAX
  PLA
  PLP
  RTS
.endproc



.proc rollout
  PHP
  PHA
  TXA
  PHA
  TYA
  PHA
  ldx move
  cpx #$01
  beq state0
  lda timer
  cmp #$00
  BNE incro
  ldx #00
  stx playerstate
  incro: 
  adc#$01
  sta timer
  cmp #$14
  BNE try2
  ldx #$01
  stx playerstate
  try2:
  CLC
  cmp #$28
  BNE state0
  ldx #$02
  stx playerstate

state0:
LDA #$06
  STA $0201
  LDA #$07
  STA $0205
  LDA #$16
  STA $0209
  LDA #$17
  STA $020d
  ldx move
  cpx #$01
  beq startdraw
  ldx playerstate
  cpx #$01
  BEQ state1
  jmp state2
state1:
  LDA #$04
  STA $0201
  LDA #$05
  STA $0205
  LDA #$14
  STA $0209
  LDA #$15
  STA $020d
  jmp startdraw

state2:
  ldx playerstate
  cpx #$02
  bne startdraw
still:
  LDA #$02
  STA $0201
  LDA #$03
  STA $0205
  LDA #$12
  STA $0209
  LDA #$13
  STA $020d
  

startdraw:
  ; write player ship tile attributes
  ; use palette 1
  LDA #%00100001
  
  STA $0202
  STA $0206
  STA $020a
  STA $020e

  ; top left tile:
  LDA player_y
  STA $0200
  LDA player_x
  STA $0203

  ; top right tile (x + 8):
  LDA player_y
  STA $0204
  LDA player_x
  CLC
  ADC #$08
  STA $0207

  ; bottom left tile (y + 8):
  LDA player_y
  CLC
  ADC #$08
  STA $0208
  LDA player_x
  STA $020b

  ; bottom right tile (x + 8, y + 8)
  LDA player_y
  CLC
  ADC #$08
  STA $020c
  LDA player_x
  CLC
  ADC #$08
  STA $020f

lda timer
  cmp #$3c
  BNE end
  lda #$00
  sta timer
  end:

  
  PLA
  TAY
  PLA
  TAX
  PLA
  PLP

  RTS
.endproc

.proc rollout2
  PHP
  PHA
  TXA
  PHA
  TYA
  PHA
  ldx move
  cpx #$01
  beq state0
  lda timer
  cmp #$00
  BNE incro
  ldx #00
  stx playerstate
  incro: 
  adc#$01
  sta timer
  cmp #$14
  BNE try2
  ldx #$01
  stx playerstate
  try2:
  CLC
  cmp #$28
  BNE state0
  ldx #$02
  stx playerstate

state0:
  LDA #$0c
  STA $0201
  LDA #$0d
  STA $0205
  LDA #$1c
  STA $0209
  LDA #$1d
  STA $020d
  
  ldx move
  cpx #$01
  beq startdraw
  ldx playerstate
  cpx #$01
  BEQ state1
  jmp state2
state1:
LDA #$08
  STA $0201
  LDA #$09
  STA $0205
  LDA #$18
  STA $0209
  LDA #$19
  STA $020d
  
  jmp startdraw

state2:
  ldx playerstate
  cpx #$02
  bne startdraw
  LDA #$0a
  STA $0201
  LDA #$0b
  STA $0205
  LDA #$1a
  STA $0209
  LDA #$1b
  STA $020d
  
startdraw:
  ; write player ship tile attributes
  ; use palette 1
 LDA #%00100001
  STA $0202
  STA $0206
  STA $020a
  STA $020e

  ; top left tile:
  LDA player_y
  STA $0200
  LDA player_x
  STA $0203

  ; top right tile (x + 8):
  LDA player_y
  STA $0204
  LDA player_x
  CLC
  ADC #$08
  STA $0207

  ; bottom left tile (y + 8):
  LDA player_y
  CLC
  ADC #$08
  STA $0208
  LDA player_x
  STA $020b

  ; bottom right tile (x + 8, y + 8)
  LDA player_y
  CLC
  ADC #$08
  STA $020c
  LDA player_x
  CLC
  ADC #$08
  STA $020f

lda timer
  cmp #$3c
  BNE end
  lda #$00
  sta timer
  end:

  
  PLA
  TAY
  PLA
  TAX
  PLA
  PLP

  RTS
.endproc


.proc rollout3
  PHP
  PHA
  TXA
  PHA
  TYA
  PHA

  ldx move
  cpx #$01
  beq state0
  lda timer
  cmp #$00
  BNE incro
  ldx #00
  stx playerstate
  incro: 
  adc#$01
  sta timer
  cmp #$14
  BNE try2
  ldx #$01
  stx playerstate
  try2:
  CLC
  cmp #$28
  BNE state0
  ldx #$02
  stx playerstate

state0:
  LDA #$0e
  STA $0201
  LDA #$0f
  STA $0205
  LDA #$1e
  STA $0209
  LDA #$1f
  STA $020d
  ldx move
  cpx #$01
  beq startdraw

  ldx playerstate
  cpx #$01
  BEQ state1
  jmp state2
state1:
  LDA #$20
  STA $0201
  LDA #$21
  STA $0205
  LDA #$30
  STA $0209
  LDA #$31
  STA $020d
  jmp startdraw

state2:
  ldx playerstate
  cpx #$02
  bne startdraw
  LDA #$22
  STA $0201
  LDA #$23
  STA $0205
  LDA #$32 
  STA $0209
  LDA #$33
  STA $020d
  

startdraw:
  ; write player ship tile attributes
  ; use palette 1
  LDA #%00100001
  STA $0202
  STA $0206
  STA $020a
  STA $020e

  ; top left tile:
  LDA player_y
  STA $0200
  LDA player_x
  STA $0203

  ; top right tile (x + 8):
  LDA player_y
  STA $0204
  LDA player_x
  CLC
  ADC #$08
  STA $0207

  ; bottom left tile (y + 8):
  LDA player_y
  CLC
  ADC #$08
  STA $0208
  LDA player_x
  STA $020b

  ; bottom right tile (x + 8, y + 8)
  LDA player_y
  CLC
  ADC #$08
  STA $020c
  LDA player_x
  CLC
  ADC #$08
  STA $020f

lda timer
  cmp #$3c
  BNE end
  lda #$00
  sta timer
  end:

  
  PLA
  TAY
  PLA
  TAX
  PLA
  PLP

  RTS
.endproc


.proc rollout4
  PHP
  PHA
  TXA
  PHA
  TYA
  PHA

  ldx move
  cpx #$01
  beq state0
  lda timer
  cmp #$00
  BNE incro
  ldx #00
  stx playerstate
  incro: 
  adc#$01
  sta timer
  cmp #$14
  BNE try2
  ldx #$01
  stx playerstate
  try2:
  CLC
  cmp #$28
  BNE state0
  ldx #$02
  stx playerstate

state0:
  LDA #$24
  STA $0201
  LDA #$25
  STA $0205
  LDA #$34
  STA $0209
  LDA #$35
  STA $020d
  ldx move
  cpx #$01
  beq startdraw

  ldx playerstate
  cpx #$01
  BEQ state1
  jmp state2
state1:
  LDA #$26
  STA $0201
  LDA #$27
  STA $0205
  LDA #$36
  STA $0209
  LDA #$37
  STA $020d
  jmp startdraw

state2:
  ldx playerstate
  cpx #$02
  bne startdraw
  LDA #$28
  STA $0201
  LDA #$29
  STA $0205
  LDA #$38 
  STA $0209
  LDA #$39
  STA $020d
  

startdraw:
  ; write player ship tile attributes
  ; use palette 1
 LDA #%00100001
  STA $0202
  STA $0206
  STA $020a
  STA $020e

  ; top left tile:
  LDA player_y
  STA $0200
  LDA player_x
  STA $0203

  ; top right tile (x + 8):
  LDA player_y
  STA $0204
  LDA player_x
  CLC
  ADC #$08
  STA $0207

  ; bottom left tile (y + 8):
  LDA player_y
  CLC
  ADC #$08
  STA $0208
  LDA player_x
  STA $020b

  ; bottom right tile (x + 8, y + 8)
  LDA player_y
  CLC
  ADC #$08
  STA $020c
  LDA player_x
  CLC
  ADC #$08
  STA $020f



  lda timer
  cmp #$3c
  BNE end
  lda #$00
  sta timer
  end:
  PLA
  TAY
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

; hello:
;   .byte $08, $02, $01, $00 ; y coord, pattern, palette, x coord 
;   .byte $08, $03, $01, $08
;   .byte $10, $12, $01, $00
;   .byte $10, $13, $01, $08
.segment "RODATA"
palettes:
  ; Background Palette
  .byte $0f, $2d, $1a, $0C
  .byte $0f, $37, $28, $30 ; bg palettes
  .byte $0f, $38, $2D, $3D
  .byte $0f, $09, $1a, $24

  ; Sprite Palette
  .byte $0f, $37, $28, $30
  .byte $0f, $38, $2D, $3D
  .byte $0f, $09, $1a, $24
  .byte $0f, $2d, $1a, $0C

backgroundc:
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

collisionmap:
.byte %11000000, %00110000, %00001100, %00000011

typebit1:
.byte %01000000, %00010000, %00000100, %00000001
typebit2:
.byte %10000000, %00100000, %00001000, %00000010


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

background1b:
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

background2b:
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


; Character memory
.segment "CHARS" 
  .incbin "project.chr"


