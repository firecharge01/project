;;Sebastián Torrez Segarra 
;;Cruz Diaz Rivera
.segment "HEADER"
  ; .byte "NES", $1A      ; iNES header identifier
  .byte $4E, $45, $53, $1A
  .byte 2               ; 2x 16KB PRG code
  .byte 1               ; 1x  8KB CHR data
  .byte $01, $00        ; mapper 0, vertical mirroring

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
  LDA #$00
  STA player_dir
  LDA #$00
  STA playerstate
  LDA #$00
  STA player_x
  LDA #$00
  STA player_y

; Main code segment for the program
.segment "CODE"





reset:
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

;; first wait for vblank to make sure PPU is ready
PPUCTRL   = $2000
PPUMASK   = $2001
PPUSTATUS = $2002
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

vblankwait1:
  bit $2002
  bpl vblankwait1

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

;; second wait for vblank, PPU is ready after this





main:
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

enable_rendering:
  lda #%10000000	; Enable NMI
  sta $2000
  lda #%00011110	; Enable Sprites and background
  sta $2001

forever:
  jmp forever


.proc nmi
nmi:
  ldx #$00 	; Set SPR-RAM address to 0
  stx $2003
  ldx #$02
  stx $4014

jsr read_controller1
jsr update_player

; lda pad1
; and #BTN_RIGHT
; bne left
; ldx #$00
; stx player_dir
; LDA player_x
; CLC
; adc #$01
; sta player_x
; left:
; lda pad1
; and #BTN_LEFT
; beq up
; LDX #$01
; stx player_dir
; LDA player_x
; CLC
; sbc #$01
; sta player_x
; up:
; lda pad1
; and #BTN_UP
; bne down
; ldx #$02
; stx player_dir
; LDA player_y
; CLC
; adc #$01
; sta player_y
; down:
; lda pad1
; and #BTN_DOWN
; beq sprite
; ldx #$03
; stx player_dir
; LDA player_y
; CLC
; sbc #$01
; sta player_y



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
; @loop:   lda $0204, x 	; Load the hello message into SPR-RAM
;   sta $2004
;   inx
;   cpx #$90
;   bne @loop
  LDA #$00        ;;tell the ppu there is no background scrolling
  STA $2005
  STA $2005


;   LoadBackground:
;   LDA $2002             ; read PPU status to reset the high/low latch
;   LDA #$20
;   STA $2006             ; write the high byte of $2000 address
;   LDA #$00
;   STA $2006             ; write the low byte of $2000 address
;   LDX #$00              ; start out at 0
; LoadBackgroundLoop:
;   LDA background, x     ; load data from address (background + the value in x)
;   STA $2007             ; write to PPU
;   INX                   ; X = X + 1
;   CPX #$6a                 ; Compare X to hex $80, decimal 128 - copying 128 bytes
;   BNE LoadBackgroundLoop  ; Branch to LoadBackgroundLoop if compare was Not Equal to zero
;                         ; if compare was equal to 128, keep going down

; LoadAttribute:
;   LDA $2002             ; read PPU status to reset the high/low latch
;   LDA #$23
;   STA $2006             ; write the high byte of $23C0 address
;   LDA #$c0
;   STA $2006             ; write the low byte of $23C0 address
;   LDX #$00              ; start out at 0
; LoadAttributeLoop:
;   LDA attribute, x      ; load data from address (attribute + the value in x)
;   STA $2007             ; write to PPU
;   INX                   ; X = X + 1
;   CPX #$10              ; Compare X to hex $08, decimal 8 - copying 8 bytes
;   BNE LoadAttributeLoop

  rti
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
  ldx #$00
  stx move 
  LDA pad1        ; Load button presses
  AND #BTN_LEFT   ; Filter out all but Left
  BEQ check_right 
  ldx #$01
  stx player_dir; If result is zero, left not pressed
  DEC player_x  ; If the branch is not taken, move player left
check_right:
  LDA pad1
  AND #BTN_RIGHT
  BEQ check_up
  ldx #$00
  stx player_dir
  INC player_x
check_up:
  LDA pad1
  AND #BTN_UP
  BEQ check_down
  ldx #$02
  stx player_dir
  DEC player_y
check_down:
  LDA pad1
  AND #BTN_DOWN
  BEQ done_checking
  ldx #$03
  stx player_dir
  INC player_y
done_checking:
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
  LDA #$01
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
  STA $0204
  LDA #$1a
  STA $0208
  LDA #$1b
  STA $020d
  
startdraw:
  ; write player ship tile attributes
  ; use palette 1
  LDA #$01
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
  LDA #$01
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
  LDA #$01
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

palettes:
  ; Background Palette
  .byte $0f, $37, $28, $30
  .byte $0f, $38, $2D, $3D
  .byte $0f, $09, $1a, $24
  .byte $0f, $2d, $1a, $0C

  ; Sprite Palette
  .byte $0f, $37, $28, $30
  .byte $0f, $38, $2D, $3D
  .byte $0f, $09, $1a, $24
  .byte $0f, $2d, $1a, $0C

background:
.byte $00,$01,$02,$03,$04,$05,$06,$07,$08,$09,$0a,$0b,$0c,$0d,$0e,$0f
.byte $20,$21,$22,$23,$24,$25,$26,$27,$28,$29,$2a,$2b,$2c,$2d,$2E,$2f
.byte $10,$11,$12,$13,$14,$15,$16,$17,$18,$19,$1a,$1b,$1c,$1d,$1E,$1f
.byte $30,$31,$32,$33,$34,$35,$36,$37,$38,$39,$3a,$3b,$3c,$3d,$3E,$3f
.byte $40,$41,$42,$43,$44,$45,$46,$47,$48,$49,$4a,$4b,$4c,$4d,$4E,$4f
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $50,$51,$52,$53,$54,$55,$56,$57,$58,$59,$5a,$00,$00,$00,$00,$00



attribute:
  .byte %00100100, %00000101, %00000101, %00000101, %0000101, %00000101, %00001101, %00001111, %00000000, %00000000

; Character memory
.segment "CHARS"
 .incbin "project.chr"


