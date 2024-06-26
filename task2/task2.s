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
  LDA #$00
  STA timer
  LDA #$00
  STA playerstate
  LDA #$10
  STA player_x
  LDA #$18
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

  LDA #$10
  STA player_x
  LDA #$18
  STA player_y ;will it work???


jsr rollout

  lda player_y ;fucked fucker
  clc 
  sbc #$10
  clc
  sta player_y

  lda player_x
  clc
  adc #$10
  sta player_x

jsr rollout2

  lda player_y ;fucked fucker
  clc 
  sbc #$10
  clc
  sta player_y

  lda player_x
  clc
  adc #$10
  sta player_x

jsr rollout3

  lda player_y ;fucked fucker
  clc 
  sbc #$10
  clc
  sta player_y

  lda player_x
  clc
  adc #$10
  sta player_x

jsr rollout4

; @loop:   lda $0204, x 	; Load the hello message into SPR-RAM
;   sta $2004
;   inx
;   cpx #$90
;   bne @loop
  LDA #$00        ;;tell the ppu there is no background scrolling
  STA $2005
  STA $2005


  rti
.endproc


  

.proc rollout
  PHP
  PHA
  TXA
  PHA
  TYA
  PHA
  
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
  LDA #$02
  STA $0201
  LDA #$03
  STA $0205
  LDA #$12
  STA $0209
  LDA #$13
  STA $020d

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
  jmp startdraw1

state2:
  ldx playerstate
  cpx #$02
  bne startdraw1
  LDA #$06
  STA $0201
  LDA #$07
  STA $0205
  LDA #$16
  STA $0209
  LDA #$17
  STA $020d
  

startdraw1:
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


  lda timer
  cmp #$00
  BNE incro
  ldx #00
  stx playerstate
  incro: 
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
  LDA #$08
  STA $0211
  LDA #$09
  STA $0215
  LDA #$18
  STA $0219
  LDA #$19
  STA $021d

  ldx playerstate
  cpx #$01
  BEQ state1
  jmp state2
state1:
  LDA #$0a
  STA $0211
  LDA #$0b
  STA $0214
  LDA #$1a
  STA $0218
  LDA #$1b
  STA $021d
  jmp startdraw2

state2:
  ldx playerstate
  cpx #$02
  bne startdraw2
  LDA #$0c
  STA $0211
  LDA #$0d
  STA $0215
  LDA #$1c
  STA $0219
  LDA #$1d
  STA $021d
  

startdraw2:
  ; write player ship tile attributes
  ; use palette 1
  LDA #$01
  STA $0212
  STA $0216
  STA $021a
  STA $021e

  ; top left tile:
  LDA player_y
  CLC
  ADC #$10
  STA $0210
  LDA player_x
  CLC
  ADC #$10
  STA $0213

  ; top right tile (x + 8):
  LDA player_y
  CLC
  ADC #$10
  STA $0214
  LDA player_x
  CLC
  ADC #$18
  STA $0217

  ; bottom left tile (y + 8):
  LDA player_y
  CLC
  ADC #$18
  STA $0218
  LDA player_x
  CLC
  ADC #$10
  STA $021b

  ; bottom right tile (x + 8, y + 8)
  LDA player_y
  CLC
  ADC #$18
  STA $021c
  LDA player_x
  CLC
  ADC #$18
  STA $021f



  
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


  lda timer
  cmp #$00
  BNE incro
  ldx #00
  stx playerstate
  incro: 
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
  STA $0221
  LDA #$0f
  STA $0225
  LDA #$1e
  STA $0229
  LDA #$1f
  STA $022d

  ldx playerstate
  cpx #$01
  BEQ state1
  jmp state2
state1:
  LDA #$20
  STA $0221
  LDA #$21
  STA $0225
  LDA #$30
  STA $0229
  LDA #$31
  STA $022d
  jmp startdraw3

state2:
  ldx playerstate
  cpx #$02
  bne startdraw3
  LDA #$22
  STA $0221
  LDA #$23
  STA $0225
  LDA #$32 
  STA $0229
  LDA #$33
  STA $022d
  

startdraw3:
  ; write player ship tile attributes
  ; use palette 1
  LDA #$01
  STA $0222
  STA $0226
  STA $022a
  STA $022e

  ; top left tile:
  LDA player_y
  CLC
  ADC #$20
  STA $0220
  LDA player_x
  CLC
  ADC #$20
  STA $0223

  ; top right tile (x + 8):
  LDA player_y
  CLC
  ADC #$20
  STA $0224
  LDA player_x
  CLC
  ADC #$28
  STA $0227

  ; bottom left tile (y + 8):
  LDA player_y
  CLC
  ADC #$28
  STA $0228
  LDA player_x
  CLC
  ADC #$20
  STA $022b

  ; bottom right tile (x + 8, y + 8)
  LDA player_y
  CLC
  ADC #$28
  STA $022c
  LDA player_x
  CLC
  ADC #$28
  STA $022f



  
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


  lda timer
  cmp #$00
  BNE incro
  ldx #00
  stx playerstate
  incro: 
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
  STA $0231
  LDA #$25
  STA $0235
  LDA #$34
  STA $0239
  LDA #$35
  STA $023d

  ldx playerstate
  cpx #$01
  BEQ state1
  jmp state2
state1:
  LDA #$26
  STA $0231
  LDA #$27
  STA $0235
  LDA #$36
  STA $0239
  LDA #$37
  STA $023d
  jmp startdraw4

state2:
  ldx playerstate
  cpx #$02
  bne startdraw4
  LDA #$28
  STA $0231
  LDA #$29
  STA $0235
  LDA #$38 
  STA $0239
  LDA #$39
  STA $023d
  

startdraw4:
  ; write player ship tile attributes
  ; use palette 1
  LDA #$01
  STA $0232
  STA $0236
  STA $023a
  STA $023e

  ; top left tile:
  LDA player_y
  CLC
  ADC #$30
  STA $0230
  LDA player_x
  CLC
  ADC #$30
  STA $0233

  ; top right tile (x + 8):
  LDA player_y
  CLC
  ADC #$30
  STA $0234
  LDA player_x
  CLC
  ADC #$38
  STA $0237

  ; bottom left tile (y + 8):
  LDA player_y
  CLC
  ADC #$38
  STA $0238
  LDA player_x
  CLC
  ADC #$30
  STA $023b

  ; bottom right tile (x + 8, y + 8)
  LDA player_y
  CLC
  ADC #$38
  STA $023c
  LDA player_x
  CLC
  ADC #$38
  STA $023f



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


