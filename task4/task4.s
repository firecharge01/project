;;Sebastián Torrez Segarra 
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
  lda #%00001110	; Enable Sprites and background
  sta $2001

.segment "ZEROPAGE"
level: .res 1
index: .res 2
MY: .res 2
MX: .res 2
tile1 .res 1
tile2 .res 1
tile3 .res 1
tile4 .res 1

  LoadBackground:
  LDA $2002             ; read PPU status to reset the high/low latch
  LDA #$20
  STA $2006             ; write the high byte of $2000 address
  LDA #$00
  STA $2006             ; write the low byte of $2000 address
  LDX #$00    

LoadBackgroundLoop1:
  
  LDA x
  LSR           ;logical shift right twice = x4
  LSR
  STA MY           ;store in mega Y
  LDA x
  AND #$03          ; x AND 3 = x % 4
  STA MX            ; store in mega X
  ASL MY            ;shift left mega Y six times (x64) 
  ASL MY
  ASL MY
  ASL MY            ;SURELY MX AND MY DONT NEED TO BE INTACT RIGHT?
  ASL MY
  ASL MY
  ASL MX            ;same with mega X but three times (x8)
  ASL MX
  ASL MX
  CLC
  LDA #$00          ;reset accumulator to 0, add the mega X and Y to get real index
  ADC MX
  ADC MY
  STA index            ;store final index in "index"
  CLC
  ADC #%0010000000000000  ;hex 2000 = binary 0010000000000000 (we add this to get to 2000 + offset)
  STA index               ;change index to real index value relative to nametable
  
;first tile
  LDA PPUSTATUS     ;basic loading background form
  LDA #index
  STA PPUADDR
  LDA #index
  ;WERE GONNA ROTATE TO LEFT THIS TIME TO SEE IF WORKS, if it dont 
  ;we rotate right and fix high byte loading above
  ROL
  ROL
  ROL
  ROL
  ROL
  ROL
  ROL
  ROL
  STA PPUADDR
  LDY #$2c        ;2c is our base tile of moss wall
  STY PPUDATA

;second tile
  CLC
  LDA PPUSTATUS     ;basic loading background form
  LDA #index
  ADC $01
  STA PPUADDR
  LDA #index
  ADC $01
  ;WERE GONNA ROTATE TO LEFT THIS TIME TO SEE IF WORKS, if it dont 
  ;we rotate right and fix high byte loading above
  ROL
  ROL
  ROL
  ROL
  ROL
  ROL
  ROL
  ROL
  STA PPUADDR
  LDY #$2c        ;2c is our base tile of moss wall
  STY PPUDATA

;third tile
  CLC
  LDA PPUSTATUS     ;basic loading background form
  LDA #index
  ADC $20
  STA PPUADDR
  LDA #index
  ADC $20
  ;WERE GONNA ROTATE TO LEFT THIS TIME TO SEE IF WORKS, if it dont 
  ;we rotate right and fix high byte loading above
  ROL
  ROL
  ROL
  ROL
  ROL
  ROL
  ROL
  ROL
  STA PPUADDR
  LDY #$2c        ;2c is our base tile of moss wall
  STY PPUDATA

;fourth tile
  CLC
  LDA PPUSTATUS     ;basic loading background form
  LDA #index
  ADC $21
  STA PPUADDR
  LDA #index
  ADC $21
  ;WERE GONNA ROTATE TO LEFT THIS TIME TO SEE IF WORKS, if it dont 
  ;we rotate right and fix high byte loading above
  ROL
  ROL
  ROL
  ROL
  ROL
  ROL
  ROL
  ROL
  STA PPUADDR
  LDY #$2c        ;2c is our base tile of moss wall
  STY PPUDATA


  ;TODO load correct data with index in places index, index+1, index+32, index+33

  ;LDA background, x     ; load data from address (background + the value in x)
                        ;may need to find another way to load data cuz compression
  ;STA $2007             ; write to PPU
  

  INX                   ; X = X + 1
  CPX #$3c                 ; Compare X to hex $3c, decimal 60 - copying 60 bytes
  BNE LoadBackgroundLoop1  ; Branch to LoadBackgroundLoop if compare was Not Equal to zero
                        ; if compare was equal to 60, keep going down
forever:
  jmp forever



nmi:
  ldx #$00 	; Set SPR-RAM address to 0
  stx $2003
@loop:	lda hello, x 	; Load the hello message into SPR-RAM
  sta $2004
  inx
  cpx #$02
  bne @loop
  LDA #$00        ;;tell the ppu there is no background scrolling
 

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

LoadAttribute:
  LDA $2002             ; read PPU status to reset the high/low latch
  LDA #$23
  STA $2006             ; write the high byte of $23C0 address
  LDA #$c0
  STA $2006             ; write the low byte of $23C0 address
  LDX #$00              ; start out at 0
LoadAttributeLoop:
  LDA attribute, x      ; load data from address (attribute + the value in x)
  STA $2007             ; write to PPU
  INX                   ; X = X + 1
  CPX #$10              ; Compare X to hex $08, decimal 8 - copying 8 bytes
  BNE LoadAttributeLoop

  rti


hello:

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



attribute:
  .byte %00100100, %00000101, %00000101, %00000101, %0000101, %00000101, %00001101, %00001111, %00000000, %00000000

; Character memory
.segment "CHARS"
 .incbin "project.chr"


