;;Cruz Y. Diaz Rivera
;;Sebastián Torrez Segarra 
.segment "HEADER"
  ; .byte "NES", $1A      ; iNES header identifier
  .byte $4E, $45, $53, $1A
  .byte 2               ; 2x 16KB PRG code
  .byte 1               ; 1x  8KB CHR data
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
level: .res 1
leveloffset: .res 1
index: .res 2
MY: .res 1
MX: .res 1
addrhigh: .res 1
addrlow: .res 1
tile: .res 1
tileoffset: .res 1
compressread: .res 1
compress: .res 1
storeX: .res 1

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



  LoadBackground:
  LDA $2002             ; read PPU status to reset the high/low latch
  LDA #$20              ; we HAVE TO VERIFY IF ITS BG 2000 OR 2400!!!!!!!!!!!!!!!!!!!!!!!!!
  STA addrhigh
  STA $2006             ; write the high byte of $2000 address
  LDA #$00
  STA addrlow
  STA $2006             ; write the low byte of $2000 address
  LDX #$00    

LoadBackgroundLoop1:


  STX storeX
  LDA storeX
  STA MY
  LDA MY
  LSR MY
  LSR MY
  ASL MY
  ASL MY
  ASL MY
  ASL MY
  ASL MY
  ASL MY

  LDA storeX
  STA MX
  LDA MX
  AND #$03
  STA MX
  ASL MX
  ASL MX
  ASL MX
  CLC
  LDA #$00
  ADC MY
  ADC MX
  STA index


  
  ;maybe make condition here to choose which tiles to load up
  LDA level
  CMP #$00
  BNE loadupbg2

  LDA background, x
  STA compressread
  LDA #$00
  STA tileoffset
  JSR tileset1
  asl compressread
  asl compressread
  LDA #$02
  STA tileoffset
  JSR tileset1
  asl compressread
  asl compressread
  LDA #$04
  STA tileoffset
  JSR tileset1
  asl compressread
  asl compressread
  LDA #$06
  STA tileoffset
  JSR tileset1

  STX storeX
  LDA storeX
  CMP #$3b
  BEQ alreadygotbg

  LDA index
  CMP #$D8
  BNE cont

  LDA #$00
  STA storeX
  LDA #$00
  STA index

  lup:
  INX 
  JMP LoadBackgroundLoop1
  cont:
  INC storeX
  LDA #$10
  CMP storeX
  BNE lup



  loadupbg2:
  LDA background2, x
  STA compressread
  LDA #$00
  STA tileoffset
  JSR tileset2
  asl compressread
  asl compressread
  LDA #$02
  STA tileoffset
  JSR tileset2
  asl compressread
  asl compressread
  LDA #$04
  STA tileoffset
  JSR tileset2
  asl compressread
  asl compressread
  LDA #$06
  STA tileoffset
  JSR tileset2

alreadygotbg:

  ; INX                   ; X = X + 1
  ; CPX #$3d                 ; Compare X to hex $3c, decimal 60 - copying 60 bytes
  ; BNE bigjump
  ; jmp forever
  ; bigjump:
  ; jmp LoadBackgroundLoop1  ; Branch to LoadBackgroundLoop if compare was Not Equal to zero
  ;                       ; if compare was equal to 60, keep going down
forever:
  jmp forever


.proc drawtiles
;first tile
  LDA PPUSTATUS     ;basic loading background form
  LDA addrhigh
  STA PPUADDR
  LDA addrlow
  ADC tileoffset     ; tileoffset makes it so we know what megatile we are drawing
  STA PPUADDR
  LDA tile        ;2c is our base tile of moss wall
  STA PPUDATA

;second tile
  CLC
  LDA PPUSTATUS     ;basic loading background form
  LDA addrhigh
  ; ADC #$01
  STA PPUADDR
  LDA addrlow
  ADC #$01
  ADC tileoffset
  STA PPUADDR
  LDA tile        ;2c is our base tile of moss wall
  CLC
  ADC #$01
  STA PPUDATA

;third tile
  CLC
  LDA PPUSTATUS     ;basic loading background form
  LDA addrhigh
  ; ADC #$20
  STA PPUADDR
  LDA addrlow
  ADC #$20
  ADC tileoffset
  STA PPUADDR
  LDA tile        ;2c is our base tile of moss wall
  CLC
  ADC #$10
  STA PPUDATA

;fourth tile
  CLC
  LDA PPUSTATUS     ;basic loading background form
  LDA addrhigh
  ; ADC #$21
  STA PPUADDR
  LDA addrlow
  ADC #$21
  ADC tileoffset
  STA PPUADDR
  LDA tile        ;2c is our base tile of moss wall
  CLC
  ADC #$11
  STA PPUDATA

  RTS
.endproc

.proc checkfatass
  CLC
  STX storeX
  LDA storeX
  SBC #$10
  BEQ fat
  LDA storeX
  SBC #$20
  BEQ fat
  LDA storeX
  SBC #$30
  BEQ fat
  jmp end
  fat:
  LDA addrhigh
  ADC #$01
  STA addrhigh  ; add 1 to the high bit of bg
  LDA #$00    
  STA addrlow ; make the low bit of bg 0 because the higher one incremented already
  end:
  RTS
.endproc

.proc tileset1
  PHP
	PHA
	TXA
	PHA
	TYA
	PHA
  ; remember: moss = 01 (2c), wall = 10 (2e), inv = 00 (00), vines = 11 (40)
  LDA compressread
  AND #%11000000
  STA compress

  ;check
  LDA compress
  CMP #%11000000 ; check if invis
  BNE wall
  JMP DRAW1 
  wall:
  LDA compress
  CMP #%10000000
  BNE moss
  JMP DRAW2 
  moss:
  LDA compress
  CMP #%01000000
  BNE INVIS
  JMP DRAW3
  INVIS:
  LDA PPUSTATUS
  LDA #$00
  STA tile
  JMP PLACE

  DRAW1:
  LDA PPUSTATUS 
  LDA #$40
  STA tile
  jmp PLACE
  DRAW2:
  LDA PPUSTATUS 
  LDA #$2e
  STA tile
  jmp PLACE
  DRAW3:
  LDA PPUSTATUS 
  LDA #$2c
  STA tile
  jmp PLACE

  PLACE: 
      LDA PPUSTATUS
      LDA MY
      STA PPUADDR
      clc
      LDA MX
      adc index
      adc tileoffset
      STA PPUADDR
      LDA tile
      STA PPUDATA

      ;2
      clc
      LDA PPUSTATUS
      LDA MY

      STA PPUADDR
      clc
      LDA MX
      
      adc index
      adc tileoffset
      adc #$01
      STA PPUADDR
      LDA tile
      CLC
      ADC #$01
      STA PPUDATA

    ;3
      CLC
      LDA PPUSTATUS     ;basic loading background form
      LDA MY
 
      STA PPUADDR
      clc
      LDA MX
      clc
      adc index
      adc tileoffset
      ADC #$20
      STA PPUADDR
      LDA tile        
      CLC
      ADC #$10
      STA PPUDATA


      ;4
      CLC
      LDA PPUSTATUS     ;basic loading background form
      LDA MY
      ;ADC #$21
      STA PPUADDR
      clc
      LDA MX
      clc
      adc index
      adc tileoffset
      ADC #$21
      STA PPUADDR
      LDA tile
      CLC
      ADC #$11
      STA PPUDATA
  PLA
	TAY
	PLA
	TAX
	PLA
	PLP
  RTS
.endproc

.proc tileset2
  ; remember: limestone = 01 (44), sandy wall = 10 (46), inv = 00 (00), sand = 11 (48)
  LDA compressread
  AND #%00000011
  CMP #%00000000 ; check if invis
  BNE sandwall
  LDA #$00
  STA tile
  JSR drawtiles
  jmp end
  sandwall:
  LDA compressread
  AND #%00000011
  CMP #%00000010
  BNE lime
  LDA #$46
  STA tile
  JSR drawtiles
  jmp end
  lime:
  LDA compressread
  AND #%00000011
  CMP #%00000001
  BNE sand
  LDA #$44
  STA tile
  JSR drawtiles
  jmp end
  sand:
  LDA #$48
  STA tile
  JSR drawtiles
  end:
  RTS
.endproc
  ;TODO load correct data with index in places index, index+1, index+32, index+33

  ;LDA background, x     ; load data from address (background + the value in x)
                        ;may need to find another way to load data cuz compression
  ;STA $2007             ; write to PPU
  





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
  CPX #$3c              ; Compare X to hex $08, decimal 8 - copying 8 bytes
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



attribute:
  .byte %00000000, %00000000, %00000000, %00000000, %0000000, %00000000, %00000000, %00000000, %00000000, %00000000

; Character memory
.segment "CHARS"
 .incbin "project.chr"


