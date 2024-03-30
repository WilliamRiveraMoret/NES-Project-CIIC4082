; William Rivera Moret & Fabian ... 
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
vblankwait2:
  bit $2002
  bpl vblankwait2

main:
load_palettes:
  lda $2002 ;reads from the CPU-RAM PPU address register to reset it
  lda #$3f  ;loads the higher byte of the PPU address register of the palettes in a (we want to write in $3f00 of the PPU since it is the address where the palettes of the PPU are stored)
  sta $2006 ;store what's in a (higher byte of PPU palettes address register $3f00) in the CPU-RAM memory location that transfers it into the PPU ($2006)
  lda #$00  ;loads the lower byte of the PPU address register in a
  sta $2006 ;store what's in a (lower byte of PPU palettes address register $3f00) in the CPU-RAM memory location that transfers it into the PPU ($2006)
  ldx #$00  ;AFTER THIS, THE PPU-RAM GRAPHICS POINTER WILL BE POINTING TO THE MEMORY LOCATION THAT CONTAINS THE SPRITES, NOW WE NEED TO TRANSFER SPRITES FROM THE CPU-ROM TO THE PPU-RAM
            ;THE PPU-RAM POINTER GETS INCREASED AUTOMATICALLY WHENEVER WE WRITE ON IT

; NO NEED TO MODIFY THIS LOOP SUBROUTINE, IT ALWAYS LOADS THE SAME AMOUNT OF PALETTE REGISTER. TO MODIFY PALETTES, REFER TO THE PALETTE SECTION
@loop: 
  lda palettes, x   ; as x starts at zero, it starts loading in a the first element in the palettes code section ($0f). This address mode allows us to copy elements from a tag with .data directives and the index in x
  sta $2007         ;THE PPU-RAM POINTER GETS INCREASED AUTOMATICALLY WHENEVER WE WRITE ON IT
  inx
  cpx #$20
  bne @loop

; Stone Brick Wall:
  lda $2002 
  lda #$22
  sta $2006
  lda #$48
  sta $2006
  ldx #$13
  stx $2007 

  lda $2002
  lda #$22
  sta $2006
  lda #$49
  sta $2006
  ldx #$13
  stx $2007 

  lda $2002
  lda #$22
  sta $2006
  lda #$68
  sta $2006
  ldx #$13
  stx $2007 

  lda $2002
  lda #$22
  sta $2006
  lda #$69
  sta $2006
  ldx #$13
  stx $2007

; Smooth Stone Wall:
  lda $2002 
  lda #$22
  sta $2006
  lda #$4A
  sta $2006
  ldx #$14
  stx $2007 

  lda $2002
  lda #$22
  sta $2006
  lda #$4B
  sta $2006
  ldx #$2B
  stx $2007 

  lda $2002
  lda #$22
  sta $2006
  lda #$6A
  sta $2006
  ldx #$2C
  stx $2007 

  lda $2002
  lda #$22
  sta $2006
  lda #$6B
  sta $2006
  ldx #$2D
  stx $2007

  lda $2002 ; Attribute
  lda #$23
  sta $2006
  lda #$E2
  sta $2006
  ldx #%01100000
  stx $2007 

; Stone:
  lda $2002 
  lda #$22
  sta $2006
  lda #$4C
  sta $2006
  ldx #$15
  stx $2007 

  lda $2002
  lda #$22
  sta $2006
  lda #$4D
  sta $2006
  ldx #$15
  stx $2007 

  lda $2002
  lda #$22
  sta $2006
  lda #$6C
  sta $2006
  ldx #$15
  stx $2007 

  lda $2002
  lda #$22
  sta $2006
  lda #$6D
  sta $2006
  ldx #$15
  stx $2007

; Bush:
  lda $2002 
  lda #$22
  sta $2006
  lda #$4E
  sta $2006
  ldx #$16
  stx $2007 

  lda $2002
  lda #$22
  sta $2006
  lda #$4F
  sta $2006
  ldx #$2E
  stx $2007 

  lda $2002
  lda #$22
  sta $2006
  lda #$6E
  sta $2006
  ldx #$2F
  stx $2007 

  lda $2002
  lda #$22
  sta $2006
  lda #$6F
  sta $2006
  ldx #$30
  stx $2007

  lda $2002 ; Attribute
  lda #$23
  sta $2006
  lda #$E3
  sta $2006
  ldx #%10010000
  stx $2007 

; Mossy Cobble Stone:
  lda $2002 
  lda #$22
  sta $2006
  lda #$50
  sta $2006
  ldx #$17
  stx $2007 

  lda $2002
  lda #$22
  sta $2006
  lda #$51
  sta $2006
  ldx #$17
  stx $2007 

  lda $2002
  lda #$22
  sta $2006
  lda #$70
  sta $2006
  ldx #$17
  stx $2007 

  lda $2002
  lda #$22
  sta $2006
  lda #$71
  sta $2006
  ldx #$17
  stx $2007

  lda $2002 ; Attribute
  lda #$23
  sta $2006
  lda #$E4
  sta $2006
  ldx #%10100000
  stx $2007 

; Wood:
  lda $2002 
  lda #$22
  sta $2006
  lda #$88
  sta $2006
  ldx #$18
  stx $2007 

  lda $2002
  lda #$22
  sta $2006
  lda #$89
  sta $2006
  ldx #$18
  stx $2007 

  lda $2002
  lda #$22
  sta $2006
  lda #$A8
  sta $2006
  ldx #$18
  stx $2007 

  lda $2002
  lda #$22
  sta $2006
  lda #$A9
  sta $2006
  ldx #$18
  stx $2007

; Dirt:
  lda $2002 
  lda #$22
  sta $2006
  lda #$8A
  sta $2006
  ldx #$19
  stx $2007 

  lda $2002
  lda #$22
  sta $2006
  lda #$8B
  sta $2006
  ldx #$19
  stx $2007 

  lda $2002
  lda #$22
  sta $2006
  lda #$AA
  sta $2006
  ldx #$1A
  stx $2007 

  lda $2002
  lda #$22
  sta $2006
  lda #$AB
  sta $2006
  ldx #$1A
  stx $2007

  lda $2002 ; Attribute
  lda #$23
  sta $2006
  lda #$EA
  sta $2006
  ldx #%00001000
  stx $2007 

; Fence:
  lda $2002 
  lda #$22
  sta $2006
  lda #$8C
  sta $2006
  ldx #$1B
  stx $2007 

  lda $2002
  lda #$22
  sta $2006
  lda #$8D
  sta $2006
  ldx #$1B
  stx $2007 

  lda $2002
  lda #$22
  sta $2006
  lda #$AC
  sta $2006
  ldx #$24
  stx $2007 

  lda $2002
  lda #$22
  sta $2006
  lda #$AD
  sta $2006
  ldx #$24
  stx $2007

; Door:
  lda $2002 
  lda #$22
  sta $2006
  lda #$8E
  sta $2006
  ldx #$1C
  stx $2007 

  lda $2002
  lda #$22
  sta $2006
  lda #$8F
  sta $2006
  ldx #$1D
  stx $2007 

  lda $2002
  lda #$22
  sta $2006
  lda #$AE
  sta $2006
  ldx #$1E
  stx $2007 

  lda $2002
  lda #$22
  sta $2006
  lda #$AF
  sta $2006
  ldx #$1F
  stx $2007

  lda $2002 ; Attribute
  lda #$23
  sta $2006
  lda #$EB
  sta $2006
  ldx #%00000010
  stx $2007 

enable_rendering: ; DO NOT MODIFY THIS
  lda #%10000000	; Enable NMI
  sta $2000
  lda #%00011000	; Enable Letters Backgroud
  sta $2001

forever: ;FOREVER LOOP WAITING FOR THEN NMI INTERRUPT, WHICH OCCURS WHENEVER THE LAST PIXEL IN THE BOTTOM RIGHT CORNER IS PROJECTED
  jmp forever

nmi:  ;WHENEVER AN NMI INTERRUPT OCCURS, THE PROGRAM JUMPS HERE (60fps)
  ldx #$00
  stx $2003
  stx $2005
@loop:
  lda dog_sprites, x
  sta $2004
  inx
  cpx #$88
  bne @loop
  rti

dog_sprites:
  .byte $00, $00, $00, $00  ; Do Not Modify
  .byte $00, $00, $00, $00  ; Do Not Modify

  ; dog_back_side:
  .byte $60, $0D, $00, $40
  .byte $60, $0D, %01000000, $48
  .byte $68, $0E, $00, $40
  .byte $68, $0F, $00, $48

  ; dog_back_side_running:
  .byte $60, $10, $00, $58
  .byte $60, $10, %01000000, $60
  .byte $68, $11, $00, $58
  .byte $68, $12, $00, $60

  ; dog_left_side:
  .byte $60, $02, %01000000, $70
  .byte $60, $01, %01000000, $78
  .byte $68, $04, %01000000, $70
  .byte $68, $03, %01000000, $78

  ; dog_left_side_running:
  .byte $60, $02, %01000000, $88
  .byte $60, $01, %01000000, $90
  .byte $68, $06, %01000000, $88
  .byte $68, $05, %01000000, $90

  ; dog_rigth_side:
  .byte $70, $01, $00, $40
  .byte $70, $02, $00, $48
  .byte $78, $03, $00, $40
  .byte $78, $04, $00, $48

  ; dog_rigth_side_running:
  .byte $70, $01, $00, $58
  .byte $70, $02, $00, $60
  .byte $78, $05, $00, $58
  .byte $78, $06, $00, $60

  ; dog_front_side:
  .byte $70, $07, $00, $70
  .byte $70, $08, $00, $78
  .byte $78, $09, $00, $70
  .byte $78, $09, %01000000, $78

  ; dog_front_side_running:
  .byte $70, $0A, $00, $88
  .byte $70, $0B, $00, $90
  .byte $78, $0C, $00, $88
  .byte $78, $0C, %01000000, $90

palettes: ;The first color should always be the same accross all the palettes. MOdify this section to determine which colors you'd like to use
  ; Background Palette % all black and gray
  .byte $0f, $27, $17, $07  ; Palette 00
  .byte $0f, $3D, $2D, $30  ; Palette 01
  .byte $0f, $06, $0B, $00  ; Palette 02
  .byte $0f, $27, $16, $07  ; Palette 03

  ; Sprite Palette
  .byte $0f, $27, $17, $07  ; Palette 00
  .byte $0f, $3D, $2D, $30  ; Palette 01
  .byte $0f, $06, $0B, $00  ; Palette 02
  .byte $0f, $27, $16, $07  ; Palette 03

; Character memory
.segment "CHARS"
.incbin "sprites_nes.chr"
