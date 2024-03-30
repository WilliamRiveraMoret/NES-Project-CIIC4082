; William Rivera Moret & Fabian Pérez Muñoz 
.segment "HEADER"
  ; .byte "NES", $1A      ; iNES header identifier
  .byte $4E, $45, $53, $1A
  .byte 2               ; 2x 16KB PRG code
  .byte 1               ; 1x  8KB CHR data
  .byte $01, $00        ; tilesper 0, vertical mirroring

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
  SEI		; disable IRQs
  CLD		; disable decimal mode
  LDX #$40
  STX $4017	; disable APU frame IRQ
  LDX #$ff 	; Set up stack
  TXS		;  .
  INX		; now X = 0
  STX $2000	; disable NMI
  STX $2001 	; disable rendering
  STX $4010 	; disable DMC IRQs

;; first wait for vblank to make sure PPU is ready
vblankwait1:
  BIT $2002
  BPL vblankwait1

clear_memory:
  LDA #$00
  STA $0000, x
  STA $0100, x
  STA $0200, x
  STA $0300, x
  STA $0400, x
  STA $0500, x
  STA $0600, x
  STA $0700, x
  INX
  BNE clear_memory

;; second wait for vblank, PPU is ready after this
vblankwait2:
  BIT $2002
  BPL vblankwait2

main:
load_palettes:
  LDA $2002 ;reads from the CPU-RAM PPU address register to reset it
  LDA #$3f  ;loads the higher byte of the PPU address register of the palettes in a (we want to write in $3f00 of the PPU since it is the address where the palettes of the PPU are stored)
  STA $2006 ;store what's in a (higher byte of PPU palettes address register $3f00) in the CPU-RAM memory location that transfers it into the PPU ($2006)
  LDA #$00  ;loads the lower byte of the PPU address register in a
  STA $2006 ;store what's in a (lower byte of PPU palettes address register $3f00) in the CPU-RAM memory location that transfers it into the PPU ($2006)
  LDX #$00  ;AFTER THIS, THE PPU-RAM GRAPHICS POINTER WILL BE POINTING TO THE MEMORY LOCATION THAT CONTAINS THE SPRITES, NOW WE NEED TO TRANSFER SPRITES FROM THE CPU-ROM TO THE PPU-RAM
            ;THE PPU-RAM POINTER GETS INCREASED AUTOMATICALLY WHENEVER WE WRITE ON IT

; NO NEED TO MODIFY THIS LOOP SUBROUTINE, IT ALWAYS LOADS THE SAME AMOUNT OF PALETTE REGISTER. TO MODIFY PALETTES, REFER TO THE PALETTE SECTION
@loop: 
  LDA palettes, x   ; as x starts at zero, it starts loading in a the first element in the palettes code section ($0f). This address mode allows us to copy elements from a tag with .data directives and the index in x
  STA $2007         ;THE PPU-RAM POINTER GETS INCREASED AUTOMATICALLY WHENEVER WE WRITE ON IT
  INX
  CPX #$20
  BNE @loop

LDX #$00
@tile_loop:
  LDA $2002 
  LDA tiles, x
  STA $2006
  INX

  LDA tiles, x
  STA $2006
  INX

  LDA tiles, x
  STA $2007
  INX

  CPX #$78
  BNE @tile_loop

enable_rendering: ; DO NOT MODIFY THIS
  LDA #%10000000	; Enable NMI
  STA $2000
  LDA #%00011000	; Enable Letters Backgroud
  STA $2001

forever: ;FOREVER LOOP WAITING FOR THEN NMI INTERRUPT, WHICH OCCURS WHENEVER THE LAST PIXEL IN THE BOTTOM RIGHT CORNER IS PROJECTED
  JMP forever

nmi:  ;WHENEVER AN NMI INTERRUPT OCCURS, THE PROGRAM JUMPS HERE (60fps)
  LDX #$00
  STX $2003
  STX $2005
@loop:
  LDA dog_sprites, x
  STA $2004
  INX
  CPX #$88
  BNE @loop
  RTI

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

tiles: 
  ; Stone Brick Wall
  .byte $22, $48, $13
  .byte $22, $49, $13
  .byte $22, $68, $13
  .byte $22, $69, $13

  ; Smooth Stone Wall:
  .byte $22, $4A, $14
  .byte $22, $4B, $2B
  .byte $22, $6A, $2C
  .byte $22, $6B, $2D

  ; Attribute
  .byte $23, $E2, %01100000

  ; Stone:
  .byte $22, $4C, $15
  .byte $22, $4D, $15
  .byte $22, $6C, $15
  .byte $22, $6D, $15

  ; Bush:
  .byte $22, $4E, $16
  .byte $22, $4F, $2E
  .byte $22, $6E, $2F
  .byte $22, $6F, $30

  ; Attribute
  .byte $23, $E3, %10010000

  ; Mossy Cobble Stone:
  .byte $22, $50, $17
  .byte $22, $51, $17
  .byte $22, $70, $17
  .byte $22, $71, $17

  ; Attribute
  .byte $23, $E4, %10100000

  ; Wood:
  .byte $22, $88, $18
  .byte $22, $89, $18
  .byte $22, $A8, $18
  .byte $22, $A9, $18

  ; Dirt:
  .byte $22, $8A, $19
  .byte $22, $8B, $19
  .byte $22, $AA, $1A
  .byte $22, $AB, $1A

  ; Attribute
  .byte $23, $EA, %00001000

  ; Fence:
  .byte $22, $8C, $1B
  .byte $22, $8D, $1B
  .byte $22, $AC, $24
  .byte $22, $AD, $24

  ; Door:
  .byte $22, $8E, $1C
  .byte $22, $8F, $1D
  .byte $22, $AE, $1E
  .byte $22, $AF, $1F

  ; Attribute
  .byte $23, $EB, %00000010

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
.incbin "sprites.chr"
