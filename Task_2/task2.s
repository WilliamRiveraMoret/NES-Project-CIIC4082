; William Rivera Moret & Fabian ... 
.segment "HEADER"
  ; .byte "NES", $1A      ; iNES header identifier
  .byte $4E, $45, $53, $1A
  .byte 2               ; 2x 16KB PRG code
  .byte 1               ; 1x  8KB CHR data
  .byte $01, $00        ; mapper 0, vertical mirroring

; "nes" linker config requires a STARTUP section, even if it's empty
.segment "STARTUP"

; Main code segment for the program
.segment "CODE"
.proc irq_handler
   RTI
.endproc

.proc reset_handler
    SEI                 ; Set Interrupt ignore bit, thus anything that would trigger an IRQ event does nothing instead
    CLD                 ; Clear Decimal mode bit
    LDX #$40    
    STX $4017
    LDX #$FF
    TXS
    INX                 ; X = $00
    STX $2000           ; PPUCTRL = 0, we turn off NMIs
    STX $2001           ; PPUMASK = 0, disable rendering to the screen during startup
    STX $4010           ; 4010 = 0, turns off DMC IRQs, to ensure that we don’t draw random garbage to the screen
    BIT $2002
vblankwait:             ; fetches the PPU’s status from PPUSTATUS, until PPU is ready
    BIT $2002
    BPL vblankwait
vblankwait2:
    BIT $2002
    BPL vblankwait2
    JMP main
.endproc
 
.proc nmi_handler  ; Define the NMI handler procedure
  ldy #$00           ; Reset index Y for looping through sprite data
  sty $2003          ; Reset OAM address
  sty $2005          ; Reset scroll registers
  lda $02
  sta $4014

; Increment and compare the 16-bit counter
  INC testloopLow        ; Increment low byte of testloop
  LDA testloopLow         ; Load the low byte
  CMP #$0F                ; Compare low byte with $FF
  BCS next             ; If equal, branch to 'isEqual'

  LDA #<dog_front_side_running  ; Load the low byte of the address
  STA spriteDataPtr
  LDA #>dog_front_side_running  ; Load the high byte of the address
  STA spriteDataPtr+1
  jsr draw_player

  LDA #<dog_left_side_running  ; Load the low byte of the address
  STA spriteDataPtr
  LDA #>dog_left_side_running  ; Load the high byte of the address
  STA spriteDataPtr+1
  jsr draw_player

  LDA #<dog_back_side_running  ; Load the low byte of the address
  STA spriteDataPtr
  LDA #>dog_back_side_running  ; Load the high byte of the address
  STA spriteDataPtr+1
  jsr draw_player

  LDA #<dog_right_side_running  ; Load the low byte of the address
  STA spriteDataPtr
  LDA #>dog_right_side_running  ; Load the high byte of the address
  STA spriteDataPtr+1
  jsr draw_player

  jmp then

  next:
    LDA #<dog_front_side  ; Load the low byte of the address
    STA spriteDataPtr
    LDA #>dog_front_side  ; Load the high byte of the address
    STA spriteDataPtr+1
    jsr draw_player

    LDA #<dog_left_side  ; Load the low byte of the address
    STA spriteDataPtr
    LDA #>dog_left_side  ; Load the high byte of the address
    STA spriteDataPtr+1
    jsr draw_player

    LDA #<dog_back_side  ; Load the low byte of the address
    STA spriteDataPtr
    LDA #>dog_back_side  ; Load the high byte of the address
    STA spriteDataPtr+1
    jsr draw_player

    LDA #<dog_right_side  ; Load the low byte of the address
    STA spriteDataPtr
    LDA #>dog_right_side  ; Load the high byte of the address
    STA spriteDataPtr+1
    jsr draw_player

  then:
    LDA testloopLow        ; Load the low byte
    CMP #$1F               ; Compare it with $7E
    BNE continue           ; Branch if not equal to $7E
    LDA #$00               ; Load 0 into A
    STA testloopLow        ; Reset testloopLow to 0 if it was $7E

  continue:
    rti  ; Return from interrupt
.endproc

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

enable_rendering: ; DO NOT MODIFY THIS
  lda #%10000000	; Enable NMI
  sta $2000
  lda #%10011000	; Enable Letters Backgroud
  sta $2001

forever: ;FOREVER LOOP WAITING FOR THEN NMI INTERRUPT, WHICH OCCURS WHENEVER THE LAST PIXEL IN THE BOTTOM RIGHT CORNER IS PROJECTED
  jmp forever

draw_player:
  LDY #$00 
  @loop_static:
    lda (spriteDataPtr), y
    sta $2004
    iny
    cpy #$18         ; 4 sprites * 4 bytes per sprite
    bne @loop_static
    rts

dog_front_side:
  .byte $00, $00, $00, $00  ; Do Not Modify
  .byte $00, $00, $00, $00  ; Do Not Modify
  .byte $70, $07, $00, $70
  .byte $70, $08, $00, $78
  .byte $78, $09, $00, $70
  .byte $78, $09, %01000000, $78

dog_front_side_running:
  .byte $00, $00, $00, $00  ; Do Not Modify
  .byte $00, $00, $00, $00 
  .byte $70, $0A, $00, $70
  .byte $70, $0B, $00, $78
  .byte $78, $0C, $00, $70
  .byte $78, $0C, %01000000, $78

dog_left_side:
  .byte $00, $00, $00, $00  ; Do Not Modify
  .byte $00, $00, $00, $00  ; Do Not Modify
  .byte $60, $02, %01000000, $70
  .byte $60, $01, %01000000, $78
  .byte $68, $04, %01000000, $70
  .byte $68, $03, %01000000, $78

dog_left_side_running:
  .byte $00, $00, $00, $00  ; Do Not Modify
  .byte $00, $00, $00, $00  ; Do Not Modify
  .byte $60, $02, %01000000, $70
  .byte $60, $01, %01000000, $78
  .byte $68, $06, %01000000, $70
  .byte $68, $05, %01000000, $78

dog_back_side:
  .byte $00, $00, $00, $00  ; Do Not Modify
  .byte $00, $00, $00, $00  ; Do Not Modify
  .byte $60, $0D, $00, $40
  .byte $60, $0D, %01000000, $48
  .byte $68, $0E, $00, $40
  .byte $68, $0F, $00, $48

dog_back_side_running:
  .byte $00, $00, $00, $00  ; Do Not Modify
  .byte $00, $00, $00, $00  ; Do Not Modify
  .byte $60, $10, $00, $40
  .byte $60, $10, %01000000, $48
  .byte $68, $11, $00, $40
  .byte $68, $12, $00, $48

dog_right_side:
  .byte $00, $00, $00, $00  ; Do Not Modify
  .byte $00, $00, $00, $00  ; Do Not Modify
  .byte $70, $01, $00, $40
  .byte $70, $02, $00, $48
  .byte $78, $03, $00, $40
  .byte $78, $04, $00, $48

dog_right_side_running:
  .byte $00, $00, $00, $00  ; Do Not Modify
  .byte $00, $00, $00, $00  ; Do Not Modify
  .byte $70, $01, $00, $40
  .byte $70, $02, $00, $48
  .byte $78, $05, $00, $40
  .byte $78, $06, $00, $48


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
 
.segment "VECTORS"
.addr nmi_handler, reset_handler,irq_handler

.segment "ZEROPAGE"
spriteDataPtr: .res 2
testloopLow:  .res 1

