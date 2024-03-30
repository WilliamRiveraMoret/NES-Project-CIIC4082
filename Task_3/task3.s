; William Rivera Moret & Fabian Pérez Muñoz 
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
  INC frame_tick        ; Increment low byte of testloop

  @loop_static:
    LDA #$00
    STA $2004
    INY
    CPY #$08
    BNE @loop_static

  jsr player_controller
  LDA #$02
  STA sprite_direction      ; Front
  jsr update_player_frame
  jsr render_player_frame

  rti
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

.proc update_player_frame
  LDA frame_tick         ; Load the low byte
  CMP #$0F               ; Compare low byte with $0F
  BCS next               ; Branch if greater than
  jmp then

  next:
    LDA sprite_direction
    CLC
    ADC #$01
    STA sprite_direction

  then:
    LDA frame_tick        ; Load the low byte
    CMP #$1F              ; Compare it with $1F
    BNE continue          ; Branch if not equal to $1F
    LDA #$00              ; Load 0 into A
    STA frame_tick        ; Reset frame_tick to 0 if it was $1F

  continue:
    rts  ; Return from interrupt
.endproc

.proc render_player_frame
  LDA sprite_direction ; Multiplying sprite start by 16
  ASL A
  ASL A
  ASL A
  ASL A
  TAY

  CLC 
  ADC #$10
  STA sprite_end

  @loop_sprites:
    lda pos_x
    clc
    adc dog, y
    sta $2004
    iny

    lda dog, y
    sta $2004
    iny

    lda dog, y
    sta $2004
    iny

    lda pos_y
    clc
    adc dog, y
    sta $2004
    iny

    cpy sprite_end
    bne @loop_sprites
    rts
.endproc

.proc player_controller
; ; Controller Initialization -----------------------------------------------------
;  ; $4016 is an I/O Register (Memory-Mapped I/O Register). Send the Latch pulse down to the 4021. High Voltage = Data Input Mode / Low Voltage = Data Output Mode
LatchController: 
; Commence Strobing Process
  LDA #$01        ; Load a value of 1 in "A" register
  STA $4016       ; Store that value of 1 into RAM, making voltage high for controller ports. This will set the 8 bits that will have the output values of the procedure. This will contain a series of bits that will denoted if a button was pressed or not.
  LDA #$00        ; Load value of 0 in acummulator.
  STA $4016       ; Brings voltage to low on the latch pins. Tell the controllers to latch buttons

; The concept above is called 'Strobing' which initializes the controllers in an NES. Summary: Give me the buttons that the player is pressing now. Buttons are shown one at a time in bit0. If bit0 = 0, then it is not pressed, if bit0 = 1, then it is pressed.

; Read Button States ------------------------------------------------------------
  LDX #$08        ; Load 8 Button States that need to be read: A,B, Select, Start, Up, Down, Left, and Right.

ReadNextButton:
  LDA $4016       ; Read P1 Current Button
  AND #%00000001  ; Only analyze bit0. AND instruction is used to clear the other bits, since only bit0 reads the button. (Isolate LSB)
  CMP #$00 ; Check if button is pressed or not (b0 == 0), then branches to Continue if not pressed.
  BEQ Continue 
  ; Depending on the value in X, then if button is pressed, branch to each instruction
  CPX #$08
  BEQ InstrA
  CPX #$07
  BEQ InstrB
  CPX #$06
  BEQ InstrSelect
  CPX #$05
  BEQ InstrStart
  CPX #$04
  BEQ InstrUp
  CPX #$03
  BEQ InstrDown
  CPX #$02
  BEQ InstrLeft
  CPX #$01
  BEQ InstrRight
; Decrement X and loop back if not all buttons have been read
Continue:
  DEX
  CPX #$00
  BNE ReadNextButton

  ; Button Instructions ------------------------------------------------------------
InstrA:
  ; Instructions for when A-Button is pressed (b0 == 1)
  JMP Continue

InstrB:
  ; Instructions for when B-Button is pressed (b0 == 1)
  JMP Continue

InstrSelect:
  ; Instructions for when Select-Button is pressed (b0 == 1)
  JMP Continue

InstrStart:
  ; Instructions for when Start-Button is pressed (b0 == 1)
  JMP Continue

InstrUp: ; LEFT
  ; Instructions for when Up-Button is pressed (b0 == 1)
  LDA pos_x       ; Load the sprite's Y position
  SEC             ; Set Carry Flag: Subtract
  SBC #$01        ; Y = Y - 1
  STA pos_x        ; Store the sprite's new Y position
  JMP Continue

InstrDown: ; RIGHT
  ; Instructions for when Down-Button is pressed (b0 == 1)
  LDA pos_x        ; Load the sprite's Y position
  CLC             ; Clear Carry Flag: Add
  ADC #$01        ; Y = Y + 1
  STA pos_x      ; Store the sprite's new Y position
  JMP Continue

InstrLeft: ; UP
  ; Instructions for when L-Button is pressed (b0 == 1)
  LDA pos_y      ; Load the sprite's X position
  SEC             ; Set Carry Flag: Subtract
  SBC #$01        ; X = X - 1
  STA pos_y      ; Store the sprite's new X position
  JMP Continue

InstrRight: ; DOWN
  ; Instructions for when R-Button is pressed (b0 == 1)
  LDA pos_y     ; Load the sprite's X position
  CLC             ; Clear Carry Flag: Add
  ADC #$01        ; X = X + 1
  STA pos_y      ; Store the sprite's new X position
rts
.endproc

dog:  
  ; Dog Front - SPRITE 0
  .byte $00, $07, $00, $00 
  .byte $00, $08, $00, $08
  .byte $08, $09, $00, $00
  .byte $08, $09, %01000000, $08

  ; Dog Front Running - SPRITE 1
  .byte $00, $0A, $00, $00  
  .byte $00, $0B, $00, $08
  .byte $08, $0C, $00, $00
  .byte $08, $0C, %01000000, $08

  ; Dog Right - SPRITE 2
  .byte $00, $01, $00, $00
  .byte $00, $02, $00, $08
  .byte $08, $03, $00, $00
  .byte $08, $04, $00, $08

  ; Dog Right Running - SPRITE 3
  .byte $00, $01, $00, $00
  .byte $00, $02, $00, $08
  .byte $08, $05, $00, $00
  .byte $08, $06, $00, $08

  ; Dog Back - SPRITE 4
  .byte $00, $0D, $00, $00      
  .byte $00, $0D, %01000000, $08
  .byte $08, $0E, $00, $00
  .byte $08, $0F, $00, $08

  ; Dog Back Running - SPRITE 5
  .byte $00, $10, $00, $00      
  .byte $00, $10, %01000000, $08
  .byte $08, $11, $00, $00
  .byte $08, $12, $00, $08

  ; Dog Left - SPRITE 6
  .byte $00, $02, %01000000, $00 
  .byte $00, $01, %01000000, $08
  .byte $08, $04, %01000000, $00
  .byte $08, $03, %01000000, $08

  ; Dog Left Running - SPRITE 7
  .byte $00, $02, %01000000, $00 
  .byte $00, $01, %01000000, $08
  .byte $08, $06, %01000000, $00
  .byte $08, $05, %01000000, $08


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
sprite_direction: .res 1
sprite_end: .res 1
frame_tick:  .res 1
pos_x: .res 1
pos_y: .res 1