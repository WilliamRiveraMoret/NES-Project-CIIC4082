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

.proc reset_handler ; Left the same but made as a procedure
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
  LDY #$00           ; Reset index Y for looping through sprite data
  STY $2003          ; Reset OAM address
  STY $2005          ; Reset scroll registers
  LDA $02
  STA $4014  
  ; Increment and compare the 16-bit counter
  INC frame_tick     

  @loop_static:
    LDA #$00
    STA $2004
    INY
    CPY #$08
    BNE @loop_static

  ; Movement Logic --------------------------------------
  LDA #$40 
  STA pos_y
  LDA #$60
  STA pos_x
  LDA #$00 ; Sprite Index
  STA sprite_direction      ; Front
  JSR update_player_frame   ; For every 15 ticks, sprite changes from stationary to motion versions
  JSR render_player_frame   ; Display the sprite 

  LDA #$40
  STA pos_y
  LDA #$70
  STA pos_x
  LDA #$02
  STA sprite_direction      ; Right
  JSR update_player_frame
  JSR render_player_frame

  LDA #$70
  STA pos_y
  LDA #$60
  STA pos_x
  LDA #$04
  STA sprite_direction      ; Back
  JSR update_player_frame
  JSR render_player_frame

  LDA #$70
  STA pos_y
  LDA #$70
  STA pos_x
  LDA #$06
  STA sprite_direction      ; Left 
  JSR update_player_frame
  JSR render_player_frame

  RTI
.endproc

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
  LDA palettes, x   ; as x STARTS at zero, it STARTS loading in a the first element in the palettes code section ($0f). This address mode allows us to copy elements from a tag with .data directives and the index in x
  STA $2007         ;THE PPU-RAM POINTER GETS INCREASED AUTOMATICALLY WHENEVER WE WRITE ON IT
  INX
  CPX #$20
  BNE @loop

enable_rendering: ; DO NOT MODIFY THIS
  LDA #%10000000	; Enable NMI
  STA $2000
  LDA #%10011000	; Enable Letters Backgroud
  STA $2001

forever: ;FOREVER LOOP WAITING FOR THEN NMI INTERRUPT, WHICH OCCURS WHENEVER THE LAST PIXEL IN THE BOTTOM RIGHT CORNER IS PROJECTED
  JMP forever

.proc update_player_frame
  LDA frame_tick 
  CMP #$0F               ; Compare low byte with $0F
  BCS next               ; Branch if greater than
  JMP then

  next:
    LDA sprite_direction
    CLC
    ADC #$01
    STA sprite_direction

  then:
    LDA frame_tick    
    CMP #$1F              ; Compare it with $1F
    BNE continue          ; Branch if not equal to $1F
    LDA #$00              ; Load 0 into A
    STA frame_tick        ; Reset frame_tick to 0 if it was $1F

  continue:
    RTS  ; Return from interrupt
.endproc

.proc render_player_frame
  LDA sprite_direction ; Multiplying sprite start by 16
  ASL A ; Shift Left: Multiply by 2 to move to another sprite index
  ASL A
  ASL A
  ASL A
  TAY ; Transfer A value to Y value

  CLC 
  ADC #$10 ; Add 16 to know when a sprite index starts and ends
  STA sprite_end

  @loop_sprites:
    LDA pos_x ; Position in X of sprite
    CLC
    ADC dog, y ; Add offset (y) to adjust position of neighboring sprites
    STA $2004 
    INY

    LDA dog, y ; Load sprite table index 
    STA $2004
    INY

    LDA dog, y ; Load attributes 
    STA $2004
    INY

    LDA pos_y ; Position in Y of sprite
    CLC
    ADC dog, y
    STA $2004
    INY

    CPY sprite_end
    BNE @loop_sprites
    RTS
.endproc

dog:  ; each sprite has 16-bits
  ; Dog Front - SPRITE 0
  .byte $00, $07, $00, $00 ; offset x, sprite address, palette, offset y
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
