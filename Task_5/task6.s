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

    LDX #$00
    STX map_index
    JSR draw_background_subroutine

    LDX #$04
    STX map_index
    JSR draw_background_subroutine

    JSR background_attributes

    LDX #$10
    STX pos_x
    STX pos_y

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
  ; STY $2005          ; Reset scroll registers
  LDA $02
  STA $4014
  LDA #$00

  INC frame_tick

  @loop_static:
    LDA #$00
    STA $2004
    INY
    CPY #$08
    BNE @loop_static

  JSR collision_handler
  JSR player_controller
  JSR update_player_frame
  JSR render_player_frame

  JSR scroll_handler
  LDA scroll ; then Y scroll
  STA $2005
  LDA #$00 ; X scroll first
  STA $2005
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
  LDA palettes, x   ; as x starts at zero, it starts loading in a the first element in the palettes code section ($0f). This address mode allows us to copy elements from a tag with .data directives and the index in x
  STA $2007         ;THE PPU-RAM POINTER GETS INCREASED AUTOMATICALLY WHENEVER WE WRITE ON IT
  INX
  CPX #$20
  BNE @loop

enable_rendering: ; DO NOT MODIFY THIS
  LDA #%10000000	; Enable NMI
  STA $2000
  LDA #%10011110	; Enable Letters Backgroud
  STA $2001

forever: ;FOREVER LOOP WAITING FOR THEN NMI INTERRUPT, WHICH OCCURS WHENEVER THE LAST PIXEL IN THE BOTTOM RIGHT CORNER IS PROJECTED
  JMP forever

.proc update_player_frame
  LDA frame_tick         ; Load the low byte
  CMP #$0F               ; Compare low byte with $0F
  BCS next               ; Branch if greater than
  JMP then

  next:
    LDA sprite_direction
    CLC
    ADC player_moving_flag
    STA sprite_direction

  then:
    LDA frame_tick        ; Load the low byte
    CMP #$1F              ; Compare it with $1F
    BNE continue          ; Branch if not equal to $1F
    LDA #$00              ; Load 0 into A
    STA frame_tick        ; Reset frame_tick to 0 if it was $1F

  continue:
    RTS  ; Return from interrupt
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
    LDA pos_x
    CLC
    ADC dog, y
    STA $2004
    INY

    LDA dog, y
    STA $2004
    INY

    LDA dog, y
    STA $2004
    INY

    LDA pos_y
    CLC
    ADC dog, y
    CLC
    SBC scroll
    STA $2004
    INY

    CPY sprite_end
    BNE @loop_sprites
    RTS
.endproc

.proc player_controller
  ; Controller Initialization -----------------------------------------------------
  ; $4016 is an I/O Register (Memory-Mapped I/O Register). Send the Latch pulse down to the 4021. High Voltage = Data Input Mode / Low Voltage = Data Output Mode
  ; Commence Strobing Process
    LDA #$01        ; Load a value of 1 in "A" register
    STA $4016       ; Store that value of 1 into RAM, making voltage high for controller ports. This will set the 8 bits that will have the output values of the procedure. This will contain a series of bits that will denoted if a button was pressed or not.
    LDA #$00        ; Load value of 0 in acummulator.
    STA $4016       ; Brings voltage to low on the latch pins. Tell the controllers to latch buttons
    STA player_moving_flag
  ; The concept above is called 'Strobing' which initializes the controllers in an NES. Summary: Give me the buttons that the player is pressing now. Buttons are shown one at a time in bit0. If bit0 = 0, then it is not pressed, if bit0 = 1, then it is pressed.

  ; Button Instructions ------------------------------------------------------------
  InstrA:
    ; Instructions for when A-Button is pressed (b0 == 1)
    LDA $4016       ; Read P1 Current Button
    AND #%00000001  ; Only analyze bit0. AND instruction is used to clear the other bits, since only bit0 reads the button. (Isolate LSB)
    CMP #$00 ; Check if button is pressed or not (b0 == 0), then branches to Continue if not pressed.
    BEQ InstrB
    LDA reset_background_flag ; Set flag to indicate background should reset
    EOR #%00000001
    STA reset_background_flag
    JSR reset_handler

  InstrB:
    ; Instructions for when B-Button is pressed (b0 == 1)
    LDA $4016       ; Read P1 Current Button
    AND #%00000001  ; Only analyze bit0. AND instruction is used to clear the other bits, since only bit0 reads the button. (Isolate LSB)
    CMP #$00 ; Check if button is pressed or not (b0 == 0), then branches to Continue if not pressed.
    BEQ InstrSelect

  InstrSelect:
    ; Instructions for when Select-Button is pressed (b0 == 1)
    LDA $4016       ; Read P1 Current Button
    AND #%00000001  ; Only analyze bit0. AND instruction is used to clear the other bits, since only bit0 reads the button. (Isolate LSB)
    CMP #$00 ; Check if button is pressed or not (b0 == 0), then branches to Continue if not pressed.
    BEQ InstrStart

  InstrStart:
    ; Instructions for when Start-Button is pressed (b0 == 1)
    LDA $4016       ; Read P1 Current Button
    AND #%00000001  ; Only analyze bit0. AND instruction is used to clear the other bits, since only bit0 reads the button. (Isolate LSB)
    CMP #$00 ; Check if button is pressed or not (b0 == 0), then branches to Continue if not pressed.
    BEQ InstrUp

  InstrUp: 
    ; Instructions for when Up-Button is pressed (b0 == 1)
    LDA $4016       ; Read P1 Current Button
    AND #%00000001  ; Only analyze bit0. AND instruction is used to clear the other bits, since only bit0 reads the button. (Isolate LSB)
    CMP #$00 ; Check if button is pressed or not (b0 == 0), then branches to Continue if not pressed.
    BEQ InstrDown

    LDA can_move_up      
    AND #%00000001
    CMP #$00
    BNE InstrDown

    LDA #$04
    STA sprite_direction  
    LDA pos_x       ; Load the sprite's Y position
    SEC             ; Set Carry Flag: Subtract
    SBC #$01        ; Y = Y - 1
    STA pos_x        ; Store the sprite's new Y position
    LDA #$01
    STA player_moving_flag
    JMP InstrEnd

  InstrDown: 
    ; Instructions for when Down-Button is pressed (b0 == 1)
    LDA $4016       ; Read P1 Current Button
    AND #%00000001  ; Only analyze bit0. AND instruction is used to clear the other bits, since only bit0 reads the button. (Isolate LSB)
    CMP #$00 ; Check if button is pressed or not (b0 == 0), then branches to Continue if not pressed.
    BEQ InstrLeft

    LDA can_move_down     
    AND #%00000001
    CMP #$00
    BNE InstrLeft

    LDA #$00
    STA sprite_direction 
    LDA pos_x        ; Load the sprite's Y position
    CLC             ; Clear Carry Flag: Add
    ADC #$01        ; Y = Y + 1
    STA pos_x      ; Store the sprite's new Y position
    LDA #$01
    STA player_moving_flag
    JMP InstrEnd

  InstrLeft:
    ; Instructions for when L-Button is pressed (b0 == 1)
    LDA $4016       ; Read P1 Current Button
    AND #%00000001  ; Only analyze bit0. AND instruction is used to clear the other bits, since only bit0 reads the button. (Isolate LSB)
    CMP #$00 ; Check if button is pressed or not (b0 == 0), then branches to Continue if not pressed.
    BEQ InstrRight
    
    LDA can_move_left      
    AND #%00000001
    CMP #$00
    BNE InstrRight
    
    LDA #$06
    STA sprite_direction 
    LDA pos_y      ; Load the sprite's X position
    SEC             ; Set Carry Flag: Subtract
    SBC #$01        ; X = X - 1
    STA pos_y      ; Store the sprite's new X position
    LDA #$01
    STA player_moving_flag
    JMP InstrEnd

  InstrRight:
    ; Instructions for when R-Button is pressed (b0 == 1)
    LDA $4016       ; Read P1 Current Button
    AND #%00000001  ; Only analyze bit0. AND instruction is used to clear the other bits, since only bit0 reads the button. (Isolate LSB)
    CMP #$00 ; Check if button is pressed or not (b0 == 0), then branches to Continue if not pressed.
    BEQ InstrEnd

    LDA can_move_right      
    AND #%00000001
    CMP #$00
    BNE InstrEnd
    LDA #$02
    STA sprite_direction 
    LDA pos_y     ; Load the sprite's X position
    CLC             ; Clear Carry Flag: Add
    ADC #$01        ; X = X + 1
    STA pos_y      ; Store the sprite's new X position
    LDA #$01
    STA player_moving_flag

  InstrEnd:
  RTS
.endproc

.proc scroll_handler
  LDA player_moving_flag
  CMP #00
  BEQ end

  LDA sprite_direction
  LSR
  ASL
  CMP #02
  BEQ moving_rigth

  LDA sprite_direction
  LSR
  ASL
  CMP #06
  BEQ moving_left
  JMP end

  moving_left:
    LDA scroll
    CMP #00
    BEQ end
    LDA pos_y
    CMP #$80
    BCS check2
    passed_check2:
    DEC scroll
    JMP end

    check2:
      LDA scroll
      CMP #$FF
      BNE passed_check2
      JMP end

  moving_rigth:
    LDA scroll
    CMP #255
    BEQ end
    LDA pos_y
    CMP #$80
    BCC check
    passed_check:
    INC scroll
    JMP end

    check:
      LDA scroll
      CMP #00
      BNE passed_check

  end:
  RTS
.endproc

.proc getMegaTileIndex
  LDA pos_x       ; Load 'pos_y' into the accumulator
  CLC
  SBC #$08
  LSR A           ; Shift right 4 times
  LSR A
  LSR A
  LSR A
  ASL A           ; Shift left 2 times
  ASL A
  STA megatile_index  ; Store the result in 'megatile_index'

  LDA pos_y
  LSR
  LSR
  LSR
  LSR
  LSR
  LSR
  CLC
  ADC megatile_index
  STA megatile_index
  INC megatile_index
  INC megatile_index
  INC megatile_index
  INC megatile_index

  LDA reset_background_flag
  CMP #$01
  BNE continue
  LDA megatile_index
  CLC
  ADC #$78
  STA megatile_index
  continue:
  ; Check interval of megatile
  LDA pos_y
  LSR
  LSR
  LSR
  AND #$07
  LSR
  ASL
  STA megatile_subindex

  RTS
.endproc

.proc collision_handler
  LDA #$00
  STA can_move_left
  STA can_move_right
  STA can_move_up
  STA can_move_down

  LDA sprite_direction
  LSR
  ASL

  CMP #$00
  BEQ down
  CMP #$02
  BEQ right
  CMP #$04
  BEQ up
  CMP #$06
  BEQ left
  
  JMP end

  down:
    JSR getMegaTileIndex
    JSR collision_down
    JMP end
  right:
    JSR getMegaTileIndex
    JSR collision_right
    JMP end
  up:
    JSR getMegaTileIndex
    JSR collision_up
    JMP end
  left:
    JSR getMegaTileIndex
    JSR collision_left
    JMP end
  
  end:
    RTS
.endproc

.proc collision_right
  LDA #$00
  STA temp2

  check_halfblock:
  LDX megatile_index
  LDA scroll
  CMP #$72
  BCC index00
  TXA
  CLC
  ADC #$39
  TAX

  LDA scroll
  CMP #$87
  BCC index00
  TXA
  CLC
  ADC #$03
  TAX

  index00:
    LDA megatile_subindex
    CMP #$00
    BNE index02
    LDA #$04
    STA temp
    JMP continue

  index02:
    LDA megatile_subindex
    CMP #$02
    BNE index04
    LDA #$02
    STA temp
    JMP continue

  index04:
    LDA megatile_subindex
    CMP #$04
    BNE index06
    LDA #$00
    STA temp
    JMP continue

  index06:
    LDA #$06
    STA temp
    INX

  continue:
    LDA map, X

  @loop:
    LDY temp
    CPY #$00
    BEQ skip_shifting
    LSR
    DEC temp
    JMP @loop

  skip_shifting:
    TAY
    AND #$03
    CMP #$00
    BEQ second_check
    TYA
    AND #$03
    CMP #$03
    BEQ second_check

  LDA #1
  STA can_move_right
  second_check:
    LDA pos_x
    LSR
    LSR
    LSR
    AND #$01
    CMP #$01
    BEQ end
      LDA megatile_index
      CLC
      ADC #$04
      STA megatile_index
      INC temp2
      LDA temp2
      CMP #$02
      BEQ end
      JMP check_halfblock
  end:
    RTS
.endproc

.proc collision_left
  LDA #$00
  STA temp2

  check_halfblock:
    LDX megatile_index
    LDA scroll
    CMP #$7E
    BEQ index00

    LDA scroll
    CMP #$7E
    BCC index00
    TXA
    CLC
    ADC #$3C
    TAX

  index00:
    LDA megatile_subindex
    CMP #$00
    BNE index02
    LDA #$06
    STA temp
    JMP continue

  index02:
    LDA megatile_subindex
    CMP #$02
    BNE index04
    LDA #$04
    STA temp
    JMP continue

  index04:
    LDA megatile_subindex
    CMP #$04
    BNE index06
    LDA #$02
    STA temp
    JMP continue

  index06:
    LDA #$00
    STA temp

  continue:
    LDA map, X

  @loop:
    LDY temp
    CPY #$00
    BEQ skip_shifting
    LSR
    DEC temp
    JMP @loop

  skip_shifting:
    TAY
    AND #$03
    CMP #$00
    BEQ second_check
    TYA
    AND #$03
    CMP #$03
    BEQ second_check


    LDA #$01
    STA can_move_left

  second_check:
  LDA pos_x
  LSR
  LSR
  LSR
  AND #$01
  CMP #$01
  BEQ end
    LDA megatile_index
    CLC
    ADC #$04
    STA megatile_index
    INC temp2
    LDA temp2
    CMP #$02
    BEQ end
    JMP check_halfblock
  end:
    RTS
.endproc

.proc collision_up
  LDA #$00
  STA temp2

  check_halfblock:
    LDX megatile_index
    LDA scroll
    CMP #$7E
    BEQ index00

    LDA scroll
    CMP #$7E
    BCC index00
    TXA
    CLC
    ADC #$3C
    TAX

  index00:
    LDA megatile_subindex
    CMP #$00
    BNE index02
    LDA #$06
    STA temp
    JMP continue

  index02:
    LDA megatile_subindex
    CMP #$02
    BNE index04
    LDA #$04
    STA temp
    JMP continue

  index04:
    LDA megatile_subindex
    CMP #$04
    BNE index06
    LDA #$02
    STA temp
    JMP continue

  index06:
    LDA megatile_subindex
    CMP #$06
    BNE indexFE
    LDA #$00
    STA temp
    JMP continue

  indexFE:
    LDA megatile_subindex
    CMP #$FE
    BNE indexFE
    LDA #$00
    STA temp
    DEX
    JMP continue

  continue:
    LDA map, X

  @loop:
    LDY temp
    CPY #$00
    BEQ skip_shifting
    LSR
    DEC temp
    JMP @loop

  skip_shifting:
    TAY
    AND #$03
    CMP #$00
    BEQ second_check
    TYA
    AND #$03
    CMP #$03
    BEQ second_check


    LDA #$01
    STA can_move_up

  second_check:
  LDA pos_y
  LSR
  LSR
  LSR
  AND #$01
  CMP #$01
  BNE end
    LDA megatile_subindex
    SEC
    SBC #$02
    STA megatile_subindex
    INC temp2
    LDA temp2
    CMP #$02
    BEQ end
    JMP check_halfblock
  end:
    RTS
.endproc

.proc collision_down
  LDA #$00
  STA temp2

  LDA pos_x
  CLC
  ADC #$01
  LSR
  LSR
  LSR
  AND #$01
  CMP #$01
  BNE check_halfblock
  JMP end

  check_halfblock:
    LDX megatile_index
    INX
    INX
    INX
    INX
    LDA scroll
    CMP #$7E
    BEQ index00

    LDA scroll
    CMP #$7E
    BCC index00
    TXA
    CLC
    ADC #$3C
    TAX

  index00:
    LDA megatile_subindex
    CMP #$00
    BNE index02
    LDA #$06
    STA temp
    JMP continue

  index02:
    LDA megatile_subindex
    CMP #$02
    BNE index04
    LDA #$04
    STA temp
    JMP continue

  index04:
    LDA megatile_subindex
    CMP #$04
    BNE index06
    LDA #$02
    STA temp
    JMP continue

  index06:
    LDA megatile_subindex
    CMP #$06
    BNE indexFE
    LDA #$00
    STA temp
    JMP continue

  indexFE:
    LDA megatile_subindex
    CMP #$FE
    BNE indexFE
    LDA #$00
    STA temp
    DEX
    JMP continue

  continue:
    LDA map, X

  @loop:
    LDY temp
    CPY #$00
    BEQ skip_shifting
    LSR
    DEC temp
    JMP @loop

  skip_shifting:
    TAY
    AND #$03
    CMP #$00
    BEQ second_check
    TYA
    AND #$03
    CMP #$03
    BEQ second_check


    LDA #$01
    STA can_move_down

  second_check:
  LDA pos_y
  LSR
  LSR
  LSR
  AND #$01
  CMP #$01
  BNE end
    LDA megatile_subindex
    SEC
    SBC #$02
    STA megatile_subindex
    INC temp2
    LDA temp2
    CMP #$02
    BEQ end
    JMP check_halfblock
  end:
    RTS
.endproc

.proc draw_background_subroutine
  LDA map_index
  CLC
  ADC reset_background_flag
  CMP #$00
  BEQ Level_1_1

  LDA map_index
  CLC
  ADC reset_background_flag
  CMP #$04
  BEQ Level_1_2

  LDA map_index
  CLC
  ADC reset_background_flag
  CMP #$01
  BEQ Level_2_1

  LDA map_index
  CLC
  ADC reset_background_flag
  CMP #$05
  BEQ Level_2_2

  Level_1_1:
    LDY #$00
    STY counter
    LDA #$3C
    STA background_end
    JMP draw_loop

  Level_1_2:
    LDY #$3B
    STY counter
    LDA #$78
    STA background_end
    JMP draw_loop

  Level_2_1:
    LDY #$3C
    STY counter
    LDA #$B4
    STA background_end
    JMP draw_loop

  Level_2_2:
    LDY #$B3
    STY counter
    LDA #$F0
    STA background_end
    JMP draw_loop

  draw_loop:
    JSR getIndex
    LDX background_index_low
    LDY counter
    LDA #$00
    STA counter2
    LDA map, y
    STA temp

    draw_from_megatile:
      LDA temp
      AND #$03
      ASL
      ASL
      TAY

      LDA background_index_high
      STA $2006
      TXA
      STA $2006
      JSR draw_background

      LDA background_index_high
      STA $2006
      TXA
      CLC 
      ADC #$01    
      STA $2006
      JSR draw_background

      LDA background_index_high
      STA $2006
      TXA         ; Transfer X to A
      CLC         ; Clear the carry flag before addition
      ADC #$20    ; Add 32 (which is $21 in hexadecimal) to A
      STA $2006
      JSR draw_background

      LDA background_index_high
      STA $2006
      TXA         ; Transfer X to A
      CLC         ; Clear the carry flag before addition
      ADC #$21    ; Add 33 (which is $21 in hexadecimal) to A
      STA $2006
      JSR draw_background

      LDA background_index_low
      CLC
      SBC #$01
      STA background_index_low
      TAX

      LDA temp
      LSR
      LSR
      STA temp
      INC counter2
      LDY counter2
      CPY #$04
      bne draw_from_megatile

  INC counter
  LDY counter
  CPY background_end
  BNE draw_loop

  RTS
.endproc

.proc background_attributes
  LDX #$00
  LDA #$C0
  STA temp
  @loop:

    LDA #$23
    STA $2006

    LDA temp
    STA $2006
    
    LDA attributes, X
    STA $2007

    INC temp
    INX
    LDY temp
    CPY #$00
    BNE @loop

  LDA #$C0
  STA temp

  @loop2:
    LDA #$27
    STA $2006

    LDA temp
    STA $2006
    
    LDA attributes, X
    STA $2007

    INC temp
    INX
    LDY temp
    CPY #$00
    BNE @loop2
  RTS
.endproc

.proc draw_background
  LDA reset_background_flag
  CMP #$01
  BNE next
  LDA tiles_info2, Y
  JMP end 
  next:
  LDA tiles_info, Y ; Load a byte from the address (tiles_info + Y)
  end:
  STA $2007         ; Store the accumulator into PPU Data register
  INY
  RTS               ; Return from subroutine
.endproc

.proc getIndex
  check_reset:
    LDA reset_background_flag
    CMP #$01
    BNE check_offset
    TYA
    SEC
    SBC #$78
    TAY
    LDA map_index
    CMP #$04
    BNE next_thing
    TYA
    SEC
    SBC #$3C
    TAY
    JMP next_thing

  check_offset:
    LDA map_index
    CMP #$04
    BNE next_thing
    TYA
    SEC
    SBC #$3C
    TAY

  next_thing:
  TYA
  LSR A                ; Shift right by 2 (x >> 2)
  LSR A

  LDX #$00
  STA background_index_low
  mult:
    ; Shift the high byte, accounting for possible carry from low byte
    LDA background_index_low   ; Load the low byte
    AND #$80                   ; Check if MSB is set (will indicate carry)
    BNE handle_carry           ; If carry is set, branch to handle_carry
    ; If no carry, simply shift the high byte
    LDA background_index_high  ; Load the high byte
    ASL A                      ; Shift left (multiply by 2)
    STA background_index_high  ; Store the shifted high byte
    JMP shift_low              ; Move on to shifting the low byte

    handle_carry:
        LDA background_index_high  ; Load the high byte
        ASL A                      ; Shift left (multiplying by 2)
        CLC                         ; Clear carry for clean addition
        ADC #$01                    ; Add one due to carry from low byte
        STA background_index_high  ; Store the updated high byte
        JMP shift_low

    shift_low:
        ; Now shift the low byte
        LDA background_index_low
        ASL A                       ; Shift left (multiply by 2)
    STA background_index_low    ; Store the shifted low byte
    
    ; Loop handling
    INX                        ; Increment loop counter
    CPX #$06                    ; Compare with loop limit (6 times)
    BNE mult                    ; If not at limit, loop again


 ; Now isolate the lower 2 bits
  TYA                    ; Reload the original value
  AND #$03               ; Isolate last 2 bits
  ASL A                  ; Multiply by 2
  ASL A                  ; Multiply by 4
  ASL A                  ; Multiply by 8
  CLC                       ; Clear carry flag
  ADC background_index_low  ; Add the multiplied bits to the low byte
  STA background_index_low  ; Store the result

  LDA background_index_high
  CLC 
  ADC #$20
  CLC
  ADC map_index
  STA background_index_high
  LDA background_index_low
  CLC 
  ADC #$06
  STA background_index_low
  RTS                    ; Return from subroutine
.endproc


dog:
  ; Dog Front - SPRITE 0
  .byte $00, $07, %00100000, $00 
  .byte $00, $08, %00100000, $08
  .byte $08, $09, %00100000, $00
  .byte $08, $09, %01100000, $08

  ; Dog Front Running - SPRITE 1
  .byte $00, $0A, %00100000, $00  
  .byte $00, $0B, %00100000, $08
  .byte $08, $0C, %00100000, $00
  .byte $08, $0C, %01100000, $08

  ; Dog Right - SPRITE 2
  .byte $00, $01, %00100000, $00
  .byte $00, $02, %00100000, $08
  .byte $08, $03, %00100000, $00
  .byte $08, $04, %00100000, $08

  ; Dog Right Running - SPRITE 3
  .byte $00, $01, %00100000, $00
  .byte $00, $02, %00100000, $08
  .byte $08, $05, %00100000, $00
  .byte $08, $06, %00100000, $08

  ; Dog Back - SPRITE 4
  .byte $00, $0D, %00100000, $00      
  .byte $00, $0D, %01100000, $08
  .byte $08, $0E, %00100000, $00
  .byte $08, $0F, %00100000, $08

  ; Dog Back Running - SPRITE 5
  .byte $00, $10, %00100000, $00
  .byte $00, $10, %01100000, $08
  .byte $08, $11, %00100000, $00
  .byte $08, $12, %00100000, $08

  ; Dog Left - SPRITE 6
  .byte $00, $02, %01100000, $00
  .byte $00, $01, %01100000, $08
  .byte $08, $04, %01100000, $00
  .byte $08, $03, %01100000, $08

  ; Dog Left Running - SPRITE 7
  .byte $00, $02, %01100000, $00
  .byte $00, $01, %01100000, $08
  .byte $08, $06, %01100000, $00
  .byte $08, $05, %01100000, $08

tiles_info: 
  ; Nothing:
  .byte $00
  .byte $00
  .byte $00
  .byte $00

  ; Stone Brick Wall
  .byte $13
  .byte $13
  .byte $13
  .byte $13  

  ; Stone:
  .byte $15
  .byte $15
  .byte $15
  .byte $15

  ; Bush:
  .byte $16
  .byte $2E
  .byte $2F
  .byte $30

tiles_info2:
   ; Nothing:
  .byte $00
  .byte $00
  .byte $00
  .byte $00

  .byte $17
  .byte $17
  .byte $17
  .byte $17

  ; Wood:
  .byte $18
  .byte $18
  .byte $18
  .byte $18

  ; Fence:
  .byte $1B
  .byte $1B
  .byte $24
  .byte $24

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

attributes:
  .byte %10010101, %10100101, %10100101, %10100101, %10100101, %10100101, %10100101, %10100101 ; 1 
  .byte %10011001, %10101010, %10101010, %10101010, %10101010, %10101010, %10101010, %10101010 ; 2
  .byte %10011001, %10101010, %10101010, %10101010, %10101010, %10101010, %10101010, %10101010 ; 3
  .byte %10011001, %10101010, %10101010, %10101010, %10101010, %10101010, %10101010, %10101010 ; 4
  .byte %10011001, %10101010, %10101010, %10101010, %10101010, %10101010, %10101010, %10101010 ; 5
  .byte %10011001, %10101010, %10101010, %10101010, %10101010, %10101010, %10101010, %10101010 ; 6
  .byte %10011001, %10101010, %10101010, %10101010, %10101010, %10101010, %10101010, %10101010 ; 7 
  .byte %00000101, %00000101, %00000101, %00000101, %00000101, %00000101, %00000101, %00000101 ; 8

  .byte %10100101, %10100101, %10100101, %10100101, %10100101, %10100101, %10100101, %01100101 ; 1 
  .byte %10101010, %10101010, %10101010, %10101010, %10101010, %10101010, %10101010, %01100110 ; 2
  .byte %10101010, %10101010, %10101010, %10101010, %10101010, %10101010, %10101010, %01100110 ; 3
  .byte %10101010, %10101010, %10101010, %10101010, %10101010, %10101010, %10101010, %01100110 ; 4
  .byte %10101010, %10101010, %10101010, %10101010, %10101010, %10101010, %10101010, %01100110 ; 5
  .byte %10101010, %10101010, %10101010, %10101010, %10101010, %10101010, %10101010, %01100110 ; 6
  .byte %10101010, %10101010, %10101010, %10101010, %10101010, %10101010, %10101010, %01100110 ; 7 
  .byte %00000101, %00000101, %00000101, %00000101, %00000101, %00000101, %00000101, %00000101 ; 8

map:
  ;Level 1.1
  .byte %10101010, %10101010, %10101010, %10101010 ; 0
  .byte %10000100, %00000000, %01010111, %11110101 ; 1
  .byte %10000101, %01110101, %01000000, %01000000 ; 2
  .byte %10000000, %00000000, %01000101, %01110100 ; 3
  .byte %10110100, %01000100, %00000100, %01000100 ; 4
  .byte %10000100, %01110101, %01000100, %01000100 ; 5
  .byte %10000100, %00111111, %01001100, %00010100 ; 6
  .byte %10000100, %01110101, %01110100, %01011100 ; 7
  .byte %10000100, %00000001, %01110100, %00000001 ; 8
  .byte %10001100, %00010001, %00000100, %01010101 ; 9
  .byte %10010101, %00010101, %00000000, %00000100 ; 10
  .byte %10000100, %00000000, %01000101, %01110100 ; 11
  .byte %10000101, %00011101, %01110100, %00000100 ; 12
  .byte %10000011, %00010000, %00000100, %11010100 ; 13
  .byte %10101010, %10101010, %10101010, %10101010 ; 14

  ;Level 1.2
  .byte %10101010, %10101010, %10101010, %10101010 ; 0
  .byte %01001100, %00000001, %00010111, %11000010 ; 1
  .byte %01000101, %01110101, %00010001, %01010010 ; 2
  .byte %00000000, %00000001, %00000001, %00010010 ; 3
  .byte %01000100, %00000001, %01000101, %00010010 ; 4
  .byte %01000100, %01011101, %01000100, %00010010 ; 5
  .byte %01000101, %01001100, %00000100, %00011110 ; 6
  .byte %00000000, %00001101, %00000101, %00111110 ; 7
  .byte %01010111, %00010101, %00010100, %00010010 ; 8
  .byte %00000000, %00000000, %00010000, %00010010 ; 9
  .byte %00011101, %01010101, %01010001, %00010010 ; 10
  .byte %01000000, %00000000, %00010001, %11010010 ; 11
  .byte %01000101, %01011101, %01010101, %11010110 ; 12
  .byte %01111100, %01000000, %00111111, %00110010 ; 13
  .byte %10101010, %10101010, %10101010, %10101010 ; 14

  ;Level 2.1
  .byte %10101010, %10101010, %10101010, %10101010 ; 0
  .byte %10000000, %00000101, %01010000, %01000000 ; 1
  .byte %10010101, %11010100, %00010001, %01110100 ; 2
  .byte %10000000, %00000101, %00010001, %01000100 ; 3
  .byte %10000100, %01000100, %11010001, %01111100 ; 4
  .byte %10000100, %00001100, %01110001, %01000100 ; 5
  .byte %10000100, %01010100, %01000011, %01000100 ; 6
  .byte %10010100, %00000100, %01000100, %00000100 ; 7
  .byte %10010000, %01001100, %00000100, %00010100 ; 8
  .byte %10011111, %01010101, %01010100, %01010000 ; 9
  .byte %10000000, %01000000, %00010000, %00111101 ; 10
  .byte %10000101, %00000100, %01001101, %00010001 ; 11
  .byte %10000111, %11010100, %01000001, %00010001 ; 12
  .byte %10001100, %01010000, %01011111, %00010000 ; 13
  .byte %10101010, %10101010, %10101010, %10101010 ; 14

  ;Level 2.2
  .byte %10101010, %10101010, %10101010, %10101010 ; 0
  .byte %01000000, %00000101, %01000000, %00011110 ; 1
  .byte %01011101, %01000001, %01011101, %01010010 ; 2
  .byte %01010001, %01010001, %00010000, %00110010 ; 3
  .byte %00001101, %01000001, %01010101, %00010110 ; 4
  .byte %01000101, %01001100, %01000000, %00010010 ; 5
  .byte %01000000, %01000100, %01000101, %01011110 ; 6
  .byte %01010100, %11111100, %00010011, %01000010 ; 7
  .byte %01000100, %01010100, %00010001, %01010010 ; 8
  .byte %01001100, %01000001, %01010001, %00000110 ; 9
  .byte %01000100, %00000101, %01000001, %00010010 ; 10
  .byte %01000101, %01000001, %01010001, %00011110 ; 11
  .byte %01010100, %00000101, %00010000, %00110010 ; 12
  .byte %00000000, %01001100, %00111100, %01010010 ; 13
  .byte %10101010, %10101010, %10101010, %10101010 ; 14


; Character memory
.segment "CHARS"
.incbin "sprites.chr"
 
.segment "VECTORS"
.addr nmi_handler, reset_handler,irq_handler

.segment "ZEROPAGE"
player_moving_flag: .res 1
sprite_direction: .res 1
sprite_end: .res 1
frame_tick:  .res 1
pos_x: .res 1
pos_y: .res 1
background_index_high: .res 1
background_index_low: .res 1
counter: .res 1
counter2: .res 1
temp: .res 1
temp2:  .res 1
temp3: .res 1
megatile_index: .res 1
megatile_subindex: .res 1
can_move_left: .res 1
can_move_right: .res 1
can_move_up: .res 1
can_move_down: .res 1
map_index: .res 1
background_end: .res 1
scroll: .res 1
reset_mega: .res 1
reset_background_flag: .res 1
testing_var: .res 1
