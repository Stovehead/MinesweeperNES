.feature line_continuations +

PPUCTRL = $2000
PPUMASK = $2001
PPUSTATUS = $2002
OAMADDR = $2003
OAMDATA = $2004
PPUSCROLL = $2005
PPUADDR = $2006
PPUDATA = $2007
APUDMC = $4010
OAMDMA = $4014
JOY1 = $4016
JOY2 = $4017
APUFRAMECOUNTER = $4017
OAMBUFFER = $0200
BUTTON_A =      %10000000
BUTTON_B =      %01000000
BUTTON_SELECT = %00100000
BUTTON_START =  %00010000
BUTTON_UP =     %00001000
BUTTON_DOWN =   %00000100
BUTTON_LEFT =   %00000010
BUTTON_RIGHT =  %00000001
NUM_MINES = 15
GRID_WIDTH = 14
GRID_HEIGHT = 9
GRID_X = 2
GRID_Y = 8
MINE_MASK = %00010000
OPENED_MASK = %00100000
FLAG_MASK = %01000000
CLICKED_MINE_MASK = %10000000
DIGIT_MASK = %00001111
GAME_NOT_STARTED = 0
GAME_IN_PROGRESS = 1
GAME_OVER = 2
GAME_WON = 3
TILE_UPDATE_BUFFER_SIZE = 8

.zeropage
    scratch: .res $10
    frame_count: .res 1
    controller_input_prev: .res 1
    controller_input: .res 1
    mouse_state_prev: .res 1
    mouse_state: .res 1
    mouse_flags: .res 1
    mouse_display_x: .res 1
    mouse_display_y: .res 1
    mouse_delta_x: .res 1
    mouse_delta_y: .res 1
    mouse_x: .res 1
    mouse_y: .res 1
    mouse_down_x: .res 1
    mouse_down_y: .res 1
    game_state: .res 1  ; 0 = game not started
                        ; 1 = game in progress
                        ; 2 = game over
                        ; 3 = game won
    opened_tiles: .res 1
    num_flags: .res 1
    seconds_elapsed: .res 2
    time_accumulator: .res 3
    timer_digits_buffer: .res 3
    mines_digits_buffer: .res 3
    rng_seed: .res 2
    minefield_update_row: .res 1
    minefield_update_current_tile: .res 2
    minefield_update_current_attribute: .res 1
    screen_update_setting: .res 1   ; 0 = update in 9 frames with no blanking
                                    ; 1 = update in 2 frames with blanking
    snes_mouse_enabled: .res 1
    currently_shuffling_mines: .res 1
    tile_stack_begin: .res 1
    num_tiles_buffered: .res 1
    tile_update_buffer: .res TILE_UPDATE_BUFFER_SIZE

.bss
    mine_shuffle_space: .res 128
    minefield_tiles: .res 504
    minefield_attributes: .res 40
    .res 96 ; Padding to align to the beginning of the page
    minefield_row_0: .res 14
    .res 2
    minefield_row_1: .res 14
    .res 2
    minefield_row_2: .res 14
    .res 2
    minefield_row_3: .res 14
    .res 2
    minefield_row_4: .res 14
    .res 2
    minefield_row_5: .res 14
    .res 2
    minefield_row_6: .res 14
    .res 2
    minefield_row_7: .res 14
    .res 2
    minefield_row_8: .res 14
    .res 2


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

.segment "STARTUP"

.segment "CODE"

reset:
    sei         ; disable IRQs
    cld         ; disable decimal mode
    ldx #$40
    stx APUFRAMECOUNTER   ; disable APU frame IRQ
    ldx #$FF    
    txs         ; Set up stack
    inx         ; now X = 0
    stx PPUCTRL	; disable NMI
    stx PPUCTRL ; disable rendering
    stx APUDMC  ; disable DMC IRQs
    ; The vblank flag is in an unknown state after reset,
    ; so it is cleared here to make sure that @vblankwait1
    ; does not exit immediately.
    bit PPUSTATUS

vblankwait1:
    bit PPUSTATUS
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

vblankwait2:
    bit $2002
    bpl vblankwait2

main:
load_palettes:
    lda PPUSTATUS
    lda #$3F
    sta PPUADDR
    lda #$00
    sta PPUADDR
    ldx #$00
:
    lda Palettes, x
    sta PPUDATA
    inx
    cpx #$20
    bne :-

load_nametable:
    lda PPUSTATUS ; Avoiding corrupting palette, but is it necessary?
    lda #$3F
    sta PPUADDR
    lda #$00
    sta PPUADDR
    sta PPUADDR
    sta PPUADDR
    lda #$23 ; Clear attribute table for first nametable
    sta PPUADDR
    lda #$C0
    sta PPUADDR
    ldx #64 
    lda #$AA
:
    sta PPUDATA
    dex
    bne :-

    lda #$27 ; Do same for second nametable
    sta PPUADDR
    lda #$C0
    sta PPUADDR
    ldx #64 
    lda #%10101010
:
    sta PPUDATA
    dex
    bne :-

    lda #$23 ; Set up attributes for numbers
    sta PPUADDR
    lda #$C8
    sta PPUADDR
    lda #%00100010
    sta PPUDATA
    lda #$00
    sta PPUDATA
    lda #$23 
    sta PPUADDR
    lda #$CE
    sta PPUADDR
    lda #$00
    sta PPUDATA
    lda #%10001000
    sta PPUDATA

    lda PPUSTATUS ; Load tiles
    lda #$20
    sta PPUADDR
    lda #$00
    sta PPUADDR
    ldx #$40
:
    sta PPUDATA
    dex
    bne :-
    lda #29
    sta PPUDATA
    ldx #30
    lda #30
:
    sta PPUDATA
    dex
    bne :-
    lda #31
    sta PPUDATA

    ldy #$00
:
    lda Rows1to7, y
    sta PPUDATA
    iny
    cpy #$E0
    bne :-
    ldy #$00
:
    lda #44
    sta PPUDATA
    lda #49
    sta PPUDATA
    ldx #14
:
    lda #$4C
    sta PPUDATA
    lda #$4D
    sta PPUDATA
    dex
    bne :-
    lda #43
    sta PPUDATA
    lda #44
    sta PPUDATA
    lda #44
    sta PPUDATA
    lda #49
    sta PPUDATA
    ldx #14
:
    lda #$4E
    sta PPUDATA
    lda #$4F
    sta PPUDATA
    dex
    bne :-
    lda #43
    sta PPUDATA
    lda #44
    sta PPUDATA
    iny
    cpy #9
    bne :---

    sta PPUDATA
    lda #72
    sta PPUDATA
    ldx #28
    lda #73
:
    sta PPUDATA
    dex
    bne :-

    lda #74
    sta PPUDATA
    lda #44
    sta PPUDATA

    ldx #$20
    :
    sta PPUDATA
    dex
    bne :-

    ldx #$00 ; Init OAM
    lda #$FF
:
    sta OAMBUFFER, x
    inx
    bne :-

    lda #23    ; Draw Mario's head
    sta OAMBUFFER + (4 * 2)
    sta OAMBUFFER + (4 * 4)
    sta OAMBUFFER + (4 * 6)
    sta OAMBUFFER + (4 * 8)
    lda #39    
    sta OAMBUFFER + (4 * 3)
    sta OAMBUFFER + (4 * 5)
    sta OAMBUFFER + (4 * 7)
    sta OAMBUFFER + (4 * 9)
    lda #$01
    sta OAMBUFFER + (4 * 2) + 1
    lda #$05
    sta OAMBUFFER + (4 * 3) + 1
    lda #$09
    sta OAMBUFFER + (4 * 4) + 1
    lda #$0D
    sta OAMBUFFER + (4 * 5) + 1
    lda #$0B
    sta OAMBUFFER + (4 * 6) + 1
    lda #$0F
    sta OAMBUFFER + (4 * 7) + 1
    lda #$03
    sta OAMBUFFER + (4 * 8) + 1
    lda #$07
    sta OAMBUFFER + (4 * 9) + 1
    lda #%00000000
    sta OAMBUFFER + (4 * 2) + 2
    sta OAMBUFFER + (4 * 4) + 2
    sta OAMBUFFER + (4 * 6) + 2
    sta OAMBUFFER + (4 * 8) + 2
    lda #%00000001
    sta OAMBUFFER + (4 * 3) + 2
    sta OAMBUFFER + (4 * 5) + 2
    sta OAMBUFFER + (4 * 7) + 2
    sta OAMBUFFER + (4 * 9) + 2
    lda #112
    sta OAMBUFFER + (4 * 2) + 3
    sta OAMBUFFER + (4 * 3) + 3
    lda #120
    sta OAMBUFFER + (4 * 4) + 3
    sta OAMBUFFER + (4 * 5) + 3
    lda #128
    sta OAMBUFFER + (4 * 6) + 3
    sta OAMBUFFER + (4 * 7) + 3
    lda #136
    sta OAMBUFFER + (4 * 8) + 3
    sta OAMBUFFER + (4 * 9) + 3

    lda #48    ; Draw Mario's mouth sprite
    sta OAMBUFFER + (4 * 10)
    lda #$29
    sta OAMBUFFER + (4 * 10) + 1
    lda #%00000000
    sta OAMBUFFER + (4 * 10) + 2
    lda #125
    sta OAMBUFFER + (4 * 10) + 3

    ; Load tile mask graphics
    ; Set Y positions
    lda #$FF
    .repeat 18, i
    sta OAMBUFFER + (4 * (11 + i))
    .endrepeat
    ; Set X positions
    lda #$00
    .repeat 18, i
    sta OAMBUFFER + (4 * (11 + i)) + 3
    .endrepeat
    ; Set attributes
    lda #%00000010
    .repeat 18, i
    sta OAMBUFFER + (4 * (11 + i)) + 2
    .endrepeat
    ; Set indices for left half
    lda #$2F
    .repeat 9, i
    sta OAMBUFFER + (4 * (11 + 2 * i)) + 1
    .endrepeat
    ; Set indices for right half
    lda #$31
    .repeat 9, i
    sta OAMBUFFER + (4 * (12 + 2 * i)) + 1
    .endrepeat


    lda #$00 ; Is this necessary?
    sta OAMADDR

    bit PPUSTATUS ; Set scroll
    lda #$00
    sta PPUSCROLL
    lda #8
    sta PPUSCROLL

    lda #$FF ; Initialize digit buffers
    sta mines_digits_buffer
    sta mines_digits_buffer + 1
    sta mines_digits_buffer + 2
    sta timer_digits_buffer
    sta timer_digits_buffer + 1
    sta timer_digits_buffer + 2

    lda #$80 ; Move mouse to center of screen
    sta mouse_display_x
    sta mouse_display_y

    lda #$01 ; Init RNG
    sta rng_seed

    lda #$00 ; Fill shuffle space with 0s
    tax
    :
    sta mine_shuffle_space, x
    inx
    cpx #(128 - NUM_MINES)
    bne :-
    lda #MINE_MASK
    :
    sta mine_shuffle_space, x ; Fill the last 15 with mines
    inx
    cpx #128
    bne :-

    lda #<minefield_tiles
    sta minefield_update_current_tile
    lda #>minefield_tiles
    sta minefield_update_current_tile + 1
    lda #<minefield_attributes
    sta minefield_update_current_attribute

check_snes_mouse:
    lda #$01
    sta scratch

    sta JOY1
    lda JOY2 ; Sending a clock while the latch is turned on will init the sensitivity
    lda JOY2
    lda JOY2
    lda #$00
    sta JOY1

    read_loop_1:
    lda JOY2 ; Mouse should be connected to port 2
    lsr
    rol scratch
    nop ; Need at least 14 cycles between reads for Hyper Click mice
    nop
    bcc read_loop_1

    lda scratch
    beq :+ 
    jmp after_check_mouse ; If the first report isn't all 0s, then it's not a mouse
    :

    lda #$01
    sta scratch
    
    read_loop_2:
    lda JOY2 
    lsr
    rol scratch
    nop
    nop
    bcc read_loop_2

    lda scratch
    and #%00001111
    cmp #%00000001 ; Check signature to see if it's a mouse
    beq :+
    jmp after_check_mouse
    :

    lda #$01
    sta snes_mouse_enabled ; Enable the mouse

    sta scratch
    read_loop_3: ; The other reads aren't really needed, just doing them to be safe
    lda JOY2 
    lsr
    rol scratch
    bcc read_loop_3

    lda #$01
    sta scratch

    read_loop_4:
    lda JOY2 
    lsr
    rol scratch
    bcc read_loop_4

after_check_mouse:

    lda #%10100000	; Enable NMI and set sprite size
    sta PPUCTRL

    sed ; Decimal flag used to see if frame finished
forever:
    jmp forever

rand: ; Brad Smith's 16-bit galois linear-feedback shift register PRNG implementation
    pha
    txa
    pha
    tya
    pha

    lda rng_seed+1
	tay
	lsr
	lsr
	lsr
	sta rng_seed+1
	lsr
	eor rng_seed+1
	lsr
	eor rng_seed+1
	eor rng_seed+0
	sta rng_seed+1
	tya
	sta rng_seed+0
	asl
	eor rng_seed+0
	asl
	eor rng_seed+0
	asl
	asl
	asl
	eor rng_seed+0
	sta rng_seed+0

    pla
    tay
    pla
    tax
    pla
    rts

shuffle_mines: ; Clobbers $0A, A, X, and Y
    lda #$01
    sta currently_shuffling_mines
    ldx #127
    :
    jsr rand ; Fisher-Yates algorithm
    lda rng_seed
    and #%01111111
    tay
    lda mine_shuffle_space, x
    sta scratch + $A
    lda mine_shuffle_space, y
    sta mine_shuffle_space, x
    lda scratch + $A
    sta mine_shuffle_space, y
    dex
    beq :+
    lda rng_seed + 1
    and #%01111111
    tay
    lda mine_shuffle_space, x
    sta scratch + $A
    lda mine_shuffle_space, y
    sta mine_shuffle_space, x
    lda scratch + $A
    sta mine_shuffle_space, y
    dex
    jmp :-
    :

    ldx #126
    reshuffle_loop:
    lda mine_shuffle_space, x ; We only have 126 spaces, so we need to reshuffle the last two spaces if they have mines
    beq reshuffle_loop_end
    :
    jsr rand
    lda rng_seed ; 
    and #%01111111
    cmp #126
    bcs :- ; Retry if we rolled a space higher than 126
    tay
    lda mine_shuffle_space, y
    bne :- ; Retry if we rolled another mine
    lda #MINE_MASK ; Swap them
    sta mine_shuffle_space, y
    lda #$00
    sta mine_shuffle_space, x
    reshuffle_loop_end:
    inx
    bpl reshuffle_loop ; 7th bit should be set when x = 128, so that's when we break out of the loop
    lda #$00
    sta currently_shuffling_mines
    rts

init_minefield: ; Clobbers $00, $01, $02, X, Y, and A
    ldx #$0E
    lda #$00
    :
    dex
    sta minefield_row_0, x
    sta minefield_row_1, x
    sta minefield_row_2, x
    sta minefield_row_3, x
    sta minefield_row_4, x
    sta minefield_row_5, x
    sta minefield_row_6, x
    sta minefield_row_7, x
    sta minefield_row_8, x
    bne :-
    ldy #$01
    sty mines_digits_buffer + 1
    ldy #$05
    sty mines_digits_buffer
    ldy #$00
    sty opened_tiles
    sty num_flags
    sty seconds_elapsed
    sty time_accumulator
    sty time_accumulator + 1
    sty time_accumulator + 2
    sty timer_digits_buffer
    sty timer_digits_buffer + 1
    sty timer_digits_buffer + 2
    sty mines_digits_buffer + 2
    lda #>minefield_row_1 ; It's a surprise tool that will help us later
    sta scratch + 1
    .repeat 9, i ; Copying from shuffle space to the actual grid
    ldx #$00 ; This code is probably horribly inefficient, but it seems... good enough
    :
    lda mine_shuffle_space + 14 * i, x ; Load the current square
    beq :+ ; Check if it's a mine or not
    clc ; If it is, increment all the adjacent tiles
    stx scratch + 2
    lda #<minefield_row_0 + 16 * i
    adc scratch + 2 ; Store address of current square
    sec
    sbc #$11 ; Go to the top left
    sta scratch ; Store the address (This may go out of bounds, but that's ok!)
    clc
    lda (scratch), y ; Increment top left
    adc #$01
    sta (scratch), y
    inc scratch
    lda (scratch), y ; Increment top
    adc #$01
    sta (scratch), y 
    inc scratch
    lda (scratch), y ; Increment top right
    adc #$01
    sta (scratch), y
    lda scratch
    adc #$0E ; Go to middle left
    sta scratch
    clc
    lda (scratch), y ; Increment left
    adc #$01
    sta (scratch), y
    inc scratch ; Skip center
    inc scratch
    lda (scratch), y ; Increment right
    adc #$01
    sta (scratch), y
    lda scratch
    adc #$0E ; Got to bottom left
    sta scratch
    clc
    lda (scratch), y ; Increment bottom left
    adc #$01
    sta (scratch), y
    inc scratch
    lda (scratch), y ; Increment bottom
    adc #$01
    sta (scratch), y
    inc scratch
    lda (scratch), y ; Increment bottom right
    adc #$01
    sta (scratch), y
    lda #MINE_MASK ; It's a mine, so load a mine to place there
    :
    sta scratch + 2
    lda minefield_row_0 + 16 * i, x
    ora scratch + 2 ; Or it in order to not overwrite what's already there
    sta minefield_row_0 + 16 * i, x
    inx
    cpx #14
    bne :--
    .endrepeat

    ldx #$00 ; Init tiles
    :
    lda #$4B
    .repeat 14, i
    sta minefield_tiles + 2 * i, x
    .endrepeat
    lda #$4C
    .repeat 14, i
    sta minefield_tiles + 2 * i + 1, x
    .endrepeat
    lda #$4D
    .repeat 14, i
    sta minefield_tiles + 2 * i + 28, x
    .endrepeat
    lda #$4E
    .repeat 14, i
    sta minefield_tiles + 2 * i + 29, x
    .endrepeat
    txa
    clc
    adc #56
    bcs :+
    tax
    jmp :-
    :
    ldx #$00
    :
    lda #$4B
    .repeat 14, i
    sta minefield_tiles + 2 * i + 280 , x
    .endrepeat
    lda #$4C
    .repeat 14, i
    sta minefield_tiles + 2 * i + 281, x
    .endrepeat
    lda #$4D
    .repeat 14, i
    sta minefield_tiles + 2 * i + 308, x
    .endrepeat
    lda #$4E
    .repeat 14, i
    sta minefield_tiles + 2 * i + 309, x
    .endrepeat
    txa
    clc
    adc #56
    cmp #224
    bcs :+
    tax
    jmp :-
    :
    lda #$AA ; Init attributes
    ldx #40
    :
    dex
    sta minefield_attributes, x
    bne :-

    rts

update_cursor_sprite: ; ID of cursor sprite in Y
    lda mouse_display_y
    sta OAMDATA
    sty OAMDATA
    lda #%00000011
    sta OAMDATA 
    lda mouse_display_x
    sta OAMDATA 
   
    lda #$00
    tax
    sta PPUSTATUS   ; put 0 in ppu bus 
    sta OAMADDR, x  ; indexed write dummy read cycle puts ppu open bus on the cpu bus before the write cycle
    rts

increment_timer: ; Clobbers A
    ; Magic constant: 00000100 01000010 01111001
    lda seconds_elapsed + 1 ; Check if the timer is already at 999
    cmp #$09
    bne :+
    lda seconds_elapsed
    cmp #$99
    beq end_increment_timer
    :
    clc ; Add our constant
    lda #%01111001
    adc time_accumulator
    sta time_accumulator
    lda #%01000010
    adc time_accumulator + 1
    sta time_accumulator + 1
    lda #%00000100
    adc time_accumulator + 2
    sta time_accumulator + 2
    bcc end_increment_timer ; Once it overflows, we add one to the seconds elapsed
    inc seconds_elapsed
    lda seconds_elapsed
    and #%00001111 ; Check if the first digit overflowed
    cmp #$A
    bne update_ones
    lda seconds_elapsed ; If it did, set it to 0 and add 1 to the second digit
    and #%11110000
    clc ; Not sure if this is necessary, but best to be safe
    adc #$10
    sta seconds_elapsed
    cmp #$A0 ; Check if second digit overflowed
    bne update_tens
    lda #$00 ; If it did, set first two digits to 0 and and increment the third digit
    sta seconds_elapsed
    inc seconds_elapsed + 1 ; No need to worry about overflowing since we stop at 999
    lda seconds_elapsed + 1 ; Update hundreds place
    sta timer_digits_buffer + 2
    update_tens:
    lda seconds_elapsed ; Update tens place
    lsr
    lsr
    lsr
    lsr
    sta timer_digits_buffer + 1
    update_ones:
    lda seconds_elapsed ; Update ones place
    and #%00001111
    sta timer_digits_buffer
    end_increment_timer:
    rts

update_timer_display:
    lda #%10100100
    sta PPUCTRL ; Set PPU increment to 32
    lda timer_digits_buffer ; Update ones
    cmp #$FF
    beq end_timer_display_update
    tay
    lda #$20
    sta $02
    lda #$9C
    sta $03
    jsr draw_digit
    lda #$FF
    sta timer_digits_buffer
    lda timer_digits_buffer + 1 ; Update tens
    cmp #$FF
    beq end_timer_display_update
    tay
    lda #$20
    sta $02
    lda #$9A
    sta $03
    jsr draw_digit 
    lda #$FF
    sta timer_digits_buffer + 1
    lda timer_digits_buffer + 2; Update hundreds
    cmp #$FF
    beq end_timer_display_update
    tay
    lda #$20
    sta $02
    lda #$98
    sta $03
    jsr draw_digit 
    lda #$FF
    sta timer_digits_buffer + 2
    end_timer_display_update:
    lda #%10100000 ; Set PPU increment back to 1
    sta PPUCTRL
    rts

update_mines_display:
    lda #%10100100
    sta PPUCTRL ; Set PPU increment to 32
    lda mines_digits_buffer ; Update ones
    cmp #$FF
    beq end_mines_display_update
    tay
    lda #$20
    sta $02
    lda #$86
    sta $03
    jsr draw_digit
    lda #$FF
    sta mines_digits_buffer
    lda mines_digits_buffer + 1 ; Update tens
    cmp #$FF
    beq end_mines_display_update
    tay
    lda #$20
    sta $02
    lda #$84
    sta $03
    jsr draw_digit 
    lda #$FF
    sta mines_digits_buffer + 1
    lda mines_digits_buffer + 2 ; Update hundreds
    cmp #$FF
    beq end_mines_display_update
    tay
    lda #$20
    sta $02
    lda #$82
    sta $03
    jsr draw_digit 
    lda #$FF
    sta mines_digits_buffer + 2
    end_mines_display_update:
    lda #%10100000 ; Set PPU increment back to 1
    sta PPUCTRL
    rts

draw_digit: ; Y register is digit to draw, - is $0A
            ; Address of top left tile in $02 and $03
    lda DigitTableLow, y
    sta $00
    lda DigitTableHigh, y
    sta $01
    lda $02
    sta PPUADDR
    lda $03
    sta PPUADDR
    ldy #$00
:
    lda ($00), y
    sta PPUDATA
    iny
    cpy #$04
    bne :-
    lda $02
    sta PPUADDR
    inc $03
    lda $03
    sta PPUADDR
:
    lda ($00), y
    sta PPUDATA
    iny
    cpy #$08
    bne :-
    rts

draw_mario: ; Clobbers X
    ; Todo: Actual logic to figure out which expression to use
    ; Store index in X register
    ldx #$09
    stx OAMBUFFER + (4 * 4) + 1
    inx
    inx
    stx OAMBUFFER + (4 * 6) + 1
    inx
    inx
    stx OAMBUFFER + (4 * 5) + 1
    inx
    inx
    stx OAMBUFFER + (4 * 7) + 1
    rts

controller_to_mouse_buttons:
    lda controller_input
    and #(BUTTON_LEFT | BUTTON_RIGHT)
    cmp #BUTTON_RIGHT
    bne :+
    lda #$02
    jmp after_left_right
    :
    cmp #BUTTON_LEFT
    bne :+
    lda #$FE
    jmp after_left_right
    :
    lda #$00
    after_left_right:
    sta mouse_delta_x
    lda controller_input
    and #(BUTTON_UP | BUTTON_DOWN)
    cmp #BUTTON_DOWN
    bne :+
    lda #$02
    jmp after_up_down
    :
    cmp #BUTTON_UP
    bne :+
    lda #$FE
    jmp after_up_down
    :
    lda #$00
    after_up_down:
    sta mouse_delta_y

    lda controller_input
    and #%00000111 ; Mask out all but A, B, and select
    sta mouse_state
    rts

read_controllers: ; Clobbers $0A, A, and Y

    lda #$01
    sta JOY1
    lda controller_input
    and #BUTTON_START ; Check the start button
    beq :+
    lda controller_input_prev
    and #BUTTON_START ; Check that we didn't press the start button on the last frame
    bne :+
    lda JOY2 ; Change sensitivity if the start button is pressed
    :
    lda #$00
    sta JOY1

    lda controller_input
    sta controller_input_prev
    lda mouse_state
    sta mouse_state_prev
    lda #$01
    sta controller_input

    read_loop:
    lda JOY1
    lsr
    rol controller_input
    bcc read_loop

    lda snes_mouse_enabled ; Check if there is a mouse
    bne :+
    jmp controller_to_mouse_buttons ; If not, use the controller as a mouse
    :

    lda #$01
    sta scratch + $A
    mouse_read_loop_1: ; First read shouldn't matter because it should be all 0s
    lda JOY2
    lsr
    rol scratch + $A
    nop ; Need at least 14 cycles between reads for Hyper Click mice
    nop
    bcc mouse_read_loop_1

    lda scratch + $A
    beq :+
    jmp controller_to_mouse_buttons ; Still checking the first read though
    :

    lda #$01
    sta scratch + $A
    mouse_read_loop_2:
    lda JOY2
    lsr
    rol scratch + $A
    nop
    nop
    bcc mouse_read_loop_2

    lda scratch + $A
    and #%00001111
    cmp #%00000001 ; Check signature to see if it's a mouse
    beq :+
    jmp controller_to_mouse_buttons
    :

    lda controller_input
    and #BUTTON_SELECT ; Get the select (shift) button
    sta mouse_state

    lda scratch + $A ; Get the mouse buttons
    .repeat 6 ; Get last two bits
    lsr
    .endrepeat
    
    ora mouse_state
    sta mouse_state

    lda #$01
    sta scratch + $A
    mouse_read_loop_3:
    lda JOY2
    lsr
    rol scratch + $A
    nop
    nop
    bcc mouse_read_loop_3

    lda scratch + $A ; Get Y displacement
    bpl :+ ; Convert to two's complement
    eor #$7F
    sec
    adc #$00
    :
    sta mouse_delta_y

    lda #$01
    sta scratch + $A
    mouse_read_loop_4:
    lda JOY2
    lsr
    rol scratch + $A
    nop
    nop
    bcc mouse_read_loop_4

    lda scratch + $A ; Get X displacement
    bpl :+ ; Convert to two's complement
    eor #$7F
    sec
    adc #$00
    :
    sta mouse_delta_x

    rts

update_mouse_position:
    clc ; Update mouse X
    lda mouse_delta_x ; Logic to make sure it doesn't wrap
    bpl check_positive_x
    adc mouse_display_x
    bcs end_x
    lda #$00
    jmp end_x
    check_positive_x:
    adc mouse_display_x
    bcc end_x
    lda #$FF
    end_x:
    sta mouse_display_x

    clc ; Update mouse y
    lda mouse_delta_y
    bpl check_positive_y
    adc mouse_display_y
    bcs end_y
    lda #$00
    jmp end_y
    check_positive_y:
    adc mouse_display_y
    bcc end_y
    lda #$FF
    end_y:
    sta mouse_display_y
    rts

update_minefield_blank:
    lda #%00000000
    sta PPUMASK
    begin_update_minefield_blank:
    lda PPUSTATUS
    lda #$21
    sta PPUADDR
    lda #$3E
    sta PPUADDR
    lda #<minefield_tiles
    sta scratch
    lda #>minefield_tiles
    sta scratch + 1
    ldx #$00
    :
    lda PPUDATA
    lda PPUDATA
    lda PPUDATA
    lda PPUDATA
    ldy #$00
    :
    lda (scratch), y
    sta PPUDATA
    iny
    cpy #28
    bcc :-
    inx
    cpx #18
    beq :+
    lda scratch
    clc
    adc #28
    sta scratch
    lda scratch + 1
    adc #$00
    sta scratch + 1
    jmp :--
    :
    
    lda #$23 ; Push attributes
    sta PPUADDR
    lda #$D0
    sta PPUADDR
    lda #<minefield_attributes
    sta scratch
    lda #>minefield_attributes
    sta scratch + 1
    ldx #$00
    ldy #$00
    :
    lda (scratch), y
    sta PPUDATA
    iny
    cpy #8
    bne :-
    inx
    cpx #5
    beq :+
    ldy #$00
    lda scratch
    clc
    adc #$08
    sta scratch
    jmp :-
    :
    lda #$00
    sta minefield_update_row
    ldy #$2B ; Load regular cursor
    jsr update_cursor_sprite
    lda #%00011110  ; Enable rendering
    sta PPUMASK
    lda PPUSTATUS
    lda #$00
    sta PPUADDR
    ldx #$4E
    stx PPUSCROLL
    ldx #$07
    inc $8000, x
    sta PPUSCROLL
    lda #$20
    sta PPUADDR
    nop
    rts

update_minefield:   ; Current row in X
                    ; Clobbers $00, $01, $09, A, and Y
    lda screen_update_setting
    beq begin_update_minefield
    jmp update_minefield_blank
    begin_update_minefield:
    lda minefield_update_row
    cmp #$01
    beq :+
    jmp grid_pop_slide
    :
    lda PPUSTATUS ; This function is pretty inefficient
    lda MinefieldRowTileStartHigh, x ; The pop slide is way more efficient
    sta PPUADDR ; But we can still use this for updating the first row
    lda MinefieldRowTileStartLow, x ; It's at least good enough for that
    sta PPUADDR
    lda minefield_update_current_tile
    sta scratch
    lda minefield_update_current_tile + 1
    sta scratch + 1
    ldy #$00
    :
    lda (scratch), y
    sta PPUDATA
    iny
    cpy #28
    bcc :-
    lda MinefieldRowTileStartHigh, x
    sta PPUADDR
    lda MinefieldRowTileStartLow, x
    clc
    adc #$20
    sta PPUADDR
    lda minefield_update_current_tile
    clc
    adc #28
    sta scratch
    lda minefield_update_current_tile + 1
    adc #$00
    sta scratch + 1
    ldy #$00
    :
    lda (scratch), y
    sta PPUDATA
    iny
    cpy #28
    bcc :-

    lda scratch ; Update where we left off
    clc
    adc #28
    sta minefield_update_current_tile
    lda scratch + 1
    adc #$00
    sta minefield_update_current_tile + 1

    lda #$23 ; Push attributes
    sta PPUADDR
    lda MinefieldRowAttributeStartLow, x
    sta PPUADDR
    lda minefield_update_current_attribute
    sta scratch
    lda #>minefield_attributes ; This relies on the entire attribute table being in one page, will break otherwise
    sta scratch + 1
    ldy #$00
    :
    lda (scratch), y
    sta PPUDATA
    iny
    cpy #8
    bcc :-

    lda minefield_update_current_attribute ; Update where we left off
    clc
    adc #$08
    sta minefield_update_current_attribute
    inc minefield_update_row

    ldy #$2D ; Load hourglass for cursor sprite
    jsr update_cursor_sprite
    rts

grid_pop_slide:
    tsx 
    stx scratch + 9 ; Store stack pointer
    ldx #$FF ; Move stack
    txs
    lda PPUSTATUS
    pla ; Get address for tiles
    sta PPUADDR
    pla
    sta PPUADDR
    .repeat 4
    .repeat 28 ; Load tiles
    pla
    sta PPUDATA
    .endrepeat
    .repeat 4
    lda PPUDATA ; Skip the 2 tiles at the end and 2 tiles at the beginning
    .endrepeat
    .endrepeat
    pla
    sta PPUADDR ; Get address for attributes
    pla
    sta PPUADDR
    .repeat 8
    pla
    sta PPUDATA ; Load attributes
    .endrepeat
    inc minefield_update_row
    inc minefield_update_row ; We've gone down two rows, so increment twice
    lda minefield_update_row
    cmp #10 ; Stop the process once we get past the 9th row
    bcc :+
    lda #$00
    sta minefield_update_row ; Set these variables back to what they were originally
    lda #<minefield_tiles
    sta minefield_update_current_tile
    lda #>minefield_tiles
    sta minefield_update_current_tile + 1
    lda #<minefield_attributes
    sta minefield_update_current_attribute
    ldy #$2B ; Load normal cursor sprite
    jmp :++
    :
    ldy #$2D ; Load hourglass for cursor sprite
    :
    ldx scratch + 9 ; Restore stack
    txs
    jsr update_cursor_sprite
    rts

update_vram:
    lda #$02 ; Push sprites to OAM
    sta OAMDMA
    lda minefield_update_row ; Check if we have to update the minefield
    beq :+
    tax
    jsr update_minefield
    lda screen_update_setting
    beq end_update_vram
    rts
    :
    ldy #$2B ; Load regular cursor sprite
    jsr update_cursor_sprite
    lda frame_count
    and #$01 ; Update timer and mines on alternating frames
    bne :+
    jsr update_timer_display
    jmp :++
    :
    jsr update_mines_display
    :
    jsr draw_mario
    end_update_vram:
    bit PPUSTATUS ; Set scroll
    lda #$00
    sta PPUSCROLL
    lda #8
    sta PPUSCROLL
    rts

push_grid_tiles_to_stack: ; Clobbers $00, $01, A, X, and Y
    ldx minefield_update_row ; Load pointer to row in nametable
    lda MinefieldRowTileStartHigh, x 
    sta $0100
    lda MinefieldRowTileStartLow, x
    sta $0100 + 1
    ldy #$00
    lda minefield_update_current_tile ; Load tile data
    sta scratch
    lda minefield_update_current_tile + 1
    sta scratch + 1
    :
    lda (scratch), y
    sta $0100 + 2, y
    iny
    cpy #112 ; Loading 4 rows of tiles
    bne :-
    clc
    lda minefield_update_current_tile ; Update pointer to tile data
    adc #112
    sta minefield_update_current_tile
    lda minefield_update_current_tile + 1
    adc #$00
    sta minefield_update_current_tile + 1
    lda #$23 ; Load pointer to attribute in nametable
    sta $0100 + 2 + 112
    lda MinefieldRowAttributeStartLow, x
    sta $0100 + 2 + 112 + 1
    ldy #$00
    lda minefield_update_current_attribute ; Load attribute data
    sta scratch
    lda #>minefield_attributes ; This relies on the entire attribute table being in one page, will break otherwise
    sta scratch + 1
    :
    lda (scratch), y
    sta $0100 + 2 + 112 + 2, y
    iny
    cpy #8
    bne :-
    clc
    lda minefield_update_current_attribute ; Update pointer to attributes
    adc #8
    sta minefield_update_current_attribute
    rts

buffer_tiles: ; Clobbers $02 and A
    sta scratch + 2
    lda num_tiles_buffered
    cmp #TILE_UPDATE_BUFFER_SIZE 
    bcc :++
    bne :+
    inc num_tiles_buffered
    :
    rts
    :
    tay
    lda scratch + 2
    sta tile_update_buffer, y
    inc num_tiles_buffered
    ldy #$00
    rts

.macro push_if_openable
    sta scratch
    tax
    lda (scratch), y
    tay
    and #(FLAG_MASK | OPENED_MASK)
    bne :+
    tya
    ldy #$00
    ora #OPENED_MASK
    sta (scratch), y
    txa
    pha
    :
    ldy #$00
    txa
.endmacro

open_tile: ; Clobbers $00, $01, $02, X, Y, and A
    ldy #>minefield_row_0
    sty scratch + 1
    tsx 
    stx tile_stack_begin
    ldy #$00
    sta scratch
    tax
    lda (scratch), y
    ora #OPENED_MASK
    sta (scratch), y
    txa
    jmp @after_pull
    @begin:
    tsx
    cpx tile_stack_begin
    bne @after_done_check
    rts
    @after_done_check:
    pla
    @after_pull:
    tax
    cmp #$90
    bcs @begin
    and #%00001111
    cmp #$0E
    bcs @begin
    stx scratch
    lda (scratch), y
    tax
    and #MINE_MASK
    beq :+
    lda #GAME_OVER
    sta game_state
    txa
    ora #CLICKED_MINE_MASK
    jmp :++
    :
    txa
    :
    lda scratch
    jsr buffer_tiles
    txa
    and #DIGIT_MASK
    bne @begin
    lda scratch
    sec
    sbc #$11
    push_if_openable
    clc
    adc #$01
    push_if_openable
    clc
    adc #$01
    push_if_openable
    clc
    adc #$0E
    push_if_openable
    clc
    adc #$02
    push_if_openable
    clc
    adc #$0E
    push_if_openable
    clc
    adc #$01
    push_if_openable
    clc
    adc #$01
    push_if_openable
    jmp @begin

update_tilemap:
    
    rts

nmi:
    pha ; Push registers to stack
    txa
    pha
    tya
    pha

    inc frame_count

    lda #%00011110  ; Enable rendering
    sta PPUMASK

    tsx
    lda $104, x
    and #%00001000 ; Check decimal flag
    bne after_early_exit
    ldy #$2D ; Load hourglass for cursor sprite
    jsr update_cursor_sprite
    jsr read_controllers
    jsr update_mouse_position
    lda game_state
    bne :+ ; Shuffle the mines on every frame the game hasn't started
    lda currently_shuffling_mines
    bne :++
    jsr shuffle_mines
    jmp :++
:
    jsr rand ; Increment the RNG on every frame after the game has started
    cmp #$01
    bne :+ ; Skip incrementing timer unless the game is started
    jsr increment_timer
:
    bit PPUSTATUS ; Set scroll
    lda #$00
    sta PPUSCROLL
    lda #8
    sta PPUSCROLL


    pla ; Pop registers from stack
    tay
    pla
    tax
    pla
    rti
    after_early_exit:
    cld
    
    jsr update_vram
    jsr rand
    lda mouse_display_x
    sta mouse_x
    .repeat 4 ; Get grid position
    lsr
    .endrepeat
    sec
    sbc #(GRID_X / 2)
    sta mouse_down_x

    lda mouse_display_y
    sec
    sbc #$07 ; Account for vertical scroll
    sta mouse_y
    .repeat 4 ; Get grid position
    lsr
    .endrepeat
    sec
    sbc #(GRID_Y / 2)
    sta mouse_down_y

    jsr read_controllers
    jsr update_mouse_position
    lda game_state ; Check current game state
    bne :++ ; Shuffle the mines on every frame the game hasn't started
    lda controller_input ; If the game isn't started yet, start the game when start is pressed
    and #BUTTON_START
    beq :+
    lda #GAME_IN_PROGRESS
    sta game_state
    jsr init_minefield
    jmp :+++
    :
    jsr shuffle_mines
    jmp :++
    :
    jsr rand ; Increment the RNG on every frame after the game has started
    cmp #$01
    bne :+ ; Skip incrementing timer unless the game is started
    jsr increment_timer
    :
    lda controller_input
    and #BUTTON_A ; Check the A button
    beq :+
    lda controller_input_prev
    and #BUTTON_A ; Check that we didn't press the A button on the last frame
    bne :+
    lda #$47 ; Open tile
    jsr open_tile
    :

    lda minefield_update_row
    cmp #$02 ; Push tiles to stack for every row after the first
    bcc :+
    jsr push_grid_tiles_to_stack
    :

    pla ; Pop registers from stack
    pla ; Probably not necessary to restore the registers?
    pla
    sed
    rti

Digit0:
    .byte $0F, $05, $09, $1A
    .byte $10, $13, $16, $1B

Digit1:
    .byte $01, $06, $0A, $0C
    .byte $11, $13, $16, $1C

Digit2:
    .byte $12, $07, $17, $1A
    .byte $10, $14, $0B, $0D

Digit3:
    .byte $12, $07, $18, $0E
    .byte $10, $14, $19, $1B

Digit4:
    .byte $02, $15, $18, $0C
    .byte $11, $14, $19, $1C

Digit5:
    .byte $03, $15, $18, $0E
    .byte $04, $08, $19, $1B

Digit6:
    .byte $03, $15, $17, $1A
    .byte $04, $08, $19, $1B

Digit7:
    .byte $12, $06, $0A, $0C
    .byte $10, $13, $16, $1C

Digit8:
    .byte $0F, $15, $17, $1A
    .byte $10, $14, $19, $1B

Digit9:
    .byte $0F, $15, $18, $0E
    .byte $10, $14, $19, $1B

DigitMinus:
    .byte $01, $07, $18, $0C
    .byte $80, $08, $0B, $81

.define DigitTable Digit0, Digit1, Digit2, Digit3, Digit4, Digit5, Digit6, Digit7, Digit8, Digit9, DigitMinus

DigitTableLow:
    .lobytes DigitTable

DigitTableHigh:
    .hibytes DigitTable

MinefieldRowTileStartHigh:
    .byte $21, $21, $21, $21, $22, $22, $22, $22, $23, $23

MinefieldRowTileStartLow:
    .byte $02, $42, $82, $C2, $02, $42, $82, $C2, $02, $42

MinefieldRowAttributeStartLow:
    .byte $D0, $D0, $D8, $D8, $E0, $E0, $E8, $E8, $F0, $F0

Rows1to7:
    .byte $20, $21, $22, $22, $22, $22, $22, $22, $23, $24, $24, $24, $24, $25, $26, $26, $26, $26, $24, $24, $24, $24, $24, $27, $22, $22, $22, $22, $22, $22, $28, $29
    .byte $20, $2A, $0F, $10, $01, $11, $03, $04, $2B, $2C, $2C, $2C, $2C, $20, $2D, $2E, $2E, $2F, $30, $2C, $2C, $2C, $2C, $31, $0F, $10, $0F, $10, $0F, $10, $32, $29
    .byte $20, $2A, $05, $13, $06, $13, $15, $08, $2B, $2C, $2C, $2C, $2C, $20, $33, $2C, $2C, $34, $30, $2C, $2C, $2C, $2C, $31, $05, $13, $05, $13, $05, $13, $32, $29
    .byte $20, $2A, $09, $16, $0A, $16, $18, $19, $2B, $2C, $2C, $2C, $2C, $20, $33, $2C, $2C, $34, $30, $2C, $2C, $2C, $2C, $31, $09, $16, $09, $16, $09, $16, $32, $29
    .byte $20, $2A, $1A, $1B, $0C, $1C, $0E, $1B, $2B, $2C, $2C, $2C, $2C, $20, $35, $36, $36, $37, $30, $2C, $2C, $2C, $2C, $31, $1A, $1B, $1A, $1B, $1A, $1B, $32, $29
    .byte $20, $3B, $3C, $3C, $3C, $3C, $3C, $3C, $3D, $3E, $3E, $3E, $3E, $3E, $3F, $3F, $3F, $3F, $40, $3E, $3E, $3E, $3E, $41, $3C, $3C, $3C, $3C, $3C, $3C, $42, $29
    .byte $43, $44, $45, $45, $45, $45, $45, $45, $45, $45, $45, $45, $45, $45, $45, $45, $45, $45, $45, $45, $45, $45, $45, $45, $45, $45, $45, $45, $45, $45, $46, $47

TilePalettes:
    .byte $02, $02, $02, $02, $02, $02, $02, $02, $02, $02, $02, $02, $02, $02, $02, $02
    .byte $02, $02, $02, $02, $02, $02, $02, $02, $02, $02, $02, $02, $02, $02, $02, $02
    .byte $02, $03, $02, $01, $03, $01, $03, $01, $03, $03, $03, $03, $03, $03, $03, $03
    .byte $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01
    .byte $02, $02, $02, $02, $02, $02, $02, $02, $02, $02, $02, $02, $02, $02, $02, $02
    .byte $02, $02, $02, $02, $02, $02, $02, $02, $02, $02, $02, $02, $02, $02, $02, $02
    .byte $02, $03, $02, $01, $03, $01, $03, $01, $03, $03, $03, $03, $03, $03, $03, $03
    .byte $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01
    .byte $02, $02, $02, $02, $02, $02, $02, $02, $02, $02, $02, $02, $02, $02, $02, $02
    .byte $02, $02, $02, $02, $02, $02, $02, $02, $02, $02, $02, $02, $02, $02, $02, $02
    .byte $02, $03, $02, $01, $03, $01, $03, $01, $03, $03, $03, $03, $03, $03, $03, $03
    .byte $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01
    .byte $02, $02, $02, $02, $02, $02, $02, $02, $02, $02, $02, $02, $02, $02, $02, $02
    .byte $02, $02, $02, $02, $02, $02, $02, $02, $02, $02, $02, $02, $02, $02, $02, $02
    .byte $02, $03, $02, $01, $03, $01, $03, $01, $03, $03, $03, $03, $03, $03, $03, $03
    .byte $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01

TilePalettesGameOver:
    .byte $02, $02, $02, $02, $02, $02, $02, $02, $02, $02, $02, $02, $02, $02, $02, $02
    .byte $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01
    .byte $02, $03, $02, $01, $03, $01, $03, $01, $03, $03, $03, $03, $03, $03, $03, $03
    .byte $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01
    .byte $02, $02, $02, $02, $02, $02, $02, $02, $02, $02, $02, $02, $02, $02, $02, $02
    .byte $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01
    .byte $02, $03, $02, $01, $03, $01, $03, $01, $03, $03, $03, $03, $03, $03, $03, $03
    .byte $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01
    .byte $02, $02, $02, $02, $02, $02, $02, $02, $02, $02, $02, $02, $02, $02, $02, $02
    .byte $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01
    .byte $02, $03, $02, $01, $03, $01, $03, $01, $03, $03, $03, $03, $03, $03, $03, $03
    .byte $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01
    .byte $02, $02, $02, $02, $02, $02, $02, $02, $02, $02, $02, $02, $02, $02, $02, $02
    .byte $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01
    .byte $02, $03, $02, $01, $03, $01, $03, $01, $03, $03, $03, $03, $03, $03, $03, $03
    .byte $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01

.define TileToNametableAddressTable \
    $2142, $2144, $2146, $2148, $214A, $214C, $214E, $2150, $2152, $2154, $2156, $2158, $215A, $215C, $0000, $0000, \
    $2182, $2184, $2186, $2188, $218A, $218C, $218E, $2190, $2192, $2194, $2196, $2198, $219A, $219C, $0000, $0000, \
    $21C2, $21C4, $21C6, $21C8, $21CA, $21CC, $21CE, $21D0, $21D2, $21D4, $21D6, $21D8, $21DA, $21DC, $0000, $0000, \
    $2202, $2204, $2206, $2208, $220A, $220C, $220E, $2210, $2212, $2214, $2216, $2218, $221A, $221C, $0000, $0000, \
    $2242, $2244, $2246, $2248, $224A, $224C, $224E, $2250, $2252, $2254, $2256, $2258, $225A, $225C, $0000, $0000, \
    $2282, $2284, $2286, $2288, $228A, $228C, $228E, $2290, $2292, $2294, $2296, $2298, $229A, $229C, $0000, $0000, \
    $22C2, $22C4, $22C6, $22C8, $22CA, $22CC, $22CE, $22D0, $22D2, $22D4, $22D6, $22D8, $22DA, $22DC, $0000, $0000, \
    $2302, $2304, $2306, $2308, $230A, $230C, $230E, $2310, $2312, $2314, $2316, $2318, $231A, $231C, $0000, $0000, \
    $2342, $2344, $2346, $2348, $234A, $234C, $234E, $2350, $2352, $2354, $2356, $2358, $235A, $235C, $0000, $0000 \

TileToNametableAddressTableHigh:
    .hibytes TileToNametableAddressTable

TileToNametableAddressTableLow:
    .lobytes TileToNametableAddressTable

Palettes:
    ; Background Palette
    .byte $10, $06, $0F, $16
    .byte $10, $00, $0F, $16
    .byte $10, $00, $20, $1A
    .byte $10, $00, $02, $1C

    ; Sprite Palette
    .byte $10, $36, $07, $16
    .byte $10, $36, $07, $26
    .byte $10, $10, $00, $0F
    .byte $10, $20, $00, $0F

.segment "CHARS"
    .incbin "bg.bin"
    .incbin "sprites.bin"