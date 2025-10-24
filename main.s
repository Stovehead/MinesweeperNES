PPUCTRL = $2000
PPUMASK = $2001
PPUSTATUS = $2002
OAMADDR = $2003
PPUSCROLL = $2005
PPUADDR = $2006
PPUDATA = $2007
APUDMC = $4010
OAMDMA = $4014
APUFRAMECOUNTER = $4017
OAMBUFFER = $0200
NUM_MINES = 15
GRID_WIDTH = 14
GRID_HEIGHT = 9

.zeropage
    scratch: .res $10
    controller_last: .res 1
    controller_new: .res 1
    mouse_state: .res 1
    mouse_flags: .res 1
    mouse_display_x: .res 1
    mouse_display_y: .res 1
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
    ldx #$FF    ; Set up stack
    txs         ;  .
    inx         ; now X = 0
    stx PPUCTRL	; disable NMI
    stx PPUCTRL ; disable rendering
    stx APUDMC  ; disable DMC IRQs

;; first wait for vblank to make sure PPU is ready
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

;; second wait for vblank, PPU is ready after this
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
    lda #$40
    sta PPUADDR
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
    ldx #28
    lda #$00
:
    sta PPUDATA
    dex
    bne :-
    lda #43
    sta PPUDATA
    lda #44
    sta PPUDATA
    iny
    cpy #19
    bne :--

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

    lda #$00 ; Is this necessary?
    sta OAMADDR

    bit PPUSTATUS ; Set scroll
    lda #$00
    sta PPUSCROLL
    lda #8
    sta PPUSCROLL

    lda #%10100000	; Enable NMI and set sprite size
    sta PPUCTRL

    sed ; Decimal flag used to see if frame finished
forever:
    jmp forever

increment_timer: ; Clobbers A, $00, $01, $02, $03
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
    lda #%10100100 ; Set PPU increment to 32
    sta PPUCTRL
    inc seconds_elapsed
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
    ldy seconds_elapsed + 1 ; Update hundreds place
    lda #$20
    sta $02
    lda #$98
    sta $03
    jsr draw_digit
    update_tens:
    lda seconds_elapsed ; Update tens place
    lsr
    lsr
    lsr
    lsr
    tay
    lda #$20
    sta $02
    lda #$9A
    sta $03
    jsr draw_digit
    update_ones:
    lda seconds_elapsed ; Update ones place
    and #%00001111
    tay
    lda #$20
    sta $02
    lda #$9C
    sta $03
    jsr draw_digit
    lda #%10100000 ; Set PPU increment back to 1
    sta PPUCTRL
    end_increment_timer:
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

nmi:
    pha ; Push registers to stack
    txa
    pha
    tya
    pha

    lda #$02 ; Push sprites to OAM
    sta OAMDMA
    lda game_state ; Check current game state
    cmp #$01
    bne :+ ; Skip incrementing timer unless the game is started
    jsr increment_timer
:
    lda #%00011110  ; Enable rendering
    sta PPUMASK

    tsx
    lda $104, x
    and #%00001000 ; Check decimal flag
    bne after_early_exit
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

    jsr draw_mario

    bit PPUSTATUS ; Set scroll
    lda #$00
    sta PPUSCROLL
    lda #8
    sta PPUSCROLL
    sed
    pla ; Pop registers from stack
    pla ; Probably not necessary to restore the registers?
    pla
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

Rows1to7:
    .byte $20, $21, $22, $22, $22, $22, $22, $22, $23, $24, $24, $24, $24, $25, $26, $26, $26, $26, $24, $24, $24, $24, $24, $27, $22, $22, $22, $22, $22, $22, $28, $29
    .byte $20, $2A, $0F, $10, $01, $11, $03, $04, $2B, $2C, $2C, $2C, $2C, $20, $2D, $2E, $2E, $2F, $30, $2C, $2C, $2C, $2C, $31, $0F, $10, $0F, $10, $0F, $10, $32, $29
    .byte $20, $2A, $05, $13, $06, $13, $15, $08, $2B, $2C, $2C, $2C, $2C, $20, $33, $2C, $2C, $34, $30, $2C, $2C, $2C, $2C, $31, $05, $13, $05, $13, $05, $13, $32, $29
    .byte $20, $2A, $09, $16, $0A, $16, $18, $19, $2B, $2C, $2C, $2C, $2C, $20, $33, $2C, $2C, $34, $30, $2C, $2C, $2C, $2C, $31, $09, $16, $09, $16, $09, $16, $32, $29
    .byte $20, $2A, $1A, $1B, $0C, $1C, $0E, $1B, $2B, $2C, $2C, $2C, $2C, $20, $35, $36, $36, $37, $30, $2C, $2C, $2C, $2C, $31, $1A, $1B, $1A, $1B, $1A, $1B, $32, $29
    .byte $20, $3B, $3C, $3C, $3C, $3C, $3C, $3C, $3D, $3E, $3E, $3E, $3E, $3E, $3F, $3F, $3F, $3F, $40, $3E, $3E, $3E, $3E, $41, $3C, $3C, $3C, $3C, $3C, $3C, $42, $29
    .byte $43, $44, $45, $45, $45, $45, $45, $45, $45, $45, $45, $45, $45, $45, $45, $45, $45, $45, $45, $45, $45, $45, $45, $45, $45, $45, $45, $45, $45, $45, $46, $47

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