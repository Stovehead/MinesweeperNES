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

.zeropage
    scratch: .res $10
    controller_last: .res 1
    controller_new: .res 1

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
    lda PPUSTATUS
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

    ldy #0
:
    lda Rows1and2, y
    sta PPUDATA
    iny
    cpy #96
    bne :-
    ldy #0
:
    lda Row3, y
    sta PPUDATA
    iny
    cpy #$80
    bne :-
    ldy #0
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

    lda #$00
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

    lda #$02
    sta OAMDMA
    lda #%00011110  ; Enable rendering
    sta PPUMASK

    tsx
    lda $104, x
    and #%00001000 ; Check decimal flag
    bne after_early_exit
    pla ; Pop registers from stack
    tay
    pla
    tax
    pla
    rti
    after_early_exit:

    cld

    jsr draw_mario

    sed
    pla ; Pop registers from stack
    pla
    pla
    rti

Rows1and2:
    .byte $20, $21, $22, $22, $22, $22, $22, $22, $23, $24, $24, $24, $24, $25, $26, $26, $26, $26, $24, $24, $24, $24, $24, $27, $22, $22, $22, $22, $22, $22, $28, $29
    .byte $20, $2A, $00, $00, $00, $00, $00, $00, $2B, $2C, $2C, $2C, $2C, $20, $2D, $2E, $2E, $2F, $30, $2C, $2C, $2C, $2C, $31, $00, $00, $00, $00, $00, $00, $32, $29
Row3:
    .byte $20, $2A, $00, $00, $00, $00, $00, $00, $2B, $2C, $2C, $2C, $2C, $20, $33, $2C, $2C, $34, $30, $2C, $2C, $2C, $2C, $31, $00, $00, $00, $00, $00, $00, $32, $29
Rows5and6and7:
    .byte $20, $2A, $00, $00, $00, $00, $00, $00, $2B, $2C, $2C, $2C, $2C, $20, $35, $36, $36, $37, $30, $2C, $2C, $2C, $2C, $31, $00, $00, $00, $00, $00, $00, $32, $29
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

  ; 000001000100001001111001