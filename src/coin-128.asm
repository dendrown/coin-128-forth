; COIN-128: Coin-128 Forth for the Commodore-128
;
; vim: set ft=asm_ca65:
;-----------------------------------------------------------------------------
.include "c128.inc"         ; cc65 definitions [origin: Elite128]
.include "c128-defs.inc"    ; More definitions for the C128
.include "coin-defs.inc"    ; Coin-128 Forth definitions & macros

.setcpu "6502"

; Constants
.define APP_TITLE   "coin-128 forth"
.define APP_VERSION "0.1"


;-----------------------------------------------------------------------------
.org $1C01                  ; ML starts after quick BASIC loader
.segment "STARTUP"
.segment "INIT"
.segment "CODE"

basic_2:                    ; Small BASIC launcher
    .word basic_4, 2        ; Point to next BASIC line
    .byte BT_REM, .sprintf(" %s", APP_TITLE), $00
basic_4:
    BASIC_SYS_TO_END = 13   ; Hard-code last BASIC line len (1-pass assembler)
    .word basic_end, 4      ; Last line points to null BASIC line
    .byte BT_SYS, .sprintf(" %d", (basic_4 + BASIC_SYS_TO_END)), $00
basic_end:
    .byte $00, $00          ; BASIC_SYS_TO_END covers these NULLs

main:
    cld                     ; No BCD operations at all
    cprintln welcome
    jsr CROUT
    cprintln silliness1
    jsr coin
done:
    cprintln silliness2
    jsr CROUT
    rts

;-----------------------------------------------------------------------------
; Forth vocabulary:
; Word labels (W9999) correspond to fig6502 labels (L999).
; @ref https://github.com/jefftranter/6502/blob/master/asm/fig-forth/fig6502.asm
orig:
W0000:                      ; ------------------------------------------------
    .byte $00               ; VOCABULARY: start token (end of reverse search)
    .word $0000

W0718:                      ; ------------------------------------------------
    cstring "swap"
    .word W0000
swap:                       ; PSTACK [1300..|TOP=0|1|2|3|..13fe:13ff]
                            ; NOTE: a ZP stack would allow us to save ops using
                            ;       ldy/sty instead of pha/pla for byte 3 -> 1.
    lda PSTACK+2,x          ; PUT prep: 2 -> RSTACK -> 0
    pha
    lda PSTACK+3,x          ; PUT prep: 3 -> RSTACK
    pha
    lda PSTACK,x            ; Move 0 -> 2
    sta PSTACK+2,x
    lda PSTACK+1,x          ; Move 1 -> 3
    sta PSTACK+3,x
    pla                     ; PUT prep:      RSTACK -> 1
    jmp put

W0733:                      ; ------------------------------------------------
    cstring "dup"
    .word W0718
dup:                        ; PSTACK [1300..|TOP=0|1|......13fe:13ff]
    lda PSTACK,x
    pha
    lda PSTACK+1,x
    jmp push

W0867:                      ; ------------------------------------------------
    cstring "constant"
    .word W0733
;   .word docol
;   .word creat
;   .word smudg
;   .word lbrac
;   .word semis
const:
    ldy #03                 ; IP is at code past link, offset by `jmp const`
    lda (IP),y
    pha
    iny
    lda (IP),y
    jmp push

W0902:                      ; ------------------------------------------------
    cstring "user"          ; TODO: Rework struct with Coin-OP order (or NOT)
    .word W0867
;   .word docol
;   .word const
;   .word pscod
douse:
    jsr enter_ml            ; TODO: ENTER_ML is TEMPORARY scaffolding
    ldy #$02
    clc
    lda (W),y
    adc UP
    pha
    lda #$00
    adc UP+1
    jmp push

W0928:                      ; ------------------------------------------------
    cstring "1"
    .word W0902
one:
    jmp const
    .word 1

W0936:
    cstring "2"
    .word W0928
two:
    jmp const
    .word 2

W0944:
    cstring "3"
    .word W0936
three:
    jmp const
    .word 3

W1010:                      ; ------------------------------------------------
    cstring "tib"           ; byte "ti",'b'|$80
    .word W0944
tib:
    jmp const
    .word TIBX

W3585:                      ; ------------------------------------------------
    cstring "."             ; DOT
    .word W1010
dot:
    jsr enter_ml            ; TODO: ENTER_ML is TEMPORARY scaffolding
                            ; TODO: Check for empty stack!
    lda PSTACK,x            ; Load lo-byte from top pstack cell
    tay
    inx
    lda PSTACK,x            ; Load hi-byte from top pstack cell
    inx
    stx XSAVE               ; Save pstack pointer now that value is popped
    tax                     ; X = hi-byte of cell
    tya                     ; A = lo-byte of cell
    jsr PRNTHEX4            ; TODO: print value according to BASE
    ldx XSAVE               ; Restore pstack pointer
    jmp next

W9999:                      ; ------------------------------------------------
    cstring "debug"
    .word W3585
    jmp bye

;-----------------------------------------------------------------------------
; TODO: we are hanging out behind the BASIC stub for now. The kernel will
; move once we have a kernel to move and a memory layout to move it to.
coin:
    store_w APPAREA,UP      ; UP is PSP base for a single-task Forth
    ldx #$00                ; X: top of PSP
cold:
    ; EMPTY-BUFFERS...      ; TODO: Set up for disk usage
    ; ORIG...               ; TODO: Set up memory
    ; FORTH...              ; TODO: Set FORTH vocabulary linkage
abort:
    ; FORTH...              ; TODO: Select FORTH trunk vocabulary
    ; DEFINITIONS...        ; TODO: Set CURRENT to CONTEXT
quit:
    lda #$00
    sta STATE
                            ; TODO: 0 BLK !
interpret:
    store_w W9999,DP        ; TODO: Un-hardcode
    ldy #$00
charin:
    jsr JBASIN
    cmp #C_SPACE
    beq word_in
    cmp #C_RETURN
    beq word_in
    sta WORD,y
    iny                     ; Prep for next character
    cpy MAXWORD+1           ; Past single-word buffer?
    bne charin
    jmp line_error
word_in:
    pha                     ; Save space|CR
    lda #$00
    sta WORD,y              ; Terminate word for -FIND
    jmp find
word_out:
    pla                     ; Get that last space|CR
    cmp #C_SPACE            ; More before printing result line?
    beq interpret           ; Yes, keep processing more words
    jmp line_done           ; Process line
line_error:
    cprintln error
line_done:
    jsr CROUT
    zprintln WORD
    jsr CROUT
    jmp interpret           ; TODO: Handle interpretive/compile states
bye:
    rts

;-----------------------------------------------------------------------------
find:                       ; TODO: Generalize for tick & interpret
    ldy #$00
    lda (DP),y              ; Load count byte
    and #WLENMSK            ; Remove precedence bit
    beq find_no_match       ; Zero-length word never matches; end of dictionary
    sta COUNT               ; Save length for offset
    tay
find_test_char:
    lda (DP),y              ; Load next char (working backwards)
    cmp WORD-1,y            ; Back up one to account for count byte
    bne find_no_match
    dey
    beq find_match
    jmp find_test_char
find_no_match:
    ldy COUNT               ; Get offset to link to previous word
    iny                     ; Offset = count byte + word length
    iny                     ; Start with hi-byte
    lda (DP),y              ; Only check hi-byte (no ZP dictionary)
    beq find_no_more        ; TODO: flag NOT-FOUND error
    pha                     ; Note hi-byte of next word
    dey
    lda (DP),y              ; Grab lo-byte of next word
    sta DP                  ; Store it for previous word
    pla                     ; Pull hi-byte again
    sta DP+1                ; Store it for previous word
    jmp find
find_match:
    clc
    lda COUNT
    adc #WORDOFF+1          ; Word char count + length + link will never carry
    adc DP                  ; Calc lo byte offset to dictionary code
    sta IP
    lda DP+1
    adc #$00                ; Handle carry for 16-bit + 8-bit addition
    sta IP+1
    jmp (IP)
find_no_more:
    jmp line_error          ; TODO: proper linkage into FORTH loop


enter_ml:
    store_w word_out,IP     ; TODO: ENTER_ML is TEMPORARY scaffolding
    rts
;
; ENTER is common across all colon definitions
;
enter:
    nop                     ; TODO: common entry for all colon defs


;
; PUSH/PUT: parameter stack operations
;
push:                       ; PSTACK is on top of page 1300, grows down
    dex
    dex
put:
    sta PSTACK+1,X          ; Hi byte in A
    pla                     ; Lo byte on R-stack
    sta PSTACK,X
;
; NEXT is the address interpreter that moves from machine-level word to word
;
next:
    lda STATE               ; Are we done with current iteration?
    bne next_cont           ; no! Keep processing...
    jmp word_out            ; TODO: Refactory per standard Forth loop
next_cont:
    ldy #$01
    lda IP,y                ; Transfer hi-byte of *IP
    sta W+1                 ; into W
    dey
    lda IP,y                ; Transfer lo-byte of *IP
    sta W                   ; into W
    clc
    lda IP                  ; Advance IP
    adc #$02
    sta IP
    bcc next_done
    inc IP+1                ; High byte ticked on IP+=2 operation
next_done:
    jmp (W)                 ; Execute DTC



;-----------------------------------------------------------------------------
.rodata

welcome:    cstring .sprintf("%s v%s", APP_TITLE, APP_VERSION)
error:      cstring "error!"
silliness1: cstring "forth? maybe zeroth..."
silliness2: cstring "we don't do anything yet!"
