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
    jsr CROUT
    cprintln silliness2
    jsr CROUT
    rts

;-----------------------------------------------------------------------------
; Forth vocabulary:
; Word order corresponds to fig6502.
; @ref https://github.com/jefftranter/6502/blob/master/asm/fig-forth/fig6502.asm
orig:
W0000:
    .byte $00               ; VOCABULARY: start token (end of reverse search)
    .word $0000

FORTH_WORD ".s"             ; ------------------------------------------------
dot_s:                      ; .S (_ -- _)
    stx W                   ; We need XSAVE for stack/print exchange, so use W
    ldx #$00                ; Start as if the PSTACK were empty
dot_s_loop:
    cpx W                   ; Real value of PSP (X) ?
    beq dot_s_done
    dex
    dex
    jsr sub_dot
    dex                     ; Unpop the printed value on the PSTACK
    dex
    jmp dot_s_loop
dot_s_done:
    ldx W                   ; Restore real PSP (X) value
    jmp next

FORTH_WORD ">r"             ; ------------------------------------------------
to_r:                       ; >R (n -- )
    lda PSTACK+1,x          ; Transfer HI byte (BACKWARDS on RSTACK)
    pha
    lda PSTACK,x            ; Transfer LO byte (BACKWARDS on RESTACK)
    pha
    jmp pop

FORTH_WORD "r>"             ; ------------------------------------------------
r_from:                     ; >R ( -- n)
    pla                     ; Prep LO byte
    jmp push                ; Push takes HI byte from RSTACK

FORTH_WORD "+"              ; ------------------------------------------------
plus:
    clc
    lda PSTACK,x            ; Lo byte of TOP
    adc PSTACK+2,x          ; Lo byte of TOP-1
    sta PSTACK+2,x
    lda PSTACK+1,x          ; Hi byte of TOP
    adc PSTACK+3,x          ; Hi byte of TOP-1
    sta PSTACK+3,x
    jmp pop                 ; Result in TOP-1...drop TOP

FORTH_WORD "-"              ; ------------------------------------------------
minus:
    sec
    lda PSTACK+2,x          ; Lo byte of TOP-1
    sbc PSTACK,x            ; Lo byte of TOP
    sta PSTACK+2,x
    lda PSTACK+3,x          ; Hi byte of TOP-1
    sbc PSTACK+1,x          ; Hi byte of TOP
    sta PSTACK+3,x
    jmp pop                 ; Result in TOP-1...drop TOP

FORTH_WORD "over"           ; ------------------------------------------------
over:                       ; OVER (n1 n2 -- n1 n2 n1)
    lda PSTACK+2,x
    pha
    lda PSTACK+3,x
    jmp push

FORTH_WORD "drop"           ; ------------------------------------------------
drop:                       ; DROP (n -- )
pop:                        ; TODO: move to poptwo/pop in (do) W0185
    inx
    inx
    jmp next

FORTH_WORD "swap"           ; ------------------------------------------------
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

FORTH_WORD "dup"            ; ------------------------------------------------
dup:                        ; PSTACK [1300..|TOP=0|1|......13fe:13ff]
    lda PSTACK,x
    pha
    lda PSTACK+1,x
    jmp push

FORTH_WORD "constant"       ; ------------------------------------------------
;   .word docol
;   .word creat
;   .word smudg
;   .word lbrac
;   .word semis
const:
    ldy #$03                ; IP is at code past link, offset by `jmp const`
    lda (W),y
    pha
    iny
    lda (W),y
    jmp push

FORTH_WORD "user"           ; ------------------------------------------------
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

FORTH_WORD "1"              ; ------------------------------------------------
one:
    jmp const
    .word 1

FORTH_WORD "2"              ; ------------------------------------------------
two:
    jmp const
    .word 2

FORTH_WORD "3"              ; ------------------------------------------------
three:
    jmp const
    .word 3

FORTH_WORD "tib"            ; ------------------------------------------------
tib:
    jmp const
    .word TIBX

FORTH_WORD "1+"             ; ------------------------------------------------
one_plus:
    jmp enter
    .word one
    .word plus
    .word exit

FORTH_WORD "rot"            ; ------------------------------------------------
rot:                        ; ROT (n1 n2 n3 -- n2 n3 n1)
    jmp enter
    .word to_r
    .word swap
    .word r_from
    .word swap
    .word exit

FORTH_WORD "."              ; ------------------------------------------------
dot:                        ; TODO: Check for empty stack!
    jsr sub_dot
    jmp next
sub_dot:                    ; Subroutine called from . and .S
    inc PNTR                ; Give space so we don't overwrite the dot
    inc PNTR
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
    rts

W9997:
FORTH_WORD "noop"           ; ------------------------------------------------
noop:                       ; Debug word that does nothing
    jmp enter
    .word exit

W9998:
FORTH_WORD "~out"           ; ------------------------------------------------
word_out:
    .word *+2               ; FIXME: FAKE (second) WORD LINK
    lda WEND                ; Get the last space|CR
    cmp #C_SPACE            ; More before printing result line?
    beq interpret           ; Yes, keep processing more words
    jmp line_out            ; Process line
line_error:
    inc PNTR
    cprintln error
    jmp quit
line_out:
    inc PNTR
    cprint ok
    jsr CROUT
    jmp interpret           ; TODO: Handle interpretive/compile states

W9999:
FORTH_WORD "debug"          ; ------------------------------------------------
    jmp bye

;-----------------------------------------------------------------------------
; TODO: we are hanging out behind the BASIC stub for now. The kernel will
; move once we have a kernel to move and a memory layout to move it to.
coin:
    store_w APPAREA,UP      ; UP is PSP base for a single-task Forth
    ldx #$00                ; X: Empty PSTACK
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
    store_w W9999,DP        ; Initialize start of dictionary
    store_w word_out,IP     ; TODO: Forthify this hack!
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
    sty WLEN                ; Save length of interpreted word
    sta WEND                ; Save space|CR
    lda #$00
    sta WORD,y              ; Terminate word for -FIND
    jmp find
bye:
    rts

;-----------------------------------------------------------------------------
find:                       ; TODO: Generalize for tick & interpret
    ldy #$00
    lda (DP),y              ; Load count byte
    and #WLENMSK            ; Remove precedence bit
    beq find_no_more        ; Zero-length word never matches; end of dictionary
    sta COUNT               ; Save length for offset
    cmp WLEN                ; Check word length to avoid partials matching
    bne find_no_match       ; Dictionary & intepreted word lengths do NOT match
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
    sta W
    lda DP+1
    adc #$00                ; Handle carry for 16-bit + 8-bit addition
    sta W+1
    jmp (W)
find_no_more:
    jmp line_error          ; TODO: proper linkage into FORTH loop


enter_ml:
    store_w word_out,IP     ; TODO: ENTER_ML is TEMPORARY scaffolding
    rts
;
; ENTER is common across all colon definitions
;
enter:                      ; Common entry for all colon defs
    clc                     ; Push IP++ for where we came from on the RSTACK
    lda IP
    adc #$02                ; Increment pointer to next word as we stack it
    pha                     ; Stack BACKWARDS [__:LO] to combine ADD/PUSH
    lda IP+1
    adc #$00
    pha                     ; Stack BACKWARDS [HI:lo]
    clc                     ; Set IP <- first WORD address
    lda W                   ; W points to JMP ENTER in the current word
    adc #$03                ; skip past the JMP instruction
    sta IP
    lda W+1
    adc #$00
    sta IP+1
    jmp next
;
; EXIT represents the semicolon for all word definitions
;
exit:                       ; Terminate forth word thread
    pla
    sta IP+1                ; Remove from RSTACK BACKWARDS [HI:lo] (see: ENTER)
    pla
    sta IP                  ; Remove from RSTACK BACKWARDS [__:LO]
    jmp (IP)
;
; PUSH/PUT: parameter stack operations
;
push:                       ; PSTACK uses page 1300, grows from the top, down
    dex
    dex
put:
    sta PSTACK+1,X          ; Hi byte from A
    pla                     ; Lo byte from R-stack
    sta PSTACK,X
;
; NEXT is the address interpreter that moves from machine-level word to word
;
next:
    ldy #$00                ; Set W <- (IP)
    lda (IP),y
    sta W
    iny
    lda (IP),y
    sta W+1
    clc
    lda IP                  ; Set IP <- next word
    adc #$02                ; Advance IP
    sta IP
    lda IP+1
    adc #$00
    sta IP+1
    jmp (W)                 ; Execute DTC


;-----------------------------------------------------------------------------
.rodata

welcome:    cstring .sprintf("%s v%s", APP_TITLE, APP_VERSION)
ok:         cstring " ok"
error:      cstring "error!"
silliness1: cstring "forth? maybe zeroth..."
silliness2: cstring "we don't do anything yet!"
