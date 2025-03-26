; COIN-128: Coin-128 Forth for the Commodore-128
;
; asmsyntax=asm68k  (6510/8502 ASM, but better syntax highlighting in vim)
;-----------------------------------------------------------------------------
.include "c128.inc"         ; cc65 definitions [origin: Elite128]
.include "c128-defs.inc"    ; More definitions for the C128
.include "coin-defs.inc"    ; Coin-128 Forth definitions & macros

;ENTRY_LINK_NAME = 1        ; TODO: Rework struct with Coin-OP order (or NOT)

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
orig:
W0000:
W0902:
    cstring "user"          ; TODO: Rework struct with Coin-OP order (or NOT)
    .word 0
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

W1010:
W9999:
.ifdef ENTRY_LINK_NAME
    .word W000
    cstring "tib"           ; TODO: Rework struct with Coin-OP order (or NOT)
.else
    ;byte "ti",'b'|$80
    cstring "tib"           ; TODO: Rework struct with Coin-OP order (or NOT)
    .word W0000
.endif
tib:                        ; TODO: TIB needs to be handled as a constant
    jsr enter_ml            ; TODO: ENTER_ML is TEMPORARY scaffolding
    lda #<TIBX
    pha
    lda #>TIBX
    jmp push


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
                            ; TODO: 0 BLK !
interpret:
    store_w W9999,DP        ; TODO: Un-hardcode
    ldy #$00
charin:
    jsr JBASIN
    cmp #C_SPACE
    beq wordin
    cmp #C_RETURN
    beq wordin
    sta WORD,y
    iny                     ; Prep for next character
    cpy MAXWORD+1           ; Past single-word buffer?
    bne charin
    jmp errline
wordin:
    lda #$00
    sta WORD,y
    jmp find
    beq line                ; Process line without printing CR (yet)
errline:
    cprintln error
line:
    jsr CROUT
    zprintln WORD
    jsr CROUT
    jmp interpret           ; TODO: Handle interpretive/compile states

;-----------------------------------------------------------------------------
find:                       ; TODO: Generalize for tick & interpret
.ifdef ENTRY_LINK_NAME
    ldy #WORDOFF            ; Offset to word name
.else
    ldy #$00
.endif
    lda (DP),y              ; Load count byte
    and #WLENMSK            ; Remove precedence bit
    sta COUNT               ; Save length for offset
.ifdef ENTRY_LINK_NAME
    clc
    adc #WORDOFF            ; We'll be comparing characters in reverse
.endif
    tay
find_test_char:
    lda (DP),y              ; Load next char (working backwards)
    cmp WORD-1,y            ; Back up one to account for count byte
    bne find_no_match
    dey
.ifdef ENTRY_LINK_NAME
    cpy #WORDOFF            ; Backed up to the count byte again?
.endif
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
    jmp errline             ; TODO: proper linkage into FORTH loop


enter_ml:
    store_w line,IP         ; TODO: ENTER_ML is TEMPORARY scaffolding
    rts
;
; ENTER is common across all colon definitions
;
enter:
    nop                     ; TODO: common entry for all colon defs


push:                       ; PSTACK is on top of page 1300, grows down
    dex
    sta PSTACK,X            ; Hi byte in A
    pla                     ; Lo byte on R-stack
    dex
    sta PSTACK,X
;
; NEXT is the address interpreter that moves from machine-level word to word
;
next:
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
