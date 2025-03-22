; COIN-128: Coin-128 Forth for the Commodore-128
;
; asmsyntax=asm68k  (6510/8502 ASM, but better syntax highlighting in vim)
;-----------------------------------------------------------------------------
.include "c128.inc"         ; cc65 definitions [origin: Elite128]
.include "c128-defs.inc"    ; More definitions for the C128
.include "c128-basic.inc"   ; To auto-build BASIC launcher

;ENTRY_LINK_NAME = 1        ; TODO: Rework struct with Coin-OP order (or NOT)

.setcpu "6502"

; Zero-page registers       ; LO:HI 16-bit pointers
DP      := $40              ; 41:40 Dictionary pointer  (BASIC RESHO)
FREE42  := $42              ; 43:42                     (BASIC RESHO)
FREE44  := $44              ; 44                        (BASIC RESHO)
NO_WORD := $63              ; 63-69 Curr word: 7 chars  (FAC1:FACSGN:SGNFLAG)
WORD    := $6A              ; 6A-70 Curr word: 7 chars  (FAC2:ARGSGN:ARIFLAG)
UP      := $74              ; 74:75 Forth User Pointer  (BASIC AUTINC)
W       := $FA              ; FB:FA Forth Working (W) register
IP      := $FC              ; FD:FC Forth Interpreter Pointer (IP)
XSAVE   := $FE              ; Temporary for X register

; Constants
.define APP_TITLE   "coin-128 forth"
.define APP_VERSION "0.1"

TIBX    := $0B00            ; Initial Terminal Input Buffer
PSTACK  := APPAREA          ; Parameter stack page

MAXWORD = 6                 ; Maximum word length (note ZP buffer) : TODO: 32
WORDOFF = 2                 ; Offset to WORD in dictionary entry
WLENMSK = $1F               ; Word length bit mask
WPRCBIT = $10               ; Word precedence bit to indicate IMMEDIATE word


;-----------------------------------------------------------------------------
; Macros
.macro cstring s            ; Forth counted string
    .byte .strlen(s), s     ; (not a NULL-terminated C string)
.endmacro


.macro prc_cstring s        ; Forth counted string with precedence bit set
    .byte WPRCBIT|.strlen(s), s
.endmacro


.macro cprint s             ; Print a counted string
.local load_char
    ldy #$00
load_char:
    iny                     ; Text starts at offset 1
    lda s,y                 ; A = next character
    jsr JBSOUT              ; BASIC output(A)
    cpy s                   ; Y == strlen?
    bne load_char
.endmacro


.macro cprintln s           ; Print a counted string + CR
    cprint s
    jsr CROUT
.endmacro


.macro zprint s             ; Print a NUL-terminated string
.local load_char
.local zprint_done
    ldy #$00
load_char:
    lda s,y                 ; A = next character
    beq done
    jsr JBSOUT              ; BASIC output(A)
    iny
    jmp load_char
zprint_done:
    nop
.endmacro


.macro zprintln s           ; Print a NUL-terminated string + CR
    ziprint s
    jsr CROUT
.endmacro


.macro ziprint s            ; Print a NUL-terminated string via zero-page ptr
.local load_char
.local done
    ldy #$00
load_char:
    lda s,y                 ; A = next character
    beq done
    jsr JBSOUT              ; BASIC output(A)
    iny
    jmp load_char
done:
.endmacro


.macro ziprintln s          ; Print a NUL-term string + CR via zero-page ptr
    ziprint s
    jsr CROUT
.endmacro


.macro store_w w,addr       ; Store a 16-bit value in addr (LO:HI)
    lda #<(w)
    sta addr
    lda #>(w)
    sta addr+1
.endmacro


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
.ifdef ENTRY_LINK_NAME
    cstring "tib"           ; TODO: Rework struct with Coin-OP order (or NOT)
    .word 0
.else
    ;byte "ti",'b'|$80
    cstring "tib"           ; TODO: Rework struct with Coin-OP order (or NOT)
    .word 0
.endif
tib:                        ; TODO: TIB needs to be handled as a constant
    lda #<TIBX
    tay
    lda #>TIBX
    jsr push
    jmp next


;-----------------------------------------------------------------------------
; TODO: we are hanging out behind the BASIC stub for now. The kernel will
; move once we have a kernel to move and a memory layout to move it to.
coin:
    store_w APPAREA,UP      ; UP is PSP base for a single-task Forth
    ldx #$00                ; X: top of PSP
cold:
    ; EMPTY-BUFFERSR...     ; TODO: Set up for disk usage
    ; ORIG...               ; TODO: Set up memory
    ; FORTH...              ; TODO: Set FORTH vocabulary linkage
abort:
    ; FORTH...              ; TODO: Select FORTH trunk vocabulary
    ; DEFINITIONS...        ; TODO: Set CURRENT to CONTEXT
interpret:
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
    pha                     ; First character of word
    jsr find
    pla
;   cmp #C_RETURN
    beq line                ; Process line without printing CR (yet)
;   jsr JBSOUT
;   jmp interpret
errline:
    cprintln error
line:
    jsr CROUT
    zprintln WORD
    jsr CROUT
    rts

;-----------------------------------------------------------------------------
find:                       ; TODO: Generalize for tick & interpret
    store_w W0000,DP        ; TODO: Un-hardcode

.ifdef ENTRY_LINK_NAME
    ldy #WORDOFF            ; Offset to word name
.else
    ldy #$00
.endif
    lda (DP),y              ; Load count byte
    and #WLENMSK            ; Remove precedence bit
    sta COUNT               ; Save length in case we match
.ifdef ENTRY_LINK_NAME
    clc
    adc #WORDOFF            ; We'll be comparing characters in reverse
.endif
    tay
find_test_char:
    lda (DP),y              ; Load next char (working backwards)
    cmp WORD,y
    bne find_no_match
    dey
.ifdef ENTRY_LINK_NAME
    cpy #WORDOFF            ; Backed up to the count byte again?
.endif
    beq find_match
    jmp find_test_char
find_no_match:
    ldy #$01
    lda (DP),y              ; Only check high byte (no ZP dictionary)
    beq find_done           ; TODO: flag NOT-FOUND error
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
find_done:
    rts                     ; TODO: proper linkage into FORTH loop

enter:
    nop                     ; TODO: common entry for all colon defs

next:
    jmp find_done           ; TODO: move past single-shot trial

push:                       ; PSTACK is on top of page 1300, grows down
    dex
    sta PSTACK,X            ; Hi byte in A
    tya                     ; Lo byte in Y
    dex
    sta PSTACK,X
    rts


;-----------------------------------------------------------------------------
.rodata

welcome:    cstring .sprintf("%s v%s", APP_TITLE, APP_VERSION)
error:      cstring "error!"
silliness1: cstring "forth? maybe zeroth..."
silliness2: cstring "we don't do anything yet!"
