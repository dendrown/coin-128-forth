; COIN-128: Coin-128 Forth for the Commodore-128
;
; asmsyntax=asm68k  (6510/8502 ASM, but better syntax highlighting in vim)
;-----------------------------------------------------------------------------
.include "c128.inc"         ; cc65 definitions [origin: Elite128]
.include "c128-defs.inc"    ; More definitions for the C128

; Zero-page registers

; Constants
.define APP_TITLE   "coin-128 forth"
.define APP_VERSION "0.1"

.macro cstring s            ; Forth counted string
    .byte .strlen(s), s     ; (not a NULL-terminated C string)
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


;-----------------------------------------------------------------------------
.org $1C01                  ; Default for c128-asm in cl65 config
                            ; (why not $1C00?)
.segment "STARTUP"
.segment "INIT"
.segment "CODE"

main:
    cld                     ; No BCD operations at all
    cprintln welcome
    jsr CROUT
    cprintln silliness1
    cprintln silliness2
done:
    jsr CROUT
    rts


;-----------------------------------------------------------------------------
welcome:    cstring .sprintf("%s v%s", APP_TITLE, APP_VERSION)
silliness1: cstring "forth? maybe zeroth..."
silliness2: cstring "we don't do anything yet!"
