; COIN-128: Coin-128 Forth for the Commodore-128
;
; asmsyntax=asm68k  (6510/8502 ASM, but better syntax highlighting in vim)
;-----------------------------------------------------------------------------
.include "c128.inc"         ; cc65 definitions [origin: Elite128]
.include "c128-defs.inc"    ; More definitions for the C128

; Zero-page registers

; Constants
.define APP_TITLE   "coin-128 forth"
.define APP_VERSION "v0.1"

.macro cstring s            ; Forth counted string
    .byte .strlen(s), s     ; (not a NULL-terminated C string)
.endmacro


;-----------------------------------------------------------------------------
.org $1C01                  ; Default for c128-asm in cl65 config
                            ; (why not $1C00?)
.segment "STARTUP"
.segment "INIT"
.segment "CODE"

main:
    cld                     ; No BCD operations at all
    jsr cprint              ; FIXME: Macroize...!


done:
    rts


;-----------------------------------------------------------------------------
; CPRINT: output a counted string
;   A: address of counted string (the initial count byte)
cprint:
    ldy #$00
cprint_read:
    iny                     ; Text starts at offset 1
    lda title,y
    jsr JBSOUT              ; BASIC output(A)
    cpy title
    bne cprint_read
    rts


;-----------------------------------------------------------------------------
title:      cstring APP_TITLE
version:    cstring APP_VERSION
silliness:  cstring "Forth? Maybe zeroth...we don't do anything yet!"
