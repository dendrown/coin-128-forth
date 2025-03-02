; Commodore-128 BASIC access from ML
;
; asmsyntax=asm68k  (6502/8502 ASM, but better syntax highlighting in vim)
;-----------------------------------------------------------------------------
.ifndef __C128_BASIC_INC
__C128_BASIC_INC = 1

;-----------------------------------------------------------------------------
; Hooks to BASIC
CRUPOUT     := $B8AD    ; Output carriage return + cursor up
CROUT       := $B8B4    ; Output carriage return
JBSOUT      := $FFD2    ; Sends a byte to the current output logical file

;-----------------------------------------------------------------------------
; BASIC tokens
; @ref: https://www.c64-wiki.com/wiki/BASIC_token
BT_REM      = $8F
BT_SYS      = $9E

.endif
