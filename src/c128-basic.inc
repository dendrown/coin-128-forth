; Commodore-128 BASIC access from ML
;
; asmsyntax=asm68k  (6502/8502 ASM, but better syntax highlighting in vim)
;-----------------------------------------------------------------------------
.ifndef __C128_BASIC_INC
__C128_BASIC_INC = 1

;-----------------------------------------------------------------------------
; Zero Page Registers
AUTINC  = $74           ; 75:74 Step value for program line auto-increment

;-----------------------------------------------------------------------------
; Hooks to BASIC
INLIN       := $4F93    ; Input a line to the input buffer (BUF)

CRUPOUT     := $B8AD    ; Output carriage return + cursor up
CROUT       := $B8B4    ; Output carriage return

;-----------------------------------------------------------------------------
; BASIC tokens
; @ref: https://www.c64-wiki.com/wiki/BASIC_token
BT_REM      = $8F
BT_SYS      = $9E

.endif
