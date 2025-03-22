; Commodore-128 definitions
;
; asmsyntax=asm68k  (6502/8502 ASM, but better syntax highlighting in vim)
;-----------------------------------------------------------------------------
.ifndef __C128_DEFS_INC
__C128_MMAP_INC = 1

; C128 memory map (beyond cc65 c128 include)
PC          := $03      ; 03:04 Target address for JMPFAR and JSRFAR
COUNT       := $0D      ; Working storage for various routines

BUF         := $0200    ; Input buffer: BASIC & monitor (0200..02A1)
VICSCN      := $0400    ; VIC 40-col text screen
APPAREA     := $1300    ; Application program area (1300..1BFF)
RAMBOT      := $1C00    ; Start of BASIC text (kernal sets MEMBOT here)

;-----------------------------------------------------------------------------
; Hooks to kernal
JBASIN      := $FFCF    ; Retrieves a byte from the current input logical file
JBSOUT      := $FFD2    ; Sends a byte to the current output logical file
JGETIN      := $FFE4    ; Retrieves a character from current input device

;-----------------------------------------------------------------------------
; Screen display codes
C_RETURN    = 13        ; Carriage return
C_SPACE     = 32        ; Carriage return
C_HLINE     = 64        ; Middle horizontal line
C_VLINE     = 66        ; Middle vertical line

C_SW_CNR    = 109       ; Middle corner: bottom-left
C_NE_CNR    = 110       ; Middle corner: top-right
C_NW_CNR    = 112       ; Middle corner: top-left
C_SE_CNR    = 125       ; Middle corner: bottom-right

C_CROSS     = 123       ; Straight horz/vert cross

C_R_TEE     = 107       ; T-joint: pointing right
C_UP_TEE    = 113       ; T-joint: pointing up
C_DN_TEE    = 114       ; T-joint: pointing down
C_L_TEE     = 115       ; T-joint: pointing left

.endif
