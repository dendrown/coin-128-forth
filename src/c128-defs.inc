; Commodore-128 definitions
;
; vim: set ft=asm_ca65:
;-----------------------------------------------------------------------------
.ifndef __C128_DEFS_INC
__C128_MMAP_INC = 1

; C128 memory map (beyond cc65 c128 include)
PC          := $03      ; 03:04 Target address for JMPFAR and JSRFAR
COUNT       := $0D      ; Working storage for various routines
LINNUM      := $16      ; 17:16 Integer value of an ASCII string
INPPTR      := $45      ; 46:45 Text pointer for BASIC input
TEMPF3      := $50      ; Temporary storage for floating-point value (50..54)
AUTINC      := $74      ; 75:74 Step value for program line auto-increment
PNTR        := $EC      ; Current cursor column

BUF         := $0200    ; Input buffer: BASIC & monitor (0200..02A1)
VICSCN      := $0400    ; VIC 40-col text screen

APPAREA     := $1300    ; Application program area (1300..1BFF)
RAMBOT      := $1C00    ; Start of BASIC text (kernal sets MEMBOT here)

INLIN       := $4F93    ; Input a line to the input buffer (BUF)

;-----------------------------------------------------------------------------
; Direct BASIC routines
CRUPOUT     := $B8AD    ; Output carriage return + cursor up
CROUT       := $B8B4    ; Output carriage return
PRNTHEX     := $B892    ; Print six  hex digits + space from 68:67:66
PRNTHEX4    := $B89F    ; Print four hex digits + space from X:A
PRNTHEX2    := $B8A5    ; Print two  hex digits + space from A

;-----------------------------------------------------------------------------
; Jump-table to BASIC routines
JLINGET     := $AF9F    ; Convert a string @ INPPTR to integer @ LINNUM

;-----------------------------------------------------------------------------
; Jump-table to kernal routines
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

;-----------------------------------------------------------------------------
; BASIC tokens
; @ref: https://www.c64-wiki.com/wiki/BASIC_token
BT_REM      = $8F
BT_SYS      = $9E

.endif
