; COIN-128: Coin-128 Forth symbol/macro definitions
;
; asmsyntax=asm68k  (6510/8502 ASM, but better syntax highlighting in vim)
;-----------------------------------------------------------------------------
; Zero-page registers       ; LO:HI 16-bit pointers
DP      := $40              ; 41:40 Dictionary pointer  (BASIC RESHO)
FREE42  := $42              ; 43:42                     (BASIC RESHO)
FREE44  := $44              ; 44                        (BASIC RESHO)
NO_WORD := $63              ; 63-69 Curr word: 7 chars  (FAC1:FACSGN:SGNFLAG)
WORD    := $6A              ; 6A-70 Curr word: 7 chars  (FAC2:ARGSGN:ARIFLAG)
UP      := $74              ; 75:74 Forth User Pointer  (BASIC AUTINC)
W       := $FA              ; FB:FA Forth Working (W) register
IP      := $FC              ; FD:FC Forth Interpreter Pointer (IP)
XSAVE   := $FE              ; Temporary for X register

TIBX    := $0B00            ; Initial Terminal Input Buffer
PSTACK  := APPAREA          ; Parameter stack page


MAXWORD = 6                 ; Maximum word length (note ZP buffer) : TODO: 32
WORDOFF = 2                 ; Offset to WORD in dictionary entry
WLENMSK = $7F               ; Word length bit mask
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

