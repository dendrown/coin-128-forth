; COIN-128: Coin-128 Forth symbol/macro definitions
;
; vim: set ft=asm_ca65:
;-----------------------------------------------------------------------------
; Zero-page registers       ; LO:HI 16-bit pointers
STATE   := $3F              ; Forth main loop state     (<FNDPNT)
RRESET  := $40              ; RSTACK reset cap          (>FNDPNT)
DP      := $41;42           ; Dictionary pointer        (BASIC DATLIN)
VOCLINK := $43;44           ; Top vocabulary link       (BASIC DATPTR)
VOCSEEK := $45;46           ; Current word for (FIND)   (BASIC INPPTR)
Z       := $47;48           ; General purpose register  (BASIC VARNAM)

WORD    := $63;..70         ; Curr word: Max 14 chars   (FAC1:FACSGN:SGNFLAG)
                            ;       [CNT|12-CHARS|SPC]  (FAC2:ARGSGN:ARIFLAG)
FREE_74 := $74;75           ;                           (BASIC AUTINC)
WEND    := $80              ; Word END character        (BASIC POINT)
WLEN    := $82              ; Interpreted word length   (BASIC OLDSTK)
W       := $FA;FB           ; Forth Working [W] register
IP      := $FC;FD           ; Forth Interpreter Pointer [IP]
XSAVE   := $FE              ; Temporary for X register

EMITBUF := BUF              ; EMIT/print buffer         (BASIC/moN input BUF)
TIBX    := $0B00            ; Initial Terminal Input Buffer
PSTACK  := APPAREA          ; Parameter stack page


MAXWORD = 12                ; Maximum WORD length (before separator) : TODO: 32
WORDOFF = 2                 ; Offset to WORD in dictionary entry
WLENMSK = $7F               ; Word length bit mask
WPRCBIT = $10               ; Word precedence bit to indicate IMMEDIATE word


;-----------------------------------------------------------------------------
; Assembler variables
curr_forth_word .set 0      ; Holds the link to the current FORTH_WORD


;-----------------------------------------------------------------------------
; Macros
.macro FORTH_WORD name      ; Creates name:link for a dictionary entry
    prev_forth_word .set curr_forth_word
    curr_forth_word .set curr_forth_word + 1
.ident (.sprintf("w%04d", curr_forth_word)):
    cstring name                                        ; NFA
    .word .ident (.sprintf("w%04d", prev_forth_word))   ; LFA
.endmacro


.macro init_vocseek         ; VOCLINK => VOCSEEK for dictionary search/update
    lda VOCLINK
    sta VOCSEEK
    lda VOCLINK+1
    sta VOCSEEK+1
.endmacro


.macro cpy_wptr src,dst     ; Copy word address pointer
    lda src
    sta dst
    lda src+1
    sta dst+1
.endmacro


.macro adc_w v,addr         ; Add v to 16-bit word
    clc                     ; addr <- next word
    lda addr                ; Increment lo byte
    adc v
    sta addr
    lda addr+1              ; Carry into hi byte
    adc #$00
    sta addr+1
.endmacro


.macro inc_wptr addr        ; Increment word address pointer
    adc_w #$02,addr
.endmacro


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
    lda s,Y                 ; A = next character
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
    lda s,Y                 ; A = next character
    beq done
    jsr JBSOUT              ; BASIC output(A)
    iny
    jmp load_char
zprint_done:
    nop
.endmacro


.macro zprintln s           ; Print a NUL-terminated string + CR
    zprint s
    jsr CROUT
.endmacro


.macro ziprint s            ; Print a NUL-terminated string via zero-page ptr
.local load_char
.local done
    ldy #$00
load_char:
    lda s,Y                 ; A = next character
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


.macro store_iy_w w,addr    ; Store a 16-bit value to (addr),Y & advance Y offset
    lda #<(w)
    sta (addr),Y
    iny
    lda #>(w)
    sta (addr),Y
    iny
.endmacro


.macro store_iy_ptr ptr,addr; Store pointer value to (addr),Y & advance Y offset
    lda ptr
    sta (addr),Y
    iny
    lda ptr+1
    sta (addr),Y
    iny
.endmacro

