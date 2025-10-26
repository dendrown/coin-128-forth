; COIN-128: Coin-128 Forth for the Commodore-128
;
; vim: set ft=asm_ca65:
;-----------------------------------------------------------------------------
.include "c128.inc"         ; cc65 definitions [origin: Elite128]
.include "c128-defs.inc"    ; More definitions for the C128
.include "coin-defs.inc"    ; Coin-128 Forth definitions & macros

.setcpu "6502"

; Constants
.define APP_TITLE   "coin-128 forth"
.define APP_VERSION "0.1"


.macro push_word addr
    dex
    lda #>(addr)
    sta PSTACK,X
    dex
    lda #<(addr)
    sta PSTACK,X
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
    jsr CROUT
    cprintln silliness2
    jsr CROUT
    rts

;-----------------------------------------------------------------------------
; Forth vocabulary:
; Word tags "L9999" refer to fig6502.
; @ref https://github.com/jefftranter/6502/blob/master/asm/fig-forth/fig6502.asm
orig:
W0000:
    .byte $00               ; VOCABULARY: start token (end of reverse search)
    .word $0000

FORTH_WORD "ok"             ; ------------------------------------------------
    jmp next                ; OK ( -- )

FORTH_WORD ".s"             ; ------------------------------------------------
dot_s:                      ; .S (_ -- _)
    stx W                   ; We need XSAVE for stack/print exchange, so use W
    ldx #$00                ; Start as if the PSTACK were empty
dot_s_loop:
    cpx W                   ; Real value of PSP (X) ?
    beq dot_s_done
    dex
    dex
    jsr dot_sub
    dex                     ; Unpop the printed value on the PSTACK
    dex
    jmp dot_s_loop
dot_s_done:
    ldx W                   ; Restore real PSP (X) value
    jmp next

FORTH_WORD "execute"        ; --------------------------------------------L75-
execute:                    ; EXECUTE (a -- )
    lda PSTACK,X            ; Lo byte of CFA
    sta W
    lda PSTACK+1,X          ; Hi byte of CFA
    sta W+1
    inx                     ; Pop PSTACK
    inx
    jmp (W)
.macro exec_word addr
    push_word addr
    jmp execute
.endmacro


FORTH_WORD "(find)"         ; -------------------------------------------L243-
p_find_p:                   ; (FIND) (a -- a)       \ for a dictionary word
                            ;        (a -- a a)     \ for a number
    store_w W9999,DP        ; Initialize start of dictionary
                            ; TODO: Check: if a != W => CMOVE to WORD
p_find_p_word:
    ldy #$00
    lda (DP),Y              ; Load count byte
    and #WLENMSK            ; Remove precedence bit
    beq p_find_p_number     ; Zero-length word never matches; end of dictionary
    sta COUNT               ; Save length for offset
    cmp WORD                ; Check word length to avoid partials matching
    bne p_find_p_nope       ; Dictionary & intepreted word lengths do NOT match
    tay                     ; Y <- char count for word candidate
p_find_p_char:
    lda (DP),Y              ; Load next char (working backwards)
    cmp WORD,Y              ; Test char against WORD buffer
    bne p_find_p_nope
    dey
    beq p_find_p_match      ; No more chars to test (0th is the count)
    jmp p_find_p_char
p_find_p_nope:
    ldy COUNT               ; Get offset to link to previous word
    iny                     ; Offset = count byte + word length
    iny                     ; Start with hi-byte
    lda (DP),y              ; Only check hi-byte (no ZP dictionary)
    beq p_find_p_number
    pha                     ; Note hi-byte of next word
    dey
    lda (DP),y              ; Grab lo-byte of next word
    sta DP                  ; Store it for previous word
    pla                     ; Pull hi-byte again
    sta DP+1                ; Store it for previous word
    jmp p_find_p_word
p_find_p_match:
    clc
    lda COUNT
    adc #WORDOFF+1          ; Word char count + length + link will never carry
    adc DP                  ; Calc lo byte offset to dictionary word CFA
    pha
    lda #$00                ; Calc hi byte of dictionary word CFA
    adc DP+1
    jmp put                 ; PSTACK, in place: word-string => word-exec
p_find_p_number:
    lda #<(number)
    pha
    lda #>(number)
    jmp push


FORTH_WORD ">r"             ; ------------------------------------------------
to_r:                       ; >R (n -- )
    lda PSTACK+1,x          ; Transfer HI byte (BACKWARDS on RSTACK)
    pha
    lda PSTACK,x            ; Transfer LO byte (BACKWARDS on RESTACK)
    pha
    jmp pop

FORTH_WORD "r>"             ; ------------------------------------------------
r_from:                     ; >R ( -- n)
    pla                     ; Prep LO byte
    jmp push                ; Push takes HI byte from RSTACK

FORTH_WORD "+"              ; ------------------------------------------------
plus:
    clc
    lda PSTACK,x            ; Lo byte of TOP
    adc PSTACK+2,x          ; Lo byte of TOP-1
    sta PSTACK+2,x
    lda PSTACK+1,x          ; Hi byte of TOP
    adc PSTACK+3,x          ; Hi byte of TOP-1
    sta PSTACK+3,x
    jmp pop                 ; Result in TOP-1...drop TOP

FORTH_WORD "-"              ; ------------------------------------------------
minus:
    sec
    lda PSTACK+2,x          ; Lo byte of TOP-1
    sbc PSTACK,x            ; Lo byte of TOP
    sta PSTACK+2,x
    lda PSTACK+3,x          ; Hi byte of TOP-1
    sbc PSTACK+1,x          ; Hi byte of TOP
    sta PSTACK+3,x
    jmp pop                 ; Result in TOP-1...drop TOP

FORTH_WORD "over"           ; ------------------------------------------------
over:                       ; OVER (n1 n2 -- n1 n2 n1)
    lda PSTACK+2,x
    pha
    lda PSTACK+3,x
    jmp push

FORTH_WORD "drop"           ; ------------------------------------------------
drop:                       ; DROP (n -- )
pop:                        ; TODO: move to poptwo/pop in (do) W0185
    inx
    inx
    jmp next

FORTH_WORD "swap"           ; ------------------------------------------------
swap:                       ; PSTACK [1300..|TOP=0|1|2|3|..13fe:13ff]
                            ; NOTE: a ZP stack would allow us to save ops using
                            ;       ldy/sty instead of pha/pla for byte 3 -> 1.
    lda PSTACK+2,x          ; PUT prep: 2 -> RSTACK -> 0
    pha
    lda PSTACK+3,x          ; PUT prep: 3 -> RSTACK
    pha
    lda PSTACK,x            ; Move 0 -> 2
    sta PSTACK+2,x
    lda PSTACK+1,x          ; Move 1 -> 3
    sta PSTACK+3,x
    pla                     ; PUT prep:      RSTACK -> 1
    jmp put

FORTH_WORD "dup"            ; ------------------------------------------------
dup:                        ; DUP (n -- n n)
    lda PSTACK,x            ; PSTACK [1300..|TOP=0|1|......13fe:13ff]
    pha
    lda PSTACK+1,x
    jmp push

FORTH_WORD "@"              ; ------------------------------------------------
fetch:                      ; @ (a -- n)
    jsr pop_addr_w          ; Use W as target address pointer
    ldy #$00                ; Lo byte goes on the RSTACK
    lda (W),Y
    pha
    iny                     ; Hi byte in accumulator
    lda (W),Y
    jmp push
pop_addr_w:
    lda PSTACK,X            ; Lo byte of target address
    sta W
    inx
    lda PSTACK,X            ; Hi byte of target address
    sta W+1
    inx
    rts

FORTH_WORD "c@"             ; ------------------------------------------------
c_fetch:                    ; C@ (a -- b)
    jsr pop_addr_w          ; Use W as target address pointer
    ldy #$00                ; Load lo byte
    lda (W),Y
    pha
    tya                     ; Hi byte is always 00
    jmp push

FORTH_WORD "!"              ; ------------------------------------------------
store:                      ; ! (n a -- )
    jsr pop_addr_w          ; Use W as target address pointer
    lda PSTACK+1,X          ; Hi byte of value at TOS
    ldy #$01
    sta (W),Y
store_lo_iw:
    lda PSTACK,X            ; Lo byte of value at TOS
    ldy #$00
    sta (W),Y
    jmp drop                ; Pop lo byte of value off PSTACK

FORTH_WORD "c!"             ; ------------------------------------------------
c_store:                    ; C! (b a -- )
    jsr pop_addr_w          ; Use W as target address pointer
    jmp store_lo_iw

FORTH_WORD "constant"       ; ------------------------------------------------
;   .word docol
;   .word creat
;   .word smudg
;   .word lbrac
;   .word semis
const:
    ldy #$03                ; IP is at code past link, offset by `jmp const`
    lda (W),y
    pha
    iny
    lda (W),y
    jmp push

FORTH_WORD "user"           ; ------------------------------------------------
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

FORTH_WORD "0"              ; ------------------------------------------------
zero:                       ; 0 ( -- 0000)
    jmp const
    .word 0

FORTH_WORD "1"              ; ------------------------------------------------
one:                        ; 1 ( -- 0001)
    jmp const
    .word 1

FORTH_WORD "2"              ; ------------------------------------------------
two:                        ; 2 ( -- 0002)
    jmp const
    .word 2

FORTH_WORD "3"              ; ------------------------------------------------
three:                      ; 3 ( -- 0003)
    jmp const
    .word 3

FORTH_WORD "bl"             ; ------------------------------------------------
bl:                         ; BL ( -- ' ')
    jmp const
    .word ' '

FORTH_WORD "tib"            ; ------------------------------------------------
tib:                        ; tib ( -- a)
    jmp const
    .word TIBX

FORTH_WORD "1+"             ; ------------------------------------------------
one_plus:                   ; 1+ (n -- n+1)
    jmp enter
    .word one
    .word plus
    .word exit

FORTH_WORD "1-"             ; ------------------------------------------------
one_minus:                  ; 1- (n -- n-1)
    jmp enter
    .word one
    .word minus
    .word exit

FORTH_WORD "rot"            ; ------------------------------------------------
rot:                        ; ROT (n1 n2 n3 -- n2 n3 n1)
    jmp enter
    .word to_r
    .word swap
    .word r_from
    .word swap
    .word exit

FORTH_WORD "word"           ; ------------------------------------------------
word:                       ; WORD (c -- a)
    lda PSTACK,X            ; Store word separator
    sta WEND                ; Save space|other
    ldy #$00
word_start:
    jsr JBASIN              ; Skip any preliminary delimiters
    cmp WEND
    beq word_start
word_char:
    cmp #C_RETURN           ; Got a char, is it the end of the line?
    beq word_line_done
    sta WORD+1,y            ; Store the char, skipping the count byte
    cmp WEND                ; Was the char our delimiter?
    beq word_done
    iny
    cpy #MAXWORD+1          ; Past single-word buffer? (allowing separator)
    bcs word_error
    jsr JBASIN              ; Get next char from current input file
    jmp word_char
word_line_done:
    sta WEND                ; Replace space|other with RETURN
word_done:
    sty WORD                ; Count byte at beginning of word buffer
    lda #<(WORD)            ; Replace separator with WORD pointer on PSTACK
    sta PSTACK,X
    lda #>(WORD)            ; Hi byte replaced in current PSTACK frame
    sta PSTACK+1,X
    jmp next                ; End of good path
word_error:
    inc PNTR
    inc PNTR
    cprintln error
    jmp quit

FORTH_WORD "number"         ; ------------------------------------------L2007-
number:                     ; NUMBER (a -- n)
    clc                     ; Increment text pointer as we store it in INPPTR
    lda PSTACK,X
    adc #01
    sta INPPTR              ; Lo byte of text pointer
    lda PSTACK+1,X
    adc #00
    sta INPPTR+1            ; Hi byte of text pointer
number_check:
    ldy INPPTR              ; Number of chars in number string
number_check_loop:
    lda INPPTR-1,Y          ; Compensate for length byte at copy destination
    cmp #'0'                ; Bad ASCII digit < 0 ?
    bcc word_error
    cmp #'f'+1              ; Bad ASCII digit > F ?
    bcs word_error
    cmp #'9'+1              ; Good ASCII digit <= 9 ?
    bcc number_strip
    cmp #'a'                ; Bad  ASCII digit < A ?
    bcc word_error
    adc #$08                ; Good ASCII digit A..F: add 8 + carry
number_strip:
    and #$0F                ; Strip off high nybble of ASCII
    sta number_buff,Y
    dey
    bne number_check_loop
number_hex2bin:
    ldy number_buff         ; Count of bytes to convert
number_hex2bin_loop:
    lda number_buff-1,Y     ; Load stripped-ASCII hi nybble
    asl                     ; Shift into high nybble of A
    asl
    asl
    asl
    clc                     ; Add in stripped-ASCII lo nybble
    adc number_buff,Y
    sta PSTACK,X
    dey
    dey
    beq number_done
    inx                     ; Half-un-push PSP for hi nybble
    jmp number_hex2bin_loop
number_done:
    dex                     ; Half-push PSP so number is on top of PSTACK
    jmp next
number_buff:
    .byte $04, $00, $00, $00, $00

FORTH_WORD "interpret"      ; ------------------------------------------L2269-
interpret:                  ; INTERPRET ( -- )
    jmp enter
    .word bl
    .word word
    .word p_find_p
    .word execute
    .word word_out
    .word exit

FORTH_WORD "quit"           ; ------------------------------------------L2381-
;quit_debug:                 ; QUIT ( -- )
;    .word $0000
quit:                       ; QUIT ( -- )
   ;stx XSAVE
   ;ldx #$FF                ; S: Reset RSTACK
   ;txs
   ;ldx XSAVE
   ;lda #$00                ; And clear STATE (interpreting)
   ;sta STATE
                            ; TODO: 0 BLK !
    jmp enter
    .word interpret
    .word exit

FORTH_WORD "."              ; ------------------------------------------L3562-
dot:                        ; TODO: Check for empty stack!
    jsr dot_sub
    jmp next
dot_sub:                    ; Subroutine called from . and .S
    inc PNTR                ; Give space so we don't overwrite the dot
    inc PNTR
    lda PSTACK,x            ; Load lo-byte from top pstack cell
    tay
    inx
    lda PSTACK,x            ; Load hi-byte from top pstack cell
    inx
    stx XSAVE               ; Save pstack pointer now that value is popped
    tax                     ; X = hi-byte of cell
    tya                     ; A = lo-byte of cell
    jsr PRNTHEX4            ; TODO: print value according to BASE
    ldx XSAVE               ; Restore pstack pointer
    rts

W9996:
FORTH_WORD "noop"           ; ------------------------------------------------
noop:                       ; Debug word that does nothing
    jmp enter
    .word exit

W9997:
    FORTH_WORD "break"      ; ------------------------------------------------
break:                      ; Debug word to go to C128 monitor
    brk

W9998:
FORTH_WORD "~out"           ; ------------------------------------------------
word_out:
    lda WEND                ; Get the last space|CR
    cmp #C_RETURN           ; Done with word on this line?
    beq word_out_line       ; Process line
    jmp next                ; No, keep processing more words
word_out_line:
    inc PNTR
    cprint ok
    jsr CROUT
    jmp next

W9999:
FORTH_WORD "debug"          ; ------------------------------------------------
    rts                     ; Return to BASIC

;-----------------------------------------------------------------------------
; TODO: we are hanging out behind the BASIC stub for now. The kernel will
; move once we have a kernel to move and a memory layout to move it to.
coin:
    store_w APPAREA,UP      ; UP is PSP base for a single-task Forth
    ldx #$00                ; X: Empty PSTACK
cold:
    ; EMPTY-BUFFERS...      ; TODO: Set up for disk usage
    ; ORIG...               ; TODO: Set up memory
    ; FORTH...              ; TODO: Set FORTH vocabulary linkage
    jmp abort_loop          ; TODO: COLD & ABORT should be proper words
abort:
    ; FORTH...              ; TODO: Select FORTH trunk vocabulary
    ; DEFINITIONS...        ; TODO: Set CURRENT to CONTEXT
    .word abort             ; TODO: ABORT should be a proper word
abort_loop:
    lda #<(abort)           ; Re-queue ABORT as hacked previous "word"
    sta IP
    lda #>(abort)
    sta IP+1
    exec_word quit
    jmp abort_loop


;-----------------------------------------------------------------------------
enter_ml:
    store_w word_out,IP     ; TODO: ENTER_ML is TEMPORARY scaffolding
    rts
;
; ENTER is common across all colon definitions
;
enter:                      ; Common entry for all colon defs
    lda IP
    pha                     ; Stack BACKWARDS [__:LO] to combine ADD/PUSH
    lda IP+1
    pha                     ; Stack BACKWARDS [HI:lo]
    clc                     ; Set IP <- first WORD address
    lda W                   ; W points to JMP ENTER in the current word
    adc #$03                ; skip past the JMP instruction
    sta IP
    lda W+1
    adc #$00
    sta IP+1
    jmp next
;
; EXIT represents the semicolon for all word definitions
;
exit:                       ; Terminate forth word thread
    pla
    sta IP+1                ; Remove from RSTACK BACKWARDS [HI:lo] (see: ENTER)
    pla
    sta IP                  ; Remove from RSTACK BACKWARDS [__:LO]
    ldy #$00                ; W <- (IP)
    lda (IP),Y
    sta W
    iny
    lda (IP),Y
    sta W+1
    jmp (W)                 ; Execute DTC
;
; PUSH/PUT: parameter stack operations
;
push:                       ; PSTACK uses page 1300, grows from the top, down
    dex
    dex
put:
    sta PSTACK+1,X          ; Hi byte from A
    pla                     ; Lo byte from R-stack
    sta PSTACK,X
;
; NEXT is the address interpreter that moves from machine-level word to word
;
next:
    ldy #$00                ; Set W <- (IP)
    lda (IP),y
    sta W
    iny
    lda (IP),y
    sta W+1
    clc
    lda IP                  ; Set IP <- next word
    adc #$02                ; Advance IP
    sta IP
    lda IP+1
    adc #$00
    sta IP+1
    jmp (W)                 ; Execute DTC


;-----------------------------------------------------------------------------
.rodata

welcome:    cstring .sprintf("%s v%s", APP_TITLE, APP_VERSION)
ok:         cstring " ok"
error:      cstring "error!"
silliness1: cstring "forth? maybe zeroth..."
silliness2: cstring "we don't do anything yet!"
