# Coin-128 FORTH
A FORTH compiler for the Commodore 128, inspired by Atari Coin-Op FORTH.

### Differences from the Atari Coin-Op implementation
Note that I do not have access to documented source code for Atari Coin-Op FORTH
(and have not yet put myself through the painful process of deciphering the raw machine code).
Therefore, my reasoning and my design decisions in this context are based on
(hopefully increasingly educated) guesses as to how Coin Op was implemented.

#### Headers
According to Atari's manual:
> In Coin-Op FORTH, the link field comes *before* the name field,
> with the code field coming immediately after the name field.
> The structure is thus:
>
> `LFA NFA CFA PFA`
>
> This improves the speed of dictionary searches, since the LFA of a word will
> have a fixed offset of -2 from the NFA rather than a variable offset.

For the current (preliminary) version of Coin-128 FORTH, I have tried implementing
dictionary entries both ways. In the `diff` listing below, the red (`-`) lines are
the for the link-first strategy; the green (`+`) lines are for the name-first strategy;
while the white lines are common to both strategies.
```diff
 find:                       ; TODO: Generalize for tick & interpret
-    ldy #WORDOFF
+    ldy #$00
     lda (DP),y              ; Load count byte
     and #WLENMSK            ; Remove precedence bit
     beq find_no_match       ; Zero-length word never matches; end of dictionary
     sta COUNT               ; Save length for offset
-    tay                     ; Y indexes from last char to first
-    iny
-    iny
+    tay
 find_test_char:
     lda (DP),y              ; Load next char (working backwards)
-    cmp WORD-WORDOFF-1,y    ; Back up ONE for count byte and TWO for link
+    cmp WORD-1,y            ; Back up one to account for count byte
     bne find_no_match
     dey
-    cpy #WORDOFF            ; Backed up to the count byte again?
     beq find_match
     jmp find_test_char
 find_no_match:
-    ldy #$01                ; Link is first in word entry
-    lda (DP),y              ; Only check link hi-byte (no ZP dictionary)
+    ldy COUNT               ; Get offset to link to previous word
+    iny                     ; Offset = count byte + word length
+    iny                     ; Start with hi-byte
+    lda (DP),y              ; Only check hi-byte (no ZP dictionary)
     beq find_no_more        ; TODO: flag NOT-FOUND error
     pha                     ; Note hi-byte of next word
```
It appears to me that the fig-FORTH structure (name first) costs us two 6502 cycles
(absolute, rather than immediate addressing for the `LDY` operation) in the `find_no_match` block
for every non-matched word as the interpreter makes its way backwards through the dictionary.
In contrast, the implementation using the Coin-Op structure costs us two  6502 cycles
(the additional immediate-mode `CPY` operation) in the `find_test_char` block
for every letter checked in every word as the interpreter makes its way through
the dictionary.  (Note the double `INY` operations in `find` and `find_no_match`
should cancel each other out for the most part.)

Clearly, fig-FORTH's name-first strategy is more performant as I have coded it here,
and so, I am using this strategy for Coin-128 FORTH.  I would be open to someone with
expertise in 6502 assembly language and/or forth interpreters suggesting improvements
to either algorithm.  Note that I have also considered (but have not coded) a strategy
that is link-first, but has the dictionary pointer pointing to the name.  Essentially,
`DP` points into the entry at its second field, simplifying comparison of the current
word and perhaps directly fulfilling the spirit of the "fixed offset of -2" design description.
However, I know of no way to do negative indexed addressing on the 6502, and I cannot
see how to code a subtraction or double decrement operation such that it will take fewer
cycles than either of the two methods I have coded.

Again, please speak up if you see something I do not.  I would like this design to follow
the spirit of Coin-Op FORTH as much as possible.  However, I do not think this goal means
we should waste CPU cycles in a tight loop, especially on a 1 or 2 MHz 8-bit machine.

### Resources
The following is an active list of resource documents I am consulting as I proceed:
- [ASE: Writing a forth interpreter from scratch](https://sifflez.org/lectures/ASE/C3.pdf)
- [Atari Coin Op FORTH And Swarthmore Extensions](https://archive.org/details/AtariCoinOpFORTHAndSwarthmoreExtensions/mode/2up)
- [Earl's Forth128 Blog](https://forth128.blogspot.com/)
- [fig-FORTH 6502 Assembly Source Listing](https://web.archive.org/web/20221113020323if_/http://archive.6502.org/books/forth_interest_group/fig_forth_6502_source.pdf)
- [Jones's FORTH](https://github.com/ratfactor/nasmjf)
- [Systems Guide to Fig Forth](http://forthfiles.net/ting/sysguidefig.pdf)
