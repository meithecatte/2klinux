: R0      $1500 ;
: S0      $7C00 ;
: CLUSTER $7C10 ;
: OFFSET  $7C14 ;
: LATEST  $7C18 ;
: HERE    $7C1C ;
: STATE   $7C20 ;

: NL 10 ;
: BL 32 ;
: CR NL EMIT ;

: FALSE 0 ;
: TRUE -1 ;

: OR!  DUP @ ROT OR  SWAP ! ;
: XOR! DUP @ ROT XOR SWAP ! ;
: AND! DUP @ ROT AND SWAP ! ;

: COR!  DUP C@ ROT OR  SWAP C! ;
: CXOR! DUP C@ ROT XOR SWAP C! ;
: CAND! DUP C@ ROT AND SWAP C! ;

: IMMEDIATE LATEST @ 2 + $80 SWAP COR! ; IMMEDIATE
: [ IMMEDIATE FALSE STATE ! ;
: ] TRUE STATE ! ;

: \ IMMEDIATE [ HERE @ ] KEY NL = 0BRANCH [ , ] ;

\ Forth is a very extensible language. In fact, you can define your own comment syntax in Forth
\ itself. To do this, I needed a few words that are not defined by stage0.asm - at the very
\ beginning of this file, a few constants are defined that represent important addresses:
\ R0 - the initial value of the return stack pointer
\ S0 - the initial value of the data stack pointer
\ CLUSTER - this variable holds the cluster number of the currently loaded cluster
\ OFFSET - holds the offset from the beginning of the cluster buffer of the next character KEY will
\          return. These two variables can be used to save and restore the current file position,
\          and therefore read multiple files at once, which is a necessary capability for the C
\          preprocessor.
\ LATEST - holds the address of the last word defined
\ HERE - holds the address of the first free byte of memory
\ STATE - FALSE if interpreting words, TRUE when compiling

\ Below, two character constants are defined - BL (for BLank) returns the character value of the
\ space character, and NL (for New Line) - the character value of the newline character. These can
\ be used with EMIT to print characters on screen, as demonstated by CR.

\ In Forth, FALSE is represented by a cell with all bits unset, and TRUE is represented by a cell
\ with all bits set, which corresponds with the two's complement representation of -1.

\ OR!, XOR! and AND! all combine the corresponding bitwise operation with ! in a similar manner to
\ +! and -!: VARIABLE @ $12 XOR VARIABLE ! is equivalent to $12 VARIABLE XOR!

\ COR!, CXOR! and CAND! work in the same manner, but on single bytes instead of 32-bit cells.

\ IMMEDIATE marks the last word defined as immediate. If a word in the dictionary is flagged as
\ immediate then the interpreter runs it immediately *even if it's in compile mode*. One such
\ example is the ; word used to end definitions. We want it to run now, not when the new word is
\ used. LATEST @ gives the address of the last word defined, 2 + transforms it into the address of
\ the flags field, which is then ORed with $80 to set the highest bit, which contains the F_IMMED
\ flag.

\ IMMEDIATE is then marked immediate with itself to make it possible to say : NEW-WORD IMMEDIATE
\ which is more idiomatic to Forth. [ and ] are words that can be used to temporarily enter the
\ interpretation mode while defining a word, which is mostly useful to calculate something once
\ and make it a number literal.

\ This functionality is used while defining \, since the loop constructs are not yet available. If
\ they were, this word would be defined as : \ IMMEDIATE BEGIN KEY NL = UNTIL ; which is arguably
\ easier to understand - skip characters until you encounter a newline. \ is marked immediate to
\ make comments work correctly in compile mode. The way BEGIN and UNTIL are replaced in that
\ definition will become clear when we define control flow structures. However, some simpler words
\ will come first.

: CELLS 2 LSHIFT ; \ CELLS turns a number of cells into a number of bytes
: NIP \ ( a b -- b )
  SWAP DROP ;
: TUCK \ ( b a -- a b a )
  SWAP OVER ;
: PICK \ ( xu ... x1 x0 u -- xu ... x1 x0 xu )
  CELLS SP@ + \ ( xu ... x1 x0 addr-x_u-1 )
  4+ @ ;

: 2DROP \ ( a b -- )
  DROP DROP ;
: 2DUP \ ( a b -- a b a b )
  OVER \ ( a b a )
  OVER \ ( a b a b )
  ;
: 2SWAP \ ( a b c d -- c d a b )
  >R    \ ( a b c R: d )
  -ROT  \ ( c a b R: d )
  R>    \ ( c a b d )
  -ROT  \ ( c d a b )
  ;

\ Because of space restriction of stage0.asm, only some comparsions are primitive. The rest can be
\ done by inverting the result of a different comparison.
: <> = INVERT ;
: >= < INVERT ;
: <= > INVERT ;

: 0<> 0= INVERT ;
: 0>= 0< INVERT ;
: 0<= 0> INVERT ;

\ In two's complement, you invert all the bits and add one to compute the additive inverse.
: NEGATE INVERT 1+ ;

\ The primitive word /MOD leaves both the remainder and the quotient on the stack, in that order
\ (on x86, the idiv instruction calculates both anyway). Now we can define / and MOD in terms of
\ /MOD and stack manipulation words.
: / /MOD NIP ;
: MOD /MOD DROP ;

\ >CFA takes an address of a word in the dictionary and returns its execution token, i. e. the
\ address of its first assembly instruction (`call DOCOL' in case of Forth words)
: >CFA 2 + \ ( flags-address )
  DUP C@   \ ( flags-address flags )
  $1F AND  \ ( flags-address name-length )
  + 1+     \ skip name-length bytes, plus one bytes for the flags byte itself
  ;

\ ' SOME-WORD will push the execution token of SOME-WORD
: ' WORD FIND >CFA ;

\ LITERAL is a compile-time word that is used to define computed number literals. Consider
\ : SOME-WORD [ 2 2 + ] LITERAL ;
\ This is equivalent to : SOME-WORD 4 ; but sometimes you want to show where a value comes from
\ without recalculating it every time the word is executed. It is also useful when defining compile
\ time words, when combined with POSTPONE
\ Consider this simpler version first: : LITERAL IMMEDIATE ['] LIT , , ;
: LITERAL IMMEDIATE
  [ ' LIT     \ ' does not behave the way one could expect it to in compile mode. ['] is what you
              \ should use in such a case, but we need LITERAL to implement [']
    DUP , , ] \ LIT LIT will push the address of LIT on the stack
  , , ;

: POSTPONE IMMEDIATE ' , ;

: CHAR WORD DROP C@ ;
: [CHAR] IMMEDIATE CHAR POSTPONE LITERAL ;
: ['] IMMEDIATE ' POSTPONE LITERAL ;

\ With all of that up our sleeves we can pursue defining control flow words, starting with IF and
\ THEN.

\ before IF inside THEN after

\ compiles to

\ before 0BRANCH ptr inside after
\                 \---------^

: IF IMMEDIATE ['] 0BRANCH ,
  HERE @ \ save the location of the branch destination word on the data stack DURING COMPILATION
  0 , \ compile a dummy destination
  ;

: THEN IMMEDIATE \ ( ptr-addr )
  HERE @ \ ( ptr-addr ptr-val )
  SWAP ! ;

\ before IF true ELSE false THEN after

\ compiles to

\ before 0BRANCH ptr1 true BRANCH ptr2 false after
\                 \----------------+---^     ^
\                                  \---------+

: ELSE IMMEDIATE \ ( ptr1-addr )
  ['] BRANCH ,
  HERE @ \ ( ptr1-addr ptr2-addr )
  0 ,    \ compile a dummy ptr2
  HERE @ \ ( ptr1-addr ptr2-addr ptr1-val )
  ROT ! ;

: TEST-IF-ELSE IF [CHAR] T ELSE [CHAR] F THEN ;
: TEST-IF IF [CHAR] Y EMIT THEN ;

CHAR Q EMIT
TRUE TEST-IF-ELSE EMIT
CHAR Z EMIT
FALSE TEST-IF-ELSE EMIT
CHAR P EMIT

: HIDDEN 2 + DUP @ $20 XOR SWAP ! ;
