: R0      $1500 ;
: S0      $7C00 ;
: CLUSTER $7C10 ;
: OFFSET  $7C14 ;
: LATEST  $7C18 ;
: HERE    $7C1C ;
: STATE   $7C20 ;

: F_IMMED   $80 ;
: F_HIDDEN  $20 ;
: F_LENMASK $1F ;

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

: IMMEDIATE LATEST @ 2 + F_IMMED SWAP COR! ; IMMEDIATE
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

\ Furthermore, the flags describing the flags field are also defined. Their meaning is the same as
\ of the corresponding constants in the assembly file.

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
\ the flags field, which is then ORed with F_IMMED to set the immediate flag.

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

\ also known as [COMPILE]

: [COMPILE] IMMEDIATE POSTPONE POSTPONE ;

: RECURSE IMMEDIATE LATEST @ >CFA , ;

: CHAR WORD DROP C@ ;
: [CHAR] IMMEDIATE CHAR POSTPONE LITERAL ;
: ['] IMMEDIATE ' POSTPONE LITERAL ;

\ With all of that up our sleeves we can pursue defining control flow words, starting with IF and
\ THEN.

\ before IF inside THEN after
\ compiles to

\ before 0BRANCH ptr inside after
\                 \_________^

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
\                 \________________|___^     ^
\                                  T         |
\                                  \_________/

: ELSE IMMEDIATE \ ( ptr1-addr )
  ['] BRANCH ,
  HERE @ \ ( ptr1-addr ptr2-addr )
  0 ,    \ compile a dummy ptr2
  HERE @ \ ( ptr1-addr ptr2-addr ptr1-val )
  ROT ! ;

\ before BEGIN inside AGAIN after
\ compiles to

\ before inside BRANCH ptr after
\        ^______________/

: BEGIN IMMEDIATE HERE @ ;
: AGAIN IMMEDIATE ['] BRANCH , , ;

\ before BEGIN inside UNTIL after
\ compiles to

\ before inside 0BRANCH ptr after
\        ^_______________/

: UNTIL IMMEDIATE ['] 0BRANCH , , ;

\ before BEGIN condition WHILE inside REPEAT after
\ compiles to

\ before condition 0BRANCH ptr1 inside BRANCH ptr2 after
\        ^__________________|__________________/   ^
\                           \______________________/

: WHILE IMMEDIATE ['] 0BRANCH , HERE @ 0 , ;
: REPEAT IMMEDIATE \ ( ptr2-val ptr1-addr )
  ['] BRANCH , SWAP , \ ( ptr1-addr )
  HERE @ SWAP ! ;

\ CASE                         ( push 0 during compilation to count the necessary amount of IFs )
\ test1 OF ... ENDOF           test1 OVER = IF DROP ... ELSE
\ test2 OF ... ENDOF           test2 OVER = IF DROP ... ELSE
\ test3 OF ... ENDOF           test3 OVER = IF DROP ... ELSE
\ default-case                 default-case
\ ENDCASE                      DROP THEN THEN THEN

: CASE IMMEDIATE 0 ;
: OF IMMEDIATE
  ['] OVER ,
  ['] = ,
  POSTPONE IF
  ['] DROP ,
  ;

: ENDOF IMMEDIATE POSTPONE ELSE ;
: ENDCASE IMMEDIATE
  ['] DROP ,
  BEGIN
    DUP 0<>
  WHILE
    POSTPONE THEN
  REPEAT
  DROP
  ;

\ this is enough control structures to define parenthesis comments
: ( IMMEDIATE \ ( -- )
  1                    \ allow nested comments by storing the depth
  BEGIN KEY            \ ( depth key )
    CASE
      [CHAR] ( OF 1+ ENDOF
      [CHAR] ) OF 1- ENDOF
    ENDCASE
  DUP 0= UNTIL         \ ( depth )
  DROP                 \ ( )
;

: CELLS ( CELLS turns a number of cells into a number of bytes ) 2 LSHIFT ;
: CELL+ 4+ ;
: NIP ( a b -- b ) SWAP DROP ;
: TUCK ( b a -- a b a ) SWAP OVER ;
: PICK ( x(u) ... x(1) x(0) u -- x(u) ... x(1) x(0) x(u) )
  CELLS SP@ + ( x(u) ... x(1) x(0) addrof-x(u-1) )
  4+ @ ;

: 2DROP ( a b -- ) DROP DROP ;
: 2DUP ( a b -- a b a b ) OVER ( a b a ) OVER ( a b a b ) ;
: 2SWAP ( a b c d -- c d a b )
  >R ( a b c R: d )
  -ROT ( c a b R: d )
  R> ( c a b d )
  -ROT ( c d a b ) ;

: 2RDROP ( R: x x -- R: ) RDROP RDROP ;

( The primitive word /MOD leaves both the remainder and the quotient on the stack, in that order
  (on x86, the idiv instruction calculates both anyway). Now we can define / and MOD in terms of
  /MOD and stack manipulation words. )
: / /MOD NIP ;
: MOD /MOD DROP ;

 ( HIDDEN takes an address of a dictionary entry and toggles its hidden flag )
: HIDDEN 2 + F_HIDDEN SWAP CXOR! ;
: HIDE WORD FIND HIDDEN ;

: WITHIN ( c a b -- a<=c<b )
  2 PICK ( c a b c )
  >      ( c a c<b )
  -ROT   ( c<b c a )
  >=     ( c<b a<=c )
  AND    ( a<=c<b )
  ;

: DEPTH ( -- n )
  S0 SP@ - 4- 2 RSHIFT ;

\ string literals are compiled as follows:
\   LITSTRING length string-itself rest-of-code
: LITSTRING
  R@ 4+ ( string-address )
  R@ @  ( string-address string-length )
  R> OVER + >R ( move the return address )
  ;

: COMPILE-STRING-CHARACTERS
  ( a helper function used to compile characters until a " )
  BEGIN
    KEY DUP [CHAR] " <>
  WHILE
    C,
  REPEAT
  DROP ;

: S" IMMEDIATE
  ( S" behaves correctly even in immediate mode )
  STATE @ IF
    ( we're in compile mode, compile LITSTRING )
    ['] LITSTRING ,
    HERE @ ( save the address of the length word on the stack )
    0 ,    ( compile a dummy length )
    COMPILE-STRING-CHARACTERS
    DUP    ( length-addr length-addr )
    4+     ( length-addr first-char-addr )
    HERE @ ( length-addr first-char-addr byte-after-last-char-addr )
    SWAP - ( length-addr length )
    SWAP ! ( )
  ELSE
    ( we're in immediate mode, use the currently free bytes but don't update HERE )
    HERE @ ( first-char-addr )
    COMPILE-STRING-CHARACTERS
    HERE @ ( first-char-addr byte-after-last-char-addr )
    OVER - ( first-char-addr length )
    OVER HERE ! ( restore HERE )
  THEN
;

HIDE COMPILE-STRING-CHARACTERS

: .DIGIT [CHAR] 0 + EMIT ;
