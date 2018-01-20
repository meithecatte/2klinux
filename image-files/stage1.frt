: R0      $1500 ;
: S0      $7C00 ;
: BLK     $7C10 ;
: >IN     $7C14 ;
: LATEST  $7C18 ;
: HERE    $7C1C ;
: STATE   $7C20 ;
: LENGTH  $7C24 ;

: F_IMMED   $80 ;
: F_HIDDEN  $20 ;
: F_LENMASK $1F ;

: ROOT $882C @ LOAD ;

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

: >FLAGS 2 + ;
: IMMEDIATE F_IMMED LATEST @ >FLAGS COR! ;

: [ FALSE STATE ! ; IMMEDIATE
: ] TRUE STATE ! ;

: \ [ HERE @ ] KEY NL = 0BRANCH [ , ] ; IMMEDIATE

\ ---------- AN EXPLANATION OF WHAT HAS JUST HAPPENED --------------------------------------------

\ Forth is a very extensible language. For example, you can even define your own comment syntax at
\ runtime. Because space is tight in the initial binary, no comment words are included, what makes
\ doing so a necessity, instead of being just an exercise. However, to define the comment word you
\ see above, I needed some other words, which I will explain here.

\ At the very beginning of this file, a few constants are defined. It's done here, because exactly
\ the same values are also defined in stage0.s, and any discrepncies mean chaos.
\ The first batch of constants contains important addresses:
\ R0 - the initial value of the return stack pointer
\ S0 - the initial value of the data stack pointer
\ BLK - holds the cluster number of the currently loaded cluster
\ >IN - holds the offset in the cluster buffer of the first unparsed character. Together with BLK,
\       it can be used to save and restore the current file position, for example in order to read
\       multiple files at once, which is necessary for implementing the C preprocessor.
\ LATEST - holds the address of the last word defined
\ HERE - holds the address of the first free byte of memory
\ STATE - FALSE if interpreting words, TRUE when compiling
\ LENGTH - the number of bytes left in the currently open file. When this goes down to 0, KEY will
\          return zeroes indefinitely. Since binary files are not expected, this should be handled
\          appropriately by anything using KEY directly. The definition of \ will be extended when
\          proper control flow becomes possible.

\ Below, the constants describing the flags field of a dictionary entry are defined. You should go
\ to stage0.s for more information if their meaning is unclear.

\ ROOT is a word that LOADs the first cluster of the root directory, and because it depends on the
\ the address of BPBRootCluster, it's also defined here.

\ Below, two character constants are defined.  BL (BLank) will return the ASCII value of the space
\ character, and NL (for New Line) - the newline character.  These can be used with EMIT to output
\ characters on screen, as demonstated by CR.

\ In Forth, FALSE is represented by a cell with all bits unset, and TRUE - by a cell with all bits
\ set, which corresponds with the two's complement representation of -1. This representation makes
\ bitwise and logical operations equivalent, which avoids cluttering up the dictionary.  While any
\ non-zero cell would work as TRUE with the control flow words, these two values are the canonical
\ representations, which becomes important when dealing with logical operations.

\ OR!, XOR! and AND! all combine the corresponding bitwise operation with ! in a similar manner to
\ +! or -!.  For example,  `VAR @ $12 XOR VAR !` is equivalent to `$12 VAR XOR!`.  COR!, CXOR! and
\ CAND! work in the same manner, but on single bytes instead of 32-bit cells.

\ Finally, >FLAGS is a word that takes a pointer to the link field of a dictionary entry and turns
\ it into a pointer to the flags field.

\ All of this makes it possible to define IMMEDIATE. Unsurprisingly, IMMEDIATE is used to mark the
\ word defined last as immediate. This flag means that INTERPRET runs the marked word immediately,
\ _even if it's in compile mode_. One example is the ; word used to end definitions. We want it to
\ run now, not when the new word is used, which would mean it is impossible to exit compile mode.

\ The way IMMEDIATE is implemented is surprisingly simple. The `LATEST @ >FLAGS` part results with
\ the address of the flags field of the relevant dictionary entry, which is then ORed with F_IMMED
\ to set the immediate flag.

\ [ and ] can be used to temporarily enter the interpretation mode while defining a word, which is
\ mostly useful to calculate something once and make it a number literal.

\ This functionality is used while defining \, since the loop constructs are not yet available. If
\ they were, this word would be defined as `: \ BEGIN KEY NL = UNTIL ; IMMEDIATE`, which is surely
\ easier to understand - skip characters until you encounter a newline.

\ This word is marked as immediate to make comments work correctly in compile mode.  The way BEGIN
\ and UNTIL are replaced in that definition should become clear when we define control flow words,
\ but some simpler words will come first.

\ ---------- COMPARISONS AND NEGATE --------------------------------------------------------------

\ Because of space restriction of stage0.asm, only some comparisons are primitive. The rest can be
\ accomplished by inverting the result of a different comparison.
: <> = INVERT ;
: >= < INVERT ;
: <= > INVERT ;

: 0<> 0= INVERT ;
: 0>= 0< INVERT ;
: 0<= 0> INVERT ;

: U>= U< INVERT ;
: U<= U> INVERT ;

\ The way one should implement NEGATE depends on the way the computer represents negative numbers.
\ The system most computers use is called the two's complement, and in that case you should invert
\ all the bits and add one to compute the additive inverse.
: NEGATE INVERT 1+ ;

: CHAR WORD DROP C@ ;

\ ---------- THE UNINTUITIVE IMPLEMENTATION OF LITERAL -------------------------------------------

\ LITERAL is a compile-time word that is used to define computed number literals. Consider
\ : SOME-WORD [ 2 2 + ] LITERAL ;

\ This is equivalent to `: SOME-WORD 4 ;`, but sometimes you need to define a literal in the terms
\ of some address or constant, or you simply want to show where the value came from - LITERAL lets
\ you do this without a runtime penalty of recalculating the value every time it's used.

\ LITERAL can also be used without the square bracket part - when used with POSTPONE or [COMPILE],
\ the uglier and less versatile version of POSTPONE. The implementations of ['] is a good example.

\ Consider this simpler version first: `: LITERAL ['] LIT , , ; IMMEDIATE`. Since this is not what
\ you see below, you probably know there's something wrong with this implementation. Namely, there
\ is a cyclic dependency - ['] is implemented using LITERAL. Therefore, we need to think about the
\ compiled representation of this word (keep in mind that ['] does its work while compiling):

\ +--+--+--+--+--+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
\ | call DOCOL   | LIT   | LIT   |   ,   |   ,   | EXIT  |
\ +--+--+--+--+--+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+

\ Therefore, one can implement LITERAL as follows, with the only drawback being unintuitiveness.
: LITERAL LIT LIT , , ; IMMEDIATE

\ ---------- EXECUTION TOKENS --------------------------------------------------------------------

\ Forth specifies a mechanism called execution tokens, somewhat similar to function pointers in C.
\ An execution token is defined as a pointer to the first assembly instruction of a word. A lot of
\ words are defined using `:` and `;`. Words defined that way begin with a call to DOCOL, which is
\ then followed by a list of execution tokens, usually terminated by EXIT. To get an such a token,
\ you should pass a pointer to the dictionary entry of a word to >CFA.

: >CFA >FLAGS    \ ( flags-address )
  DUP C@         \ ( flags-address flags )
  F_LENMASK AND  \ ( flags-address name-length )
  + 1+           \ skip name-length bytes, plus one more for the flags byte itself
;

\ However, this is not the only way. In immediate mode, `' SOMEWORD` will push the execution token
\ of SOME-WORD. ' is not immediate, and sometimes you want to use ['] instead. Imagine you want to
\ manipulate the variable HANDLER-XT, which is supposed to contain an execution token. If you were
\ going to do it often in your program, you would probably want some words to do it for you:

\ : SET-HANDLER ' HANDLER-XT ! ;
\ : SWITCH-TO-FIRST-HANDLER ['] FIRST-HANDLER HANDLER-XT ! ;

\ Note that SET-HANDLER won't work like you expect in a word definition, i. e. you can't do
\ : SWITCH-TO-FIRST-HANDLER SET-HANDLER FIRST-HANDLER ;

\ To create an implementation of SET-HANDLER that works like that, you should look into `POSTPONE`
\ and `IMMEDIATE`, probably combined with `LITERAL` or `[']`.

\ Important: add a check for non-existent words after implementing all words necessary to do so.
: ' WORD FIND >CFA ;

\ ---------- POOR MAN'S POSTPONE -----------------------------------------------------------------

\ Imagine you wanted to implement ENDIF, a word that would be equivalent to THEN, to make it clear
\ for anyone reading your code that doesn't know anything about Forth. This can be accomplished by
\ using POSTPONE:
\ : ENDIF POSTPONE THEN ; IMMEDIATE

\ That way, THEN will be executed when ENDIF is used, instead of the moment ENDIF is defined. This
\ can also be used with non-immediate words, for example
\ : CELL+ POSTPONE 4+ ; IMMEDIATE
\ will not add the overhead of one more call in the callstack during execution, compared to
\ : CELL+ 4+ ;
\ You can do much more than these kinds of "aliases", but this is the simplest way to explain it. 

\ However, to implement POSTPONE you would have to detect whether a word is immediate, and compile
\ it differently based on that information. To avoid circular dependencies, we implement [COMPILE]
\ and COMPILE, which are limited versions of POSTPONE - [COMPILE] only handles immediate words and
\ COMPILE only handles non-immediate words. Using the wrong variant is undefined behavior.

\ [COMPILE] IF is equivalent to [ ' IF , ]
: [COMPILE] ' , ; IMMEDIATE

\ COMPILE DROP is equivalent to ['] DROP ,
: COMPILE R> \ get a pointer to the execution token of the word after COMPILE
  DUP @ ,    \ compile that execution token
  4+         \ move the pointer so that the word that has just been compiled won't get executed
  >R         \ put the pointer back on the return stack
;

\ ['] SOME-WORD is equivalent to [ ' SOME-WORD ] LITERAL
: ['] ' [COMPILE] LITERAL ; IMMEDIATE

\ [CHAR] X is equivalent to [ CHAR X ] LITERAL
: [CHAR] CHAR [COMPILE] LITERAL ; IMMEDIATE

\ With all of that up our sleeves we can pursue defining control flow words, starting with IF and
\ THEN.

\ before IF inside THEN after
\ compiles to

\ before 0BRANCH ptr inside after
\                 \_________^

: IF
  COMPILE 0BRANCH
  HERE @ \ save the location of the branch destination word on the data stack DURING COMPILATION
  0 , \ compile a dummy destination
; IMMEDIATE

: THEN \ ( ptr-addr )
  HERE @ \ ( ptr-addr ptr-val )
  SWAP !
; IMMEDIATE

\ before IF true ELSE false THEN after
\ compiles to

\ before 0BRANCH ptr1 true BRANCH ptr2 false after
\                 \________________|___^     ^
\                                  T         |
\                                  \_________/

: ELSE \ ( ptr1-addr )
  COMPILE BRANCH
  HERE @ \ ( ptr1-addr ptr2-addr )
  0 ,    \ compile a dummy ptr2
  HERE @ \ ( ptr1-addr ptr2-addr ptr1-val )
  ROT !
; IMMEDIATE

\ HIDDEN takes an address of a dictionary entry and toggles its hidden flag
: HIDDEN >FLAGS F_HIDDEN SWAP CXOR! ;
: HIDDEN? >FLAGS C@ F_HIDDEN AND 0<> ;
: HIDE WORD FIND HIDDEN ;

\ Used for checking whether a dictionary entry is marked immediate
: IMMEDIATE? >FLAGS C@ F_IMMED AND 0<> ;

: POSTPONE
  WORD FIND DUP IMMEDIATE? INVERT IF
    COMPILE COMPILE
  THEN
  >CFA ,
; IMMEDIATE

HIDE COMPILE
HIDE [COMPILE]

: ?DUP DUP IF DUP THEN ;

\ before BEGIN inside AGAIN after
\ compiles to

\ before inside BRANCH ptr after
\        ^______________/

: BEGIN HERE @ ; IMMEDIATE
: AGAIN POSTPONE BRANCH , ; IMMEDIATE

\ before BEGIN inside UNTIL after
\ compiles to

\ before inside 0BRANCH ptr after
\        ^_______________/

: UNTIL POSTPONE 0BRANCH , ; IMMEDIATE

\ before BEGIN condition WHILE inside REPEAT after
\ compiles to

\ before condition 0BRANCH ptr1 inside BRANCH ptr2 after
\        ^__________________|__________________/   ^
\                           \______________________/

: WHILE
  POSTPONE 0BRANCH
  HERE @
  0 ,
; IMMEDIATE

: REPEAT \ ( ptr2-val ptr1-addr )
  POSTPONE BRANCH
  SWAP , \ ( ptr1-addr )
  HERE @
  SWAP !
; IMMEDIATE

\ CASE                         ( push 0 during compilation to count the IFs )
\ test1 OF ... ENDOF           test1 OVER = IF DROP ... ELSE
\ test2 OF ... ENDOF           test2 OVER = IF DROP ... ELSE
\ test3 OF ... ENDOF           test3 OVER = IF DROP ... ELSE
\ default-case                 default-case
\ ENDCASE                      DROP THEN THEN THEN

: CASE 0 ; IMMEDIATE
: OF
  POSTPONE OVER
  POSTPONE =
  POSTPONE IF
  POSTPONE DROP
; IMMEDIATE

: ENDOF
  POSTPONE ELSE
; IMMEDIATE

: ENDCASE
  POSTPONE DROP
  BEGIN
    DUP 0<>
  WHILE
    POSTPONE THEN
  REPEAT
  DROP
; IMMEDIATE

\ this is enough control structures to define parenthesis comments
: (                    \ ( -- )
  1                    \ allow nested comments by storing the depth
  BEGIN KEY            \ ( depth key )
    CASE
      [CHAR] ( OF 1+ ENDOF
      [CHAR] ) OF 1- ENDOF
    ENDCASE
  DUP 0= UNTIL         \ ( depth )
  DROP                 \ ( )
; IMMEDIATE

: CELLS ( CELLS turns a number of cells into a number of bytes ) 2 LSHIFT ;
: CELL+ 4+ ;
: NIP ( a b -- b ) SWAP DROP ;
: TUCK ( b a -- a b a ) SWAP OVER ;
: PICK ( x(u) ... x(1) x(0) u -- x(u) ... x(1) x(0) x(u) )
  CELLS SP@ + ( x(u) ... x(1) x(0) addrof-x(u-1) )
  4+ @
;

: 2RDROP ( R: x x retaddr -- R: retaddr ) R> RDROP RDROP >R ;
: 2R>      ( R: x y retaddr -- x y R: retaddr )
  R> R> R> ( retaddr y x R: )
  ROT      ( y x retaddr R: )
  >R       ( y x R: retaddr )
  SWAP     ( x y R: retaddr )
;

: 2>R      ( x y R: retaddr -- R: x y retaddr )
  R>       ( x y retaddr R: )
  -ROT     ( retaddr x y R: )
  SWAP     ( retaddr y x R: )
  >R >R >R ( R: x y retaddr )
;

: 2DROP ( a b -- ) DROP DROP ;
: 2DUP ( a b -- a b a b ) OVER ( a b a ) OVER ( a b a b ) ;
: 2SWAP ( a b c d -- c d a b )
  >R ( a b c R: d )
  -ROT ( c a b R: d )
  R> ( c a b d )
  -ROT ( c d a b )
;

: 2OVER ( a b c d -- a b c d a b )
  2>R
  2DUP
  2R>
  2SWAP
;

( The primitive word /MOD leaves both the remainder and the quotient on the stack, in that order
  (on x86, the idiv instruction calculates both anyway). Now we can define / and MOD in terms of
  /MOD and stack manipulation words. )
: / /MOD NIP ;
: MOD /MOD DROP ;

: C, ( char -- ) HERE @ TUCK ! 1+ HERE ! ;

: WITHIN ( c a b -- within? ) OVER - >R - R> U< ;
: MINMAX ( a b -- min max )
  2DUP > IF
    SWAP
  THEN
;

: MIN MINMAX DROP ;
: MAX MINMAX NIP ;

: DEPTH ( -- n )
  S0 SP@ - 4- 2 RSHIFT
;

( string literals are compiled as follows:
    LITSTRING length string-itself rest-of-code )
: LITSTRING
  R@ 4+ ( string-address )
  R@ @  ( string-address string-length )
  R> OVER + 4+ >R ( move the return address )
;

( a perfect example of HIDE )
: COMPILE-STRING-CHARACTERS
  ( a helper function used to compile characters until a " )
  KEY DROP ( skip one character as the word separator )
  BEGIN
    KEY DUP [CHAR] " <>
  WHILE
    C,
  REPEAT
  DROP
;

: S"
  ( S" behaves correctly even in immediate mode )
  STATE @ IF
    ( we're in compile mode, compile LITSTRING )
    POSTPONE LITSTRING
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
; IMMEDIATE

HIDE COMPILE-STRING-CHARACTERS

(
  we can now define DO, ?DO, LOOP, +LOOP and LEAVE. It would be relatively simple if not for LEAVE.
  The plan for LEAVE is the following: at first, compile it as 2RDROP BRANCH LEAVE. When LOOP or
  +LOOP is executed, it looks for any instances of BRANCH or 0BRANCH with LEAVE as the destination
  and replace it with the appropriate address. The loop control parameters are stored on the return
  stack, with the counter on top.

  DO -> 2>R loop-inside
     *      ^
  ?DO -> (?DO) 0BRANCH LEAVE loop-inside
               *             ^
)

: (?DO)
  R>       ( limit counter retaddr )
  -ROT     ( retaddr limit counter )
  2DUP 2>R ( retaddr limit counter R: limit counter )
  <>       ( retaddr should-loop-at-all? )
  SWAP >R
;

(
  This means that LOOP should look for BRANCH LEAVE two cells before the actual pointer. This will
  handle ?DO correctly without breaking DO (the word before 2>R would have to be BRANCH, which
  would make no sense). * shows where LOOP will start correcting branches, and ^ shows the pointer
  passed to LOOP and the destination of the branch at the end of the loop.

   LOOP ->  (LOOP) 0BRANCH loop-beginning 2RDROP
  +LOOP -> (+LOOP) 0BRANCH loop-beginning 2RDROP
                                          ^ LEAVE jumps here
)

: (LOOP)  ( R: limit old-counter retaddr )
  R> 2R>        ( retaddr limit old-counter )
  1+            ( retaddr limit new-counter )
  2DUP 2>R      ( retaddr limit new-counter R: limit new-counter )
  =             ( retaddr should-stop-looping? R: limit new-counter )
  SWAP >R
;

: (+LOOP)   ( diff R: limit old-counter retaddr )
  R>              ( diff retaddr )
  SWAP            ( retaddr diff )
  2R>             ( retaddr diff limit old-counter )
  2 PICK OVER +   ( retaddr diff limit old-counter new-counter )
  ROT DUP >R      ( retaddr diff old-counter new-counter limit R: limit )
  -ROT DUP >R     ( retaddr diff limit old-counter new-counter R: limit new-counter )
  3 PICK          ( retaddr diff limit old-counter new-counter diff )
  0< IF SWAP THEN ( retaddr diff limit min-limit max-limit )
  1+ SWAP 1+ SWAP ( retaddr diff limit min-limit+1 max-limit+1 )
  WITHIN          ( retaddr diff should-stop-looping? )
  NIP SWAP >R     ( should-stop-looping? R: limit new-counter retaddr )
;

: LEAVE
  POSTPONE BRANCH
  [ LATEST @ >CFA ] LITERAL ,
; IMMEDIATE

: UNLOOP
  POSTPONE 2RDROP
; IMMEDIATE

: DO
  POSTPONE 2>R
  HERE @
; IMMEDIATE

: ?DO
  POSTPONE (?DO)
  POSTPONE 0BRANCH
  ['] LEAVE ,
  HERE @
; IMMEDIATE

: SOME-LOOP
  POSTPONE 0BRANCH
  DUP ,
  HERE @ ( loop-beginning loop-end )
  POSTPONE UNLOOP
  SWAP 4- ( loop-end curr-address )
  BEGIN
    DUP @ ['] LEAVE = IF
      ( loop-end curr-address )
      DUP 4- @ ( loop-end curr-address word-before )
      DUP ['] BRANCH = SWAP ['] 0BRANCH = OR IF
        2DUP ( loop-end curr-address loop-end curr-address ) !
      THEN
    THEN

    DUP @ CASE
      ['] LIT OF 8 + ENDOF
      ['] LITSTRING OF 4+ DUP @ 4+ + ENDOF
      SWAP 4+ SWAP
    ENDCASE

    2DUP <=
  UNTIL
  2DROP
;

: LOOP
  POSTPONE (LOOP)
  SOME-LOOP
; IMMEDIATE

: +LOOP
  POSTPONE (+LOOP)
  SOME-LOOP
; IMMEDIATE

: I     ( -- n ) POSTPONE R@ ; IMMEDIATE
: I-MAX ( -- n ) RP@  8 + @ ;
: J     ( -- n ) RP@ 12 + @ ;
: J-MAX ( -- n ) RP@ 16 + @ ;

HIDE (LOOP)
HIDE (+LOOP)
HIDE SOME-LOOP

: TYPE ( c-addr u -- ) 0 MAX 0 ?DO DUP C@ EMIT 1+ LOOP DROP ;

: ."
  POSTPONE S"
  STATE @ IF
    POSTPONE TYPE
  ELSE
    TYPE
  THEN
; IMMEDIATE

: PUSH-IMM32, $68 C, , ;
: NEXT, $AD C, $FF C, $E0 C, ;
: ALLOT HERE +! ;
: CREATE
  WORD
  CREATE-BARE
  HERE @ 8 + PUSH-IMM32, NEXT,
;
: CONSTANT WORD CREATE-BARE PUSH-IMM32, NEXT, ;
: VARIABLE CREATE 4 ALLOT ;
HIDE PUSH-IMM32,
HIDE NEXT,

: EXECUTE [ HERE @ 12 + ] LITERAL !
  DROP ( this DROP is overwritten by the previous line )
;

: ABS DUP 0< IF NEGATE THEN ;

: SPACE BL EMIT ;
: SPACES
  0 MAX
  0 ?DO SPACE LOOP
;

: ABORT
  CR
  ." ABORTED"
  CR
  BEGIN AGAIN
;

: COMPILE-ONLY
  STATE @ INVERT IF
    TYPE ."  is compile only."
    ABORT
  ELSE
    2DROP
  THEN
;

: IF       S" IF"       COMPILE-ONLY POSTPONE IF       ; IMMEDIATE
: ELSE     S" ELSE"     COMPILE-ONLY POSTPONE ELSE     ; IMMEDIATE
: THEN     S" THEN"     COMPILE-ONLY POSTPONE THEN     ; IMMEDIATE
: CASE     S" CASE"     COMPILE-ONLY POSTPONE CASE     ; IMMEDIATE
: ENDCASE  S" ENDCASE"  COMPILE-ONLY POSTPONE ENDCASE  ; IMMEDIATE
: OF       S" OF"       COMPILE-ONLY POSTPONE OF       ; IMMEDIATE
: ENDOF    S" ENDOF"    COMPILE-ONLY POSTPONE ENDOF    ; IMMEDIATE
: BEGIN    S" BEGIN"    COMPILE-ONLY POSTPONE BEGIN    ; IMMEDIATE
: AGAIN    S" AGAIN"    COMPILE-ONLY POSTPONE AGAIN    ; IMMEDIATE
: UNTIL    S" UNTIL"    COMPILE-ONLY POSTPONE UNTIL    ; IMMEDIATE
: WHILE    S" WHILE"    COMPILE-ONLY POSTPONE WHILE    ; IMMEDIATE
: REPEAT   S" REPEAT"   COMPILE-ONLY POSTPONE REPEAT   ; IMMEDIATE
: DO       S" DO"       COMPILE-ONLY POSTPONE DO       ; IMMEDIATE
: ?DO      S" ?DO"      COMPILE-ONLY POSTPONE ?DO      ; IMMEDIATE
: LOOP     S" LOOP"     COMPILE-ONLY POSTPONE LOOP     ; IMMEDIATE
: +LOOP    S" +LOOP"    COMPILE-ONLY POSTPONE +LOOP    ; IMMEDIATE
: LEAVE    S" LEAVE"    COMPILE-ONLY POSTPONE LEAVE    ; IMMEDIATE
: UNLOOP   S" UNLOOP"   COMPILE-ONLY POSTPONE UNLOOP   ; IMMEDIATE
: POSTPONE S" POSTPONE" COMPILE-ONLY POSTPONE POSTPONE ; IMMEDIATE
: LITERAL  S" LITERAL"  COMPILE-ONLY POSTPONE LITERAL  ; IMMEDIATE
: [']      S" [']"      COMPILE-ONLY POSTPONE [']      ; IMMEDIATE
: [CHAR]   S" [CHAR]"   COMPILE-ONLY POSTPONE [CHAR]   ; IMMEDIATE

( RECURSE calls the word that's currently being defined - using the name of the word directly will
  compile a call to the previous definition. This is also an example of how to use COMPILE-ONLY. )
: RECURSE S" RECURSE" COMPILE-ONLY LATEST @ >CFA , ; IMMEDIATE

: .DIGIT
  DUP 10 < IF
    [CHAR] 0 + EMIT
  ELSE
    10 - [CHAR] A + EMIT
  THEN
;

: B.R ( u width base -- )
  ROT OVER U/MOD ( width base rem quot )
  ?DUP IF
    ( width base rem quot )
    SWAP >R ( width base quot R: rem )
    ROT ( base quot width R: rem )
    1- 0 MAX
    ROT ( quot width base R: rem )
    RECURSE
    R> ( rem )
  ELSE
    NIP ( width rem )
    SWAP ( rem width )
    1- SPACES ( rem )
  THEN
  .DIGIT
;

: U.R 10 B.R ; : U.X 0 U.R ; : U. U.X SPACE ;
: H.R 16 B.R ; : H.X 0 H.R ; : H. H.X SPACE ;

: .R ( n width -- )
  SWAP 10 /MOD ( width rem quot )
  ?DUP IF
    ROT
    1- 0 MAX
    RECURSE
    ABS .DIGIT
  ELSE
    ( width rem )
    DUP 0< IF 2 ELSE 1 THEN
    ( width rem actual-width )
    ROT SWAP - ( rem width-diff )
    SPACES
    DUP 0< IF
      [CHAR] - EMIT
    THEN
    ABS
    .DIGIT
  THEN
;

: .X 0 .R ;
: . .X SPACE ;

: ?.
  DUP U.
  DUP 0< IF
    ." ("
    .X
    ." ) "
  ELSE
    DROP
  THEN
;

: .S
  ." <"
  DEPTH U.X
  ." > "
  DEPTH 0 ?DO
    S0 I 1+ CELLS - @ ?.
  LOOP
  CR
;

: H.S
  ." <$"
  DEPTH H.X
  ." > "
  DEPTH 0 ?DO
    ." $"
    S0 I 1+ CELLS - @ H.
  LOOP
  CR
;

: >UPPER ( char -- char )
  DUP [CHAR] a [CHAR] z 1+ WITHIN IF
    [ CHAR A CHAR a - ] LITERAL +
  THEN
;

: >LOWER ( char -- char )
  DUP [CHAR] A [CHAR] Z 1+ WITHIN IF
    [ CHAR a CHAR A - ] LITERAL +
  THEN
;

: FILL ( c-addr u char -- )
  -ROT 0 ?DO
    ( char c-addr )
    2DUP C! 1+
  LOOP
  2DROP
;


( redefine FILE to expand the dot in the filename to the appropriate amount of spaces and convert
  the filename to uppercase )

CREATE BUFFER 12 ALLOT
0 BUFFER 11 + !
( the last byte is never written to, but FindFile will slightly break when reporting a file not
  found error if the filename is not immediately followed by a null byte )

: LENGTH-CHECK ( curr-destination curr-maximum -- curr-destination | ABORT )
  BUFFER + OVER < IF
    ." Error: filename too long" ABORT
  THEN
;

: FILE ( filename filename-length -- )
  ." Reading "
  2DUP TYPE CR
  BUFFER TUCK 11 BL FILL                   ( source destination count )
  0 ?DO                                    ( source destination )
    OVER I + C@                            ( source destination char )
    DUP [CHAR] . = IF                      ( source destination char )
      DROP                                 ( source destination )
      8 LENGTH-CHECK                       ( source destination )
      DROP [ BUFFER 8 + ] LITERAL          ( source destination )
    ELSE                                   ( source destination char )
      >UPPER OVER C! 1+                    ( source destination )
    THEN                                   ( source destination )
    11 LENGTH-CHECK                        ( source destination )
  LOOP                                     ( source destination )
  2DROP
  BUFFER FILE ( old implementation, not recursion )
;

HIDE BUFFER
HIDE LENGTH-CHECK

: COUNTED> ( counted-string -- string strlen ) DUP 1+ SWAP C@ ;

: CONCLUDE"
  POSTPONE S"
  ROOT
  FILE
;

: ROLL
  SP@ OVER 1+ CELLS + @ SWAP
  0 SWAP ?DO
    SP@ DUP I CELLS + @ SWAP I 1+ CELLS + !
  -1 +LOOP
  DROP
;

." 2K Linux" CR
CONCLUDE" TEST.FRT"
