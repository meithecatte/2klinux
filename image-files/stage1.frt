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

: IMMEDIATE
  LATEST @
  2 + F_IMMED SWAP COR!
; IMMEDIATE

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

\ ROOT is a word that LOADs the first cluster of the root directory, and since it's dependant on
\ the address of BPBRootCluster, it's also defined at the beginning of this file.

\ Below, two character constants are defined - BL (for BLank) returns the character value of the
\ space character, and NL (for New Line) - the character value of the newline character. These can
\ be used with EMIT to print characters on screen, as demonstated by CR.

\ In Forth, FALSE is represented by a cell with all bits unset, and TRUE is represented by a cell
\ with all bits set, which corresponds with the two's complement representation of -1.

\ OR!, XOR! and AND! all combine the corresponding bitwise operation with ! in a similar manner to
\ +! and -!: VAR @ $12 XOR VAR ! is equivalent to $12 VAR XOR!

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

: U>= U< INVERT ;
: U<= U> INVERT ;

\ In two's complement, you invert all the bits and add one to compute the additive inverse.
: NEGATE INVERT 1+ ;

\ Used for checking whether a dictionary entry is marked immediate
: IMMEDIATE? 2 + C@ F_IMMED AND 0<> ;

\ >CFA takes an address of a word in the dictionary and returns its execution token, i. e. the
\ address of its first assembly instruction (`call DOCOL' in case of Forth words)
: >CFA 2 + \ ( flags-address )
  DUP C@   \ ( flags-address flags )
  F_LENMASK AND  \ ( flags-address name-length )
  + 1+     \ skip name-length bytes, plus one bytes for the flags byte itself
;

\ ' SOME-WORD will push the execution token of SOME-WORD
: ' WORD FIND >CFA ;

\ LITERAL is a compile-time word that is used to define computed number literals. Consider
\ : SOME-WORD [ 2 2 + ] LITERAL ;
\ This is equivalent to : SOME-WORD 4 ; but sometimes you want to show where a value comes from
\ without recalculating it every time the word is executed. It is also useful when defining compile
\ time words, when combined with POSTPONE or [COMPILE]

\ Consider this simpler version first: : LITERAL IMMEDIATE ['] LIT , , ;
: LITERAL IMMEDIATE
  [ ' LIT     \ ' does not behave the way one could expect it to in compile mode. ['] is what you
              \ should use in such a case, but we need LITERAL to implement [']
    DUP , , ] \ LIT LIT will push the address of LIT on the stack
  , ,
;

\ [COMPILE] can be used on immediate words only
: [COMPILE] IMMEDIATE ' , ;

\ ['] SOME-WORD is equivalent to [ ' SOME-WORD ] LITERAL
: ['] IMMEDIATE ' [COMPILE] LITERAL ;

\ COMPILE can be used on non-immediate words only, COMPILE SOME-WORD <=> ['] SOME-WORD ,
: COMPILE IMMEDIATE
  ' [COMPILE] LITERAL
  ['] , , \ COMPILE ,
;

\ COMPILE and [COMPILE] will later get merged into one word called POSTPONE, but we need to define
\ IF, ELSE and THEN first

\ RECURSE calls the word that's currently being defined
: RECURSE IMMEDIATE LATEST @ >CFA , ;

: CHAR WORD DROP C@ ;

\ [CHAR] X is equivalent to [ CHAR X ] LITERAL
: [CHAR] IMMEDIATE CHAR [COMPILE] LITERAL ;

\ With all of that up our sleeves we can pursue defining control flow words, starting with IF and
\ THEN.

\ before IF inside THEN after
\ compiles to

\ before 0BRANCH ptr inside after
\                 \_________^

: IF IMMEDIATE
  COMPILE 0BRANCH
  HERE @ \ save the location of the branch destination word on the data stack DURING COMPILATION
  0 , \ compile a dummy destination
;

: THEN IMMEDIATE \ ( ptr-addr )
  HERE @ \ ( ptr-addr ptr-val )
  SWAP !
;

\ before IF true ELSE false THEN after
\ compiles to

\ before 0BRANCH ptr1 true BRANCH ptr2 false after
\                 \________________|___^     ^
\                                  T         |
\                                  \_________/

: ELSE IMMEDIATE \ ( ptr1-addr )
  COMPILE BRANCH
  HERE @ \ ( ptr1-addr ptr2-addr )
  0 ,    \ compile a dummy ptr2
  HERE @ \ ( ptr1-addr ptr2-addr ptr1-val )
  ROT !
;

: POSTPONE IMMEDIATE
  WORD FIND DUP IMMEDIATE? IF
    >CFA ,
  ELSE
    >CFA [COMPILE] LITERAL
    COMPILE ,
  THEN
;

\ HIDDEN takes an address of a dictionary entry and toggles its hidden flag
: HIDDEN 2 + F_HIDDEN SWAP CXOR! ;
: HIDDEN? 2 + C@ F_HIDDEN AND 0<> ;
: HIDE WORD FIND HIDDEN ;

HIDE COMPILE
HIDE [COMPILE]

: ?DUP DUP IF DUP THEN ;

\ before BEGIN inside AGAIN after
\ compiles to

\ before inside BRANCH ptr after
\        ^______________/

: BEGIN IMMEDIATE HERE @ ;
: AGAIN IMMEDIATE POSTPONE BRANCH , ;

\ before BEGIN inside UNTIL after
\ compiles to

\ before inside 0BRANCH ptr after
\        ^_______________/

: UNTIL IMMEDIATE POSTPONE 0BRANCH , ;

\ before BEGIN condition WHILE inside REPEAT after
\ compiles to

\ before condition 0BRANCH ptr1 inside BRANCH ptr2 after
\        ^__________________|__________________/   ^
\                           \______________________/

: WHILE IMMEDIATE
  POSTPONE 0BRANCH
  HERE @
  0 ,
;

: REPEAT IMMEDIATE \ ( ptr2-val ptr1-addr )
  POSTPONE BRANCH
  SWAP , \ ( ptr1-addr )
  HERE @
  SWAP !
;

\ CASE                         ( push 0 during compilation to count the necessary amount of IFs )
\ test1 OF ... ENDOF           test1 OVER = IF DROP ... ELSE
\ test2 OF ... ENDOF           test2 OVER = IF DROP ... ELSE
\ test3 OF ... ENDOF           test3 OVER = IF DROP ... ELSE
\ default-case                 default-case
\ ENDCASE                      DROP THEN THEN THEN

: CASE IMMEDIATE 0 ;
: OF IMMEDIATE
  POSTPONE OVER
  POSTPONE =
  POSTPONE IF
  POSTPONE DROP
;

: ENDOF IMMEDIATE
  POSTPONE ELSE
;

: ENDCASE IMMEDIATE
  POSTPONE DROP
  BEGIN
    DUP 0<>
  WHILE
    POSTPONE THEN
  REPEAT
  DROP
;

\ this is enough control structures to define parenthesis comments
: ( IMMEDIATE          \ ( -- )
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
  4+ @
;

: 2DROP ( a b -- ) DROP DROP ;
: 2DUP ( a b -- a b a b ) OVER ( a b a ) OVER ( a b a b ) ;
: 2SWAP ( a b c d -- c d a b )
  >R ( a b c R: d )
  -ROT ( c a b R: d )
  R> ( c a b d )
  -ROT ( c d a b )
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

( The primitive word /MOD leaves both the remainder and the quotient on the stack, in that order
  (on x86, the idiv instruction calculates both anyway). Now we can define / and MOD in terms of
  /MOD and stack manipulation words. )
: / /MOD NIP ;
: MOD /MOD DROP ;

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
;

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

: HALT BEGIN AGAIN ;
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

: LEAVE IMMEDIATE
  POSTPONE BRANCH
  [ LATEST @ >CFA ] LITERAL ,
;

: UNLOOP IMMEDIATE
  POSTPONE 2RDROP
;

: DO IMMEDIATE
  POSTPONE 2>R
  HERE @
;

: ?DO IMMEDIATE
  POSTPONE (?DO)
  POSTPONE 0BRANCH
  ['] LEAVE ,
  HERE @
;

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

: LOOP IMMEDIATE
  POSTPONE (LOOP)
  SOME-LOOP
;

: +LOOP IMMEDIATE
  POSTPONE (+LOOP)
  SOME-LOOP
;

: I RP@ 4 + @ ;
: J RP@ 12 + @ ;

HIDE (LOOP)
HIDE (+LOOP)
HIDE SOME-LOOP

: TYPE 0 ?DO DUP C@ EMIT 1+ LOOP DROP ;

: ." IMMEDIATE
  POSTPONE S"
  STATE @ IF
    POSTPONE TYPE
  ELSE
    TYPE
  THEN
;

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

: COMPILE-ONLY
  STATE @ INVERT IF
    TYPE ."  is compile only."
    HALT
  ELSE
    2DROP
  THEN
;

: IF       IMMEDIATE S" IF"       COMPILE-ONLY POSTPONE IF ;
: ELSE     IMMEDIATE S" ELSE"     COMPILE-ONLY POSTPONE ELSE ;
: THEN     IMMEDIATE S" THEN"     COMPILE-ONLY POSTPONE THEN ;
: CASE     IMMEDIATE S" CASE"     COMPILE-ONLY POSTPONE CASE ;
: ENDCASE  IMMEDIATE S" ENDCASE"  COMPILE-ONLY POSTPONE ENDCASE ;
: OF       IMMEDIATE S" OF"       COMPILE-ONLY POSTPONE OF ;
: ENDOF    IMMEDIATE S" ENDOF"    COMPILE-ONLY POSTPONE ENDOF ;
: BEGIN    IMMEDIATE S" BEGIN"    COMPILE-ONLY POSTPONE BEGIN ;
: AGAIN    IMMEDIATE S" AGAIN"    COMPILE-ONLY POSTPONE AGAIN ;
: UNTIL    IMMEDIATE S" UNTIL"    COMPILE-ONLY POSTPONE UNTIL ;
: WHILE    IMMEDIATE S" WHILE"    COMPILE-ONLY POSTPONE WHILE ;
: REPEAT   IMMEDIATE S" REPEAT"   COMPILE-ONLY POSTPONE REPEAT ;
: DO       IMMEDIATE S" DO"       COMPILE-ONLY POSTPONE DO ;
: ?DO      IMMEDIATE S" ?DO"      COMPILE-ONLY POSTPONE ?DO ;
: LOOP     IMMEDIATE S" LOOP"     COMPILE-ONLY POSTPONE LOOP ;
: +LOOP    IMMEDIATE S" +LOOP"    COMPILE-ONLY POSTPONE +LOOP ;
: LEAVE    IMMEDIATE S" LEAVE"    COMPILE-ONLY POSTPONE LEAVE ;
: UNLOOP   IMMEDIATE S" UNLOOP"   COMPILE-ONLY POSTPONE UNLOOP ;
: POSTPONE IMMEDIATE S" POSTPONE" COMPILE-ONLY POSTPONE POSTPONE ;
: RECURSE  IMMEDIATE S" RECURSE"  COMPILE-ONLY POSTPONE RECURSE ;
: LITERAL  IMMEDIATE S" LITERAL"  COMPILE-ONLY POSTPONE LITERAL ;
: [']      IMMEDIATE S" [']"      COMPILE-ONLY POSTPONE ['] ;
: [CHAR]   IMMEDIATE S" [CHAR]"   COMPILE-ONLY POSTPONE [CHAR] ;

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

: . 0 .R SPACE ;

: .S
  [CHAR] < EMIT
  DEPTH U.X
  [CHAR] > EMIT
  SPACE
  DEPTH 0 ?DO
    S0 I 1+ CELLS - @ U.
  LOOP
  CR
;

1234 U. CR
$DEADBEEF H. CR
1234 7 U.R CR
$BCD 4 H.R CR

1234 0 .R CR
-123 0 .R CR
1234 5 .R CR
-123 5 .R CR

.S
123 -456 789 .S

: CONCLUDE"
  POSTPONE S"
  DROP ( we don't need the count )
  ROOT FILE
;

." It's working!" CR
CONCLUDE" TEST    FRT"
