: S0      $7C00 ;
: BLK     $7C10 ;
: >IN     $7C14 ;
: LATEST  $7C18 ;
: STATE   $7C20 ;
: LENGTH  $7C24 ;

: HERE    $7C1C @ ;
: HERE!   $7C1C ! ;
: ROOT $882C @ LOAD ;

: F_IMMED   $80 ;
: F_HIDDEN  $20 ;
: F_LENMASK $1F ;

: #TAB 9 ;
: #CR 10 ;
: BL 32 ;

: FALSE 0 ;
: TRUE -1 ;

: 1+ -1 - ;
: 1-  1 - ;

: CELL 4 ;

: CELL+ -4 - ;
: CELL-  4 - ;
: CHAR+ 1+ ;
: CHAR- 1- ;

: DROP SP@ -4 - SP! ;
: DUP SP@ @ ;
: OVER SP@ CELL+ @ ;
: R@ RP@ 8 - @ ;
: R>
  RP@ 8 - @
  RP@ 4 - @
  RP@ 8 - !
  RP@ 4 - RP!
;
: >R
  RP@ 4 - @
  RP@ !
  RP@ 4 - !
  RP@ -4 - RP!
;
: NIP >R DROP R> ;
: SWAP
  OVER >R
  NIP
  R>
;
: TUCK SWAP OVER ;
: -ROT ROT ROT ;

: RDROP R> R> DROP >R ;

: 2DUP OVER OVER ;

: NEGATE >R 0 R> - ;
: + NEGATE - ;
: 2* DUP + ;

: OR 2DUP AND >R + R> - ;
: XOR 2DUP AND >R + R> 2* - ;

: C@ @ $FF AND ;
: C!
  DUP >R @
  $FFFFFF00 AND
  OR
  R> !
;

: +!    DUP >R @ +        R> ! ;
: -!    DUP >R @ - NEGATE R> ! ;
: OR!   DUP >R @ OR       R> ! ;
: XOR!  DUP >R @ XOR      R> ! ;
: AND!  DUP >R @ AND      R> ! ;
: COR!  DUP >R C@ OR      R> C! ;
: CXOR! DUP >R C@ XOR     R> C! ;
: CAND! DUP >R C@ AND     R> C! ;

: >FLAGS 2 + ;
: IMMEDIATE F_IMMED LATEST @ >FLAGS COR! ;

: KEY-NOEOF KEY ;
: UNGETC
  1 >IN -!
  1 LENGTH +!
;

: [ FALSE STATE ! ; IMMEDIATE
: ] TRUE STATE ! ;

: INVERT NEGATE 1- ;
: 0= 0 = ;
: 0<> 0= INVERT ;
: 0< $80000000 AND 0<> ;
: 0>= 0< INVERT ;
: 0<= DUP 0= >R 0< R> OR ;
: 0> 0<= INVERT ;

: <> = INVERT ;
: < $80000000 + >R $80000000 + R> U< ;
: > SWAP < ;
: >= < INVERT ;
: <= > INVERT ;

: U> SWAP U< ;
: U>= U< INVERT ;
: U<= U> INVERT ;

: ALLOT HERE + HERE! ;
: , HERE CELL ALLOT ! ;
: COMPILE R> DUP @ , CELL+ >R ;

: BRANCH R> @ >R ;
: 0BRANCH 0= DUP R@ @ AND SWAP INVERT R> CELL+ AND OR >R ;

: BEGIN HERE ; IMMEDIATE
: UNTIL COMPILE 0BRANCH , ; IMMEDIATE
: AGAIN COMPILE BRANCH , ; IMMEDIATE

: \ UNGETC
  BEGIN
    KEY-NOEOF #CR =
  UNTIL
; IMMEDIATE

: MUST-FIND FIND ;

: WHILE \ ( ptr2-val -- ptr1-addr ptr2-val )
  COMPILE 0BRANCH
  HERE             \ ( ptr2-val ptr1-addr )
  0 ,              \ a dummy destination
  SWAP
; IMMEDIATE

: REPEAT \ ( ptr1-addr ptr2-val -- )
  COMPILE BRANCH
  ,      \ ( ptr1-addr )
  HERE   \ resolve ptr1
  SWAP !
; IMMEDIATE

\ WORD is implemented in stage0, but not exposed.
: WORD
  BEGIN KEY-NOEOF DUP BL <= WHILE DROP REPEAT
  >R $7DDE R>
  BEGIN \ ( addr c )
    OVER C!
    CHAR+
    KEY DUP BL <=
  UNTIL
  DROP \ ( addr )
  $7DDE TUCK -
;

\ CHAR will parse a word and give you its first character.
: CHAR WORD DROP C@ ;

CHAR 2 EMIT

\ 2/ is an arithmetic shift and RSHIFT is a logical shift, so we have to preserve the top bit with
\ some bit twiddling.
: 2/            \ ( x )
  DUP >R        \ ( x x )
  1 RSHIFT      \ ( x x>>1 )
  R>            \ ( x>>1 x )
  $80000000 AND \ ( x>>1 topbit )
  OR
;

\ CELLS turns a number of cells into a number of bytes
: CELLS 2* 2* ;

\ Also known as the not exposed LIT in stage0
: (LITERAL) R@ @ R> CELL+ >R ;

\ (LITERAL) is not IMMEDIATE, so one can implement LITERAL like this:
: LITERAL (LITERAL) (LITERAL) , , ; IMMEDIATE

: >CFA >FLAGS    \ ( flags-address )
  DUP C@         \ ( flags-address flags )
  F_LENMASK AND  \ ( flags-address name-length )
  + 1+           \ skip name-length bytes, plus one more for the flags byte itself
;

: ' WORD MUST-FIND >CFA ;

\ IF: compile a conditional branch and push the address of the destination pointer on the stack.
: IF              \ ( -- ptr1-addr )
  COMPILE 0BRANCH
  HERE            \ save the address
  0 ,             \ compile a dummy ptr1
; IMMEDIATE

\ ELSE: compile an unconditional branch and resolve the previous, conditional branch.
: ELSE            \ ( ptr1-addr -- ptr2-addr )
  COMPILE BRANCH
  HERE            \ ( ptr1-addr ptr2-addr )
  0 ,             \ compile a dummy ptr2
  HERE            \ ( ptr1-addr ptr2-addr ptr1-val )
  ROT !           \ ( ptr2-addr )
; IMMEDIATE

\ THEN: resolve the previous branch.
: THEN            \ ( ptr-addr -- )
  >R HERE R> !
; IMMEDIATE

\ ---------- MAKING USE OF CONDITIONALS: EMIT ----------------------------------------------------

\ Now that we can use IF, let's implement a few pretty important words that need IF to work.

\ The underlying assembly implementation uses BIOS's teletype output interrupt, which uses CRLF as
\ the line ending - this overrides the implementation of EMIT to convert LF to CRLF on the fly.
: EMIT
  DUP #CR = IF
    13 EMIT
  THEN
  EMIT
;

CHAR K EMIT

\ ---------- HIDING WORDS ------------------------------------------------------------------------

\ Sometimes a word is only needed to implement something bigger, and should not be used after it's
\ used the few times it's designed for. This Forth provides HIDE just for these situations.

\ HIDDEN takes an address of a dictionary entry and toggles its hidden flag
: HIDDEN >FLAGS >R F_HIDDEN R> CXOR! ;

: HIDE WORD MUST-FIND HIDDEN ;

\ ---------- INSPECTING THE FLAGS FIELD ----------------------------------------------------------

: HIDDEN? >FLAGS C@ F_HIDDEN AND 0<> ;
: IMMEDIATE? >FLAGS C@ F_IMMED AND 0<> ;

\ ---------- MAKING USE OF CONDITIONALS: IMPLEMENTING A PROPER POSTPONE --------------------------

\ POSTPONE's job is simple: if a word is immediate, just compile it as if it wasn't. Otherwise, it
\ should be equivalent to COMPILE. This implementation does just that.
: POSTPONE
  WORD MUST-FIND DUP IMMEDIATE? INVERT IF
    COMPILE COMPILE
  THEN
  >CFA ,
; IMMEDIATE

\ Since we have POSTPONE, we don't need COMPILE anymore.
HIDE COMPILE

: [']    '    POSTPONE LITERAL ; IMMEDIATE
: [CHAR] CHAR POSTPONE LITERAL ; IMMEDIATE

\ ---------- MAKING USE OF CONDITIONALS: ?DUP ----------------------------------------------------

\ ?DUP is a useful word if you want to act on a value if it's non-zero. Compare:
\ DUP IF ... ELSE DROP THEN
\ ?DUP IF ... THEN
: ?DUP DUP IF DUP THEN ;

\ ---------- MAKING USE OF CONDITIONALS: S>D -----------------------------------------------------

\ The way you extend a number to two cells depends on its sign:
: S>D
  DUP 0< IF
    -1
  ELSE
    0
  THEN
;

\ While we're at it, let's define D>S.
: D>S DROP ;

\ ---------- MAKING USE OF CONDITIONALS: ABS -----------------------------------------------------

: ABS DUP 0< IF NEGATE THEN ;

\ Drop the address that points to the routine we're exiting from.
: EXIT RDROP ;

\ ---------- MULTIPLICATION AND DIVISION ---------------------------------------------------------

\ To save space, * and /MOD are not primitive, and are instead implemented using SM/REM or M*
: */MOD >R M* R> SM/REM ;
: */ */MOD NIP ;
: * M* D>S ;

: /MOD >R S>D R> SM/REM ;
: / /MOD NIP ;
: MOD /MOD DROP ;

\ x86 uses symmetric division, so we need to implement floored ourselves
: FM/MOD
  DUP >R \ save the divisor
  SM/REM
  OVER DUP 0<> SWAP 0< R@ 0< XOR AND IF \ if the remainder and the divisor have different signs,
    1- SWAP R> + SWAP  \ decrement the quotient and add the divisor to the quotient
  ELSE
    RDROP
  THEN
;

\ ---------- CASE STATEMENTS ---------------------------------------------------------------------

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
    ?DUP
  WHILE
    POSTPONE THEN
  REPEAT
; IMMEDIATE

\ ---------- PARENTHESIS COMMENTS ----------------------------------------------------------------

: (                    \ ( -- )
  1                    \ initial depth
  BEGIN KEY-NOEOF      \ ( depth key )
    CASE
      [CHAR] ( OF 1+ ENDOF
      [CHAR] ) OF 1- ENDOF
    ENDCASE            \ no default case is OK too!
    DUP 0=
  UNTIL                \ ( depth )
  DROP
; IMMEDIATE

( ---------- HAVING FUN WITH THE NEW TOY: STACK EFFECT COMMENTS! ------------------------------- )

: SPACE BL EMIT ;
SPACE CHAR L EMIT

( yes, I do indeed agree that my definition of fun is a weird one )

: PICK ( x(u) ... x(1) x(0) u -- x(u) ... x(1) x(0) x(u) )
  1+ CELLS SP@ + ( x(u) ... x(1) x(0) addrof-x(u) )
  @
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

: 2SWAP ( a b c d -- c d a b )
  >R    ( a b c R: d )
  -ROT  ( c a b R: d )
  R>    ( c a b d )
  -ROT  ( c d a b )
;

: 2OVER ( a b c d -- a b c d a b )
  2>R
  2DUP
  2R>
  2SWAP
;

: C, ( char -- ) HERE 1 ALLOT C! ;

: WITHIN ( c a b -- within? )
  OVER   ( c a b a )
  -      ( c a range-size )
  >R     ( c a R: range-size )
  -      ( distance-from-beginning-of-the-range R: range-size )
  R>     ( distance-from-beginning-of-the-range range-size )
  U<     ( within? )
;

: MINMAX ( a b -- min max )
  2DUP > IF
    SWAP
  THEN
;

: MIN ( a b -- min(a, b) ) MINMAX DROP ;
: MAX ( a b -- max(a, b) ) MINMAX NIP ;

: DEPTH ( -- n )
  S0 SP@ - CELL- 2 RSHIFT
;

( ---------- STRING LITERALS ------------------------------------------------------------------- )

( string literals are compiled like this:

 +--+--+--+--+---+---+---+---+- - - - - - - -+
 | LITSTRING | string-length | string-itself |
 +--+--+--+--+---+---+---+---+- - - - - - - -+ )

: LITSTRING
  R@ CELL+           ( string-address )
  R@ @               ( string-address string-length )
  R> OVER + CELL+ >R ( move the return address )
;

: COMPILE-STRING-CHARACTERS
  ( a helper function used to compile characters until a " )
  BEGIN
    KEY-NOEOF DUP [CHAR] " <>
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
    HERE ( save the address of the length word on the stack )
    0 ,    ( compile a dummy length )
    COMPILE-STRING-CHARACTERS
    DUP    ( length-addr length-addr )
    CELL+  ( length-addr first-char-addr )
    HERE   ( length-addr first-char-addr byte-after-last-char-addr )
    SWAP - ( length-addr length )
    SWAP ! ( )
  ELSE
    ( we're in immediate mode, use the currently free bytes but don't update HERE )
    HERE   ( first-char-addr )
    COMPILE-STRING-CHARACTERS
    HERE   ( first-char-addr byte-after-last-char-addr )
    OVER - ( first-char-addr length )
    OVER HERE! ( free the bytes )
  THEN
; IMMEDIATE

HIDE COMPILE-STRING-CHARACTERS

( ---------- VARIABLES ------------------------------------------------------------------------- )

: PUSH-IMM32, $68 C, , ;
: NEXT,
  $AD C, ( lodsd )
  $FF C, ( r=4 -> JMP r/m )
  $E0 C, ( r/m: eax / r=4 )
;

: REL! ( value addr -- ) DUP >R CELL+ - R> ! ;
: REL, ( value -- ) HERE CELL ALLOT REL! ;
: REL@ ( addr -- value ) DUP @ CELL+ + ;

: CREATE-BARE      ( name u -- )
  HERE LATEST @ -  ( name u link )
  HERE LATEST !
  DUP $FF AND C,
  8 RSHIFT C,      ( name u )
  DUP C,
  F_LENMASK AND
  BEGIN ( ptr u )
    ?DUP
  WHILE
    >R
    DUP C@ C,
    CHAR+ R> 1-
  REPEAT
  DROP
;

: CREATE
  WORD
  CREATE-BARE
  HERE 8 + PUSH-IMM32, NEXT,
;

: MKNOP WORD CREATE-BARE NEXT, ;

MKNOP ALIGN
MKNOP ALIGNED

: CONSTANT WORD CREATE-BARE PUSH-IMM32, NEXT, ;
: VARIABLE CREATE CELL ALLOT ;
HIDE PUSH-IMM32,
HIDE NEXT,

CHAR i EMIT

( ---------- COUNTED LOOPS --------------------------------------------------------------------- )

( We can now define DO, ?DO, LOOP, +LOOP and LEAVE. It would be much easier if LEAVE didn't exist,
  but oh well. Because storing LEAVE's data on the stack would interfere with other control flow
  inside the loop, let's store it in a variable. )

VARIABLE LEAVE-PTR

( Let's consider the base case: only one LEAVE in the loop. This can be trivially handled by
  storing the address we need to patch in the variable.

  This would also work quite well with nested loops. All we need to do is store the old value of
  the variable on the stack when opening a loop.

  Finally, we can extend this to an arbitrary number of LEAVEs by threading a singly-linked list
  through the branch target address holes. )

( The loop control variables are stored on the return stack, with the counter on top and the limit
  on the bottom.

  DO -> 2>R loop-inside

  ?DO -> (?DO) 0BRANCH [do the LEAVE thing but with a conditional jump] loop-inside
)

: (?DO)    ( limit counter R: retaddr -- R: limit counter retaddr )
  R>       ( limit counter retaddr )
  -ROT     ( retaddr limit counter )
  2DUP 2>R ( retaddr limit counter R: limit counter )
  <>       ( retaddr should-loop-at-all? )
  SWAP >R  ( should-loop-at-all? R: limit counter retaddr )
;

( This means that LOOP should look for LEAVE one cells before the actual loop body. That will make
  it handle ?DO correctly, and because the execution token of 2>R is not the same as the execution
  token of LEAVE, this will not break DO.

   LOOP ->  (LOOP) 0BRANCH loop-beginning 2RDROP
  +LOOP -> (+LOOP) 0BRANCH loop-beginning 2RDROP
                                          ^ LEAVE jumps here
)

: (LOOP)   ( R: limit old-counter retaddr )
  R> 2R>   ( retaddr limit old-counter )
  1+       ( retaddr limit new-counter )
  2DUP 2>R ( retaddr limit new-counter R: limit new-counter )
  =        ( retaddr should-stop-looping? R: limit new-counter )
  SWAP >R  ( should-stop-looping? R: limit new-counter retaddr )
;

: (+LOOP)         ( diff R: limit old-counter retaddr )
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

: LEAVE,
  HERE
  LEAVE-PTR @ ,
  LEAVE-PTR !
;

: LEAVE POSTPONE BRANCH LEAVE, ; IMMEDIATE

: UNLOOP
  POSTPONE 2RDROP
; IMMEDIATE

: DO
  LEAVE-PTR @
  0 LEAVE-PTR !
  POSTPONE 2>R
  HERE
; IMMEDIATE

: ?DO
  LEAVE-PTR @
  0 LEAVE-PTR !
  POSTPONE (?DO)
  POSTPONE 0BRANCH LEAVE,
  HERE
; IMMEDIATE

: SOME-LOOP
  POSTPONE 0BRANCH ,
  LEAVE-PTR @
  BEGIN
    ?DUP
  WHILE
    DUP @ >R
    HERE SWAP !
    R>
  REPEAT
  POSTPONE UNLOOP
  LEAVE-PTR !
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
: I-MAX ( -- n ) RP@ 12 - @ ;
: J     ( -- n ) RP@ 16 - @ ;
: J-MAX ( -- n ) RP@ 20 - @ ;

HIDE (?DO)
HIDE (LOOP)
HIDE (+LOOP)
HIDE SOME-LOOP

: LSHIFT 0 ?DO DUP + LOOP ;

CHAR n EMIT
( ---------- STRING HANDLING ------------------------------------------------------------------- )

: S=
  2 PICK <> IF DROP 2DROP FALSE EXIT THEN
  SWAP 0 ?DO
    OVER C@ OVER C@
    <> IF 2DROP UNLOOP FALSE EXIT THEN
    CHAR+ SWAP CHAR+ SWAP
  LOOP
  2DROP TRUE EXIT
;

: SCASE 0 ; IMMEDIATE
: SOF
  POSTPONE 2OVER
  POSTPONE S=
  POSTPONE IF
  POSTPONE 2DROP
; IMMEDIATE

: SENDOF
  POSTPONE ELSE
; IMMEDIATE

: SENDCASE
  POSTPONE 2DROP
  BEGIN
    ?DUP
  WHILE
    POSTPONE THEN
  REPEAT
; IMMEDIATE

: TYPE ( c-addr u -- ) 0 ?DO DUP C@ EMIT 1+ LOOP DROP ;

: ."
  POSTPONE S"
  STATE @ IF
    POSTPONE TYPE
  ELSE
    TYPE
  THEN
; IMMEDIATE

: COUNT ( counted-string -- string strlen ) DUP 1+ SWAP C@ ;

: EXECUTE [ HERE 12 + ] LITERAL !
  DROP ( this DROP is overwritten by the previous line )
;

: CR #CR EMIT ;
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

( ---------- MISCELLANEOUS --------------------------------------------------------------------- )
: FOLLOW-LINK
  DUP C@ OVER 1+ C@ 8 LSHIFT +
  DUP 0= IF
    CR ." FOLLOW-LINK: end of dictionary"
    ABORT
  THEN
  -
;

: FORGET
  WORD MUST-FIND
  DUP HERE!
  FOLLOW-LINK LATEST !
;

: CFA> ( xt -- dict )
  LATEST @
  BEGIN
    DUP >CFA 2 PICK = IF
      NIP EXIT
    THEN
    FOLLOW-LINK
  AGAIN
;

: CFA>NAME CFA> >FLAGS COUNT F_LENMASK AND ;

: (COMPILE-ONLY)
  STATE @ IF EXIT THEN
  R> CELL- ( address of (COMPILE-ONLY) xt )
  5 - ( address of CALL DOCOL, also the xt of the protected word )
  CFA>NAME TYPE ."  is compile only."
  ABORT
;

: COMPILE-ONLY IMMEDIATE POSTPONE (COMPILE-ONLY) ; IMMEDIATE
: RETRO
  WORD
  2DUP MUST-FIND >CFA >R
  CREATE-BARE DOCOL,
  POSTPONE COMPILE-ONLY
  R> ,
  POSTPONE ;
;

RETRO IF
RETRO ELSE
RETRO THEN
RETRO CASE
RETRO ENDCASE
RETRO OF
RETRO ENDOF
RETRO BEGIN
RETRO AGAIN
RETRO UNTIL
RETRO WHILE
RETRO REPEAT
RETRO DO
RETRO ?DO
RETRO LOOP
RETRO +LOOP
RETRO LEAVE
RETRO UNLOOP
RETRO POSTPONE
RETRO LITERAL
RETRO [']
RETRO [CHAR]

HIDE RETRO
CHAR u EMIT

VARIABLE RECURSE-XT

( RECURSE calls the word that's currently being defined - using the name of the word directly will
  compile a call to the previous definition. This is also an example of how to use COMPILE-ONLY.
  It would be simpler to just do LATEST @ >CFA, but that does not work with :NONAME. )
: RECURSE COMPILE-ONLY RECURSE-XT @ , ; IMMEDIATE
: : WORD F_HIDDEN OR CREATE-BARE HERE RECURSE-XT ! DOCOL, ] ;
: :NONAME HERE DUP RECURSE-XT ! DOCOL, ] ;

: .DIGIT
  DUP 10 < IF
    [CHAR] 0 + EMIT
  ELSE
    10 - [CHAR] A + EMIT
  THEN
;

: B.R ( u width base -- )
  ROT ( width base u )
  0   ( width base ud )
  2 PICK ( width base ud base )
  UM/MOD ( width base rem quot )
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

: UPPER? [CHAR] A [CHAR] Z 1+ WITHIN ;
: LOWER? [CHAR] a [CHAR] z 1+ WITHIN ;
: DIGIT? [CHAR] 0 [CHAR] 9 1+ WITHIN ;
: ALPHA? DUP UPPER? SWAP LOWER? OR ;
: ALNUM? DUP ALPHA? SWAP DIGIT? OR ;

: >UPPER ( char -- char )
  DUP LOWER? IF
    [ CHAR A CHAR a - ] LITERAL +
  THEN
;

: >LOWER ( char -- char )
  DUP UPPER? IF
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

CREATE BUFFER 11 ALLOT

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
CHAR x EMIT

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

: 2@ DUP CELL+ @ SWAP @ ;
: 2! SWAP OVER ! CELL+ ! ;
: 2R@ R> 2R> 2DUP 2>R ROT >R ;
MKNOP CHARS

: CMOVE
  0 ?DO ( src dst )
    OVER C@
    OVER C!
    CHAR+ >R CHAR+ R>
  LOOP 2DROP
;

: MOVE
  >R
  2DUP < IF
    0 R> 1- ?DO
      OVER I + C@
      OVER I + C!
    -1 +LOOP
    2DROP
  ELSE
    R> CMOVE
  THEN
;

: DEFER-DEFAULT
  CR ." DEFER-DEFAULT: "
  5 - ( because a CALL is 5 bytes long )
  CFA>NAME TYPE ."  used before being defined with IS"
  ABORT
;

: DEFER
  WORD CREATE-BARE
  $E8 C, ( call )
  ['] DEFER-DEFAULT REL,
;

: DEFER@ ( xt -- inner )
  1+ ( skip the jmp/call )
  REL@
;

: DEFER! ( new xt -- )
  $E9 OVER C!
  1+ REL!
;

: IS
  STATE @ IF
    POSTPONE ['] POSTPONE DEFER!
  ELSE
    ' DEFER!
  THEN
; IMMEDIATE

: ACTION-OF
  STATE @ IF
    POSTPONE ['] POSTPONE DEFER@
  ELSE
    ' DEFER@
  THEN
; IMMEDIATE

:NONAME
  2DUP
  FIND
  DUP 0= IF
    ." MUST-FIND: can't find "
    DROP TYPE ABORT
  THEN
  NIP NIP
; IS MUST-FIND

:NONAME
  KEY
  DUP 0= IF
    ." KEY-NOEOF: EOF" ABORT
  THEN
; IS KEY-NOEOF

CR
CONCLUDE" TEST.FRT"
