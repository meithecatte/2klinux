: R0      $1500 ;
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

: 1+  1 + ;
: 1- -1 + ;

: INVERT -1 XOR ;
: NEGATE INVERT 1+ ;
: - NEGATE + ;

: +!   DUP @ ROT +   SWAP ! ;
: -!   DUP @ ROT -   SWAP ! ;
: OR!  DUP @ ROT OR  SWAP ! ;
: XOR! DUP @ ROT XOR SWAP ! ;
: AND! DUP @ ROT AND SWAP ! ;

: C@ @ $FF AND ;
: C!
  DUP @
  $FFFFFF00 AND
  ROT OR
  SWAP !
;

: COR!  DUP C@ ROT OR  SWAP C! ;
: CXOR! DUP C@ ROT XOR SWAP C! ;
: CAND! DUP C@ ROT AND SWAP C! ;

: >FLAGS 2 + ;
: IMMEDIATE F_IMMED LATEST @ >FLAGS COR! ;

: KEY-NOEOF KEY ;
: UNGETC
  1 >IN -!
  1 LENGTH +!
;

: [ FALSE STATE ! ; IMMEDIATE
: ] TRUE STATE ! ;

: CELL 4 ;

: CELL+ CELL + ;
: CELL- CELL - ;
: CHAR+ 1+ ;
: CHAR- 1- ;

: ALLOT HERE + HERE! ;
: , HERE CELL ALLOT ! ;
: COMPILE R> DUP @ , CELL+ >R ;

: BEGIN HERE ; IMMEDIATE
: UNTIL COMPILE 0BRANCH , ; IMMEDIATE
: AGAIN COMPILE BRANCH , ; IMMEDIATE

: \ UNGETC
  BEGIN
    KEY-NOEOF #CR =
  UNTIL
; IMMEDIATE

\ ---------- AN EXPLANATION OF WHAT HAS JUST HAPPENED --------------------------------------------

\ Forth is a very extensible language. For example, you can even define your own comment syntax at
\ runtime. Because space is tight in the initial binary, no comment words are included, what makes
\ doing so a necessity, instead of being just an exercise. However, to define the comment word you
\ see above, I needed some other words, which I will explain here.

\ At the very beginning of this file, a few words that need to hardcode addresses are defined. The
\ first batch contains simple constants:
\ R0 - the initial value of the return stack pointer
\ S0 - the initial value of the data stack pointer
\ BLK - holds the cluster number of the currently loaded cluster
\ >IN - holds the offset in the cluster buffer of the first unparsed character. Together with BLK,
\       it can be used to save and restore the current file position, for example in order to read
\       multiple files at once, which is necessary for implementing the C preprocessor.
\ LATEST - holds the address of the last word defined
\ STATE - FALSE if interpreting words, TRUE when compiling
\ LENGTH - the number of bytes left in the currently open file. When this goes down to 0, KEY will
\          return zeroes indefinitely. Since binary files are not expected, this should be handled
\          appropriately by anything using KEY directly.

\ Below, you can see words that do a little bit more:
\ HERE - push the current value of the data pointer
\ HERE! - deallocate memory: given an address, make it the first free byte in the data space
\ ALLOT - allocate memory
\ ROOT - load the root directory, often used before FILE

\ Next, the constants describing the flags field of a dictionary entry are defined. If you want to
\ learn more, they are discussed throroughly in stage0.s

\ After that we see the character constants:
\ #TAB - the ASCII value of the tab character
\ #CR - the ASCII value of the linefeed character
\ BL - the ASCII value of the space character

\ In Forth, FALSE is represented by a cell with all bits unset, and TRUE - by a cell with all bits
\ set, which corresponds with the two's complement representation of -1. This representation makes
\ bitwise and logical operations equivalent, which avoids cluttering up the dictionary.  While any
\ non-zero cell would work as TRUE with the control flow words, these two values are the canonical
\ representations, which becomes important when dealing with logical operations.

\ OR!, XOR! and AND! all combine the corresponding bitwise operation with ! in a similar manner to
\ +! or -!. For example, `VAR @ $12 XOR VAR !` is equivalent to `$12 VAR XOR!`. Below, you can see
\ their one-byte equivalents, which are prefixed with C, like C@, C! and C,.

\ Finally, >FLAGS is a word that takes a pointer to the link field of a dictionary entry and turns
\ it into a pointer to the flags field.

\ All of this makes it possible to define IMMEDIATE. Unsurprisingly, IMMEDIATE is used to mark the
\ word defined last as immediate. This flag means that INTERPRET runs the marked word immediately,
\ _even if it's in compile mode_. One example is the ; word used to end colon definitions. We want
\ it to run now, not when the new word is used, because that would trap us in compile mode.

\ The way IMMEDIATE is implemented is surprisingly simple. While the grammar of Forth is different
\ from English grammar, it is fair to say that the literal translation would be "Set the IMMEDIATE
\ bit of the last word defined".

\ KEY-NOEOF is a word that errors on EOF, instead of just returning a zero. Since conditionals are
\ not yet implemented, this is a simple wrapper around KEY until it gets overwritten with IS. This
\ is fine, because we don't expect an EOF condition before this file ends (duh). We will also need
\ a similar word, MUST-FIND, that errors if a word is not in the dictionary:
: MUST-FIND FIND ;

\ Finally, there's UNGETC, that undoes the last KEY operation. Trying to UNGETC more than once can
\ result in... well... just don't do it.

\ [ and ] can be used to temporarily enter the interpretation mode while defining a word, which is
\ most often used for compile-time expression evalutaion.

\ These two words are used while defining \, since loop constructs are not yet available. Consider
\   : \ UNGETC BEGIN KEY-NOEOF #CR = UNTIL ; IMMEDIATE
\ The UNGETC is necessary because WORD will read one whitespace character after the word. It means
\ that an implementation of \ without UNGETC would behave incorrectly when immediately followed by
\ a newline. You can see how simple the following loop is - skip characters until a newline. Since
\ BEGIN and UNTIL are not available so early, we use [ and ] to simulate them. How exactly this is
\ done will become clear once you see how control flow is done. Finally, \ is marked immediate, to
\ make comments work correctly in compile mode.

\ ---------- THE MOST BASIC OF WORDS -------------------------------------------------------------

\ Because of space restriction of stage0.asm, only some comparisons are primitive. The rest can be
\ accomplished by combining other comparisons.
: 0= 0 = ;
: 0<> 0= INVERT ;
: 0< $80000000 AND 0<> ;
: 0>= 0< INVERT ;
: 0<= DUP 0= SWAP 0< OR ;
: 0> 0<= INVERT ;

: <> = INVERT ;
: >= < INVERT ;
: <= OVER OVER = >R < R> OR ;
: > <= INVERT ;

: U>= U< INVERT ;
: U<= OVER OVER = >R U< R> OR ;
: U> U<= INVERT ;

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

: NIP SWAP DROP ;
: TUCK SWAP OVER ;

\ WORD is implemented in stage0, but not exposed.
: WORD
  BEGIN KEY-NOEOF DUP BL <= WHILE DROP REPEAT
  $7DDE SWAP
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

\ 2* and 2/ are separate words to make it possible to use the shift instructions of the processor.
: 2* DUP + ;

\ 2/ is an arithmetic shift and RSHIFT is a logical shift, so we have to preserve the top bit with
\ some bit twiddling.
: 2/            \ ( x )
  DUP           \ ( x x )
  1 RSHIFT      \ ( x x>>1 )
  SWAP          \ ( x>>1 x )
  $80000000 AND \ ( x>>1 topbit )
  OR
;

\ CELLS turns a number of cells into a number of bytes
: CELLS 2* 2* ;

\ ---------- THE UNINTUITIVE IMPLEMENTATION OF LITERAL -------------------------------------------

\ LITERAL is a compile-time word that is used to define computed number literals. Consider
\   : FOUR [ 2 2 + ] LITERAL ;

\ LITERAL can also be used without the square bracket part - when used with POSTPONE or [COMPILE],
\ the uglier and less versatile version of POSTPONE. The implementations of ['] is a good example.

\ Consider this simpler version first:
\   : LITERAL ['] LIT , , ; IMMEDIATE
\ Since it is not what you see below, you can probably infer that there's something wrong with it.
\ Namely, there is a cyclic dependency - ['] is implemented using LITERAL. However, there is a not
\ very complicated way out. Think about how ['] would handle the above. It's just

\ +--+--+--+--+--+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
\ |  call DOCOL  |  LIT  |  LIT  |   ,   |   ,   | EXIT  |
\ +--+--+--+--+--+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+

\ LIT is not IMMEDIATE, so one can implement LITERAL like this:
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

\ With >CFA available, implementing ' is a piece of cake: get a word, find it in a dictionary, and
\ turn it into an execution token.
: ' WORD MUST-FIND >CFA ;

\ ---------- POOR MAN'S POSTPONE -----------------------------------------------------------------

\ Imagine you wanted to implement ENDIF, a word that would be equivalent to THEN, to make it clear
\ for anyone reading your code that doesn't know anything about Forth. This can be accomplished by
\ using POSTPONE:
\ : ENDIF POSTPONE THEN ; IMMEDIATE

\ That way, THEN will be executed when ENDIF is used, instead of the moment ENDIF is defined. This
\ can also be used with non-immediate words, if such needs arise. This is a small fraction of what
\ POSTPONE can do, but I think this is the simplest way to explain it.

\ However, to implement POSTPONE you would have to detect whether a word is immediate, and compile
\ it differently based on that information. To avoid circular dependencies, we implement [COMPILE]
\ and COMPILE, which are limited versions of POSTPONE - [COMPILE] only handles immediate words and
\ COMPILE only handles non-immediate words. Using the wrong variant is undefined behavior.

\ [COMPILE] IF is equivalent to [ ' IF , ]
: [COMPILE] ' , ; IMMEDIATE

\ ---------- MAKING USE OF LITERAL - ['] and [CHAR] ----------------------------------------------

\ ' and CHAR are great, but sometimes you would want them to work differently in compile mode. You
\ are thinking about their wrapped-in-square-bracked counterparts. They both work very similarily:
\ ['] SOME-WORD is equivalent to [ ' SOME-WORD ] LITERAL, and [CHAR] X is equivalent to [ CHAR X ]
\ LITERAL. For any such "macro" you just need to POSTPONE everything except the stuff in brackets,
\ or if you don't have POSTPONE, think about the immediancy of the words and use [COMPILE], or its
\ bracketless equivalent.

: [']    '    [COMPILE] LITERAL ; IMMEDIATE
: [CHAR] CHAR [COMPILE] LITERAL ; IMMEDIATE

\ ---------- CONTROL FLOW - IF ELSE THEN ---------------------------------------------------------

\ Let's look at how you can implement a conditional using BRANCH and 0BRANCH:

\ +--+--+--+--+-+-+-+-+- - - - -+
\ |  0BRANCH  | ptr1  | if-part | after
\ +--+--+--+--+-+ | +-+- - - - -+   ^
\                 \_________________/

\                                                ___________________
\                                               /                   \
\ +--+--+--+--+-+-+-+-+- - - - -+--+--+--+--+-+ | +-+- - - - - -+   V
\ |  0BRANCH  | ptr1  | if-part |  BRANCH   | ptr2  | else-part | after
\ +--+--+--+--+-+ | +-+- - - - -+--+--+--+--+-+-+-+-+  ^  - - - +
\                 \____________________________________/

\ To control the branch destinations, we can use the stack. You should keep in mind that all of it
\ happens during compilation, so it will not interfere with the stack usage during execution.

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
  HERE            \ ( ptr-addr ptr-val )
  SWAP !
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

\ ---------- HIDING WORDS ------------------------------------------------------------------------

\ Sometimes a word is only needed to implement something bigger, and should not be used after it's
\ used the few times it's designed for. This Forth provides HIDE just for these situations.

\ HIDDEN takes an address of a dictionary entry and toggles its hidden flag
: HIDDEN >FLAGS F_HIDDEN SWAP CXOR! ;

: HIDE WORD MUST-FIND HIDDEN ;

\ ---------- INSPECTING THE FLAGS FIELD ----------------------------------------------------------

\ Since they are about to get useful, let's implement words that will tell you if a flag is set in
\ a dictionary entry.

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

\ Since we have POSTPONE, we don't need those.
HIDE COMPILE
HIDE [COMPILE]

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

\ ---------- MANIPULATING THE RETURN STACK -------------------------------------------------------

: RDROP R> R> DROP >R ;
: R@ R> R> DUP >R SWAP >R ;

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
  DUP >R     \ save the divisor
  SM/REM
  OVER DUP 0<> SWAP 0< R@ 0< XOR AND IF \ if the remainder and the divisor have different signs,
    1- SWAP R> + SWAP  \ decrement the quotient and add the divisor to the quotient
  ELSE
    RDROP
  THEN
;

\ ---------- BASIC LOOPING -----------------------------------------------------------------------

\ The most basic loop in Forth is made using BEGIN and UNTIL. This code example will print numbers
\ from 1 to 5:

\ 0 BEGIN 1+ . DUP 5 > UNTIL

\ UNTIL jumps to the corresponding BEGIN unless TRUE is found on the stack. When compiled, a BEGIN
\ UNTIL loop will look like this:

\ +- - - - - - -+--+--+--+--+-+-+-+-+
\ | inside-loop |  0BRANCH  |  ptr  |
\ +- ^ - - - - -+--+--+--+--+-+ | +-+
\    \__________________________/

\ BEGIN: save a branch destination for later consumption
\ UNTIL: compile a conditional branch using the destination left on the stack by BEGIN
\ A similar loop variant is BEGIN AGAIN - an infinite loop, unless stopped by EXIT

\ More advanced is the BEGIN WHILE REPEAT loop - run the part between BEGIN and WHILE, and if that
\ leaves TRUE on the stack, run the part between WHILE and REPEAT, then loop back to the beginning
\ to repeat the whole process. This is how it looks compiled:

\      _____________________________________________________
\     /                                                     \
\ +-  V  - - -+--+--+--+--+-+-+-+-+- - - - -+--+--+--+--+-+ | +-+
\ | condition |  0BRANCH  | ptr1  | inside  |  BRANCH   | ptr2  | after
\ +- - - - - -+--+--+--+--+-+ | +-+- - - - -+--+--+--+--+-+-+-+-+  ^
\                             \____________________________________/

\ ---------- CASE STATEMENTS ---------------------------------------------------------------------

\ Many programmers are familiar with `switch` from C-like languages. In Forth, one would write:

\ : SAY-NUMBER
\   CASE
\     1 OF ." ONE" ENDOF
\     2 OF ." TWO" ENDOF
\     3 OF ." THREE" ENDOF
\     ." WE DIDN'T LEARN HOW TO COUNT TO " DUP . ." YET :("
\   ENDCASE CR
\ ;

\ This could obviously be also achieved using IFs, and in fact, that's exactly what happens:

\ CASE
\ test1 OF ... ENDOF           test1 OVER = IF DROP ... ELSE
\ test2 OF ... ENDOF           test2 OVER = IF DROP ... ELSE
\ test3 OF ... ENDOF           test3 OVER = IF DROP ... ELSE
\ default-case                 default-case
\ ENDCASE                      DROP THEN THEN THEN

\ To count how many THENs ENDCASE needs to POSTPONE, CASE pushes a 0 on the stack. Since IF leaves
\ a branch pointer on the stack, the 0 will only be on the top when all IFs have matching THENs.

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

\ Defining parenthesis comments works particularily nicely with CASE. Note how this implementation
\ allows nesting parenthesis by keeping track of the nesting level.

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
: 2DUP ( a b -- a b a b )
  OVER ( a b a )
  OVER ( a b a b )
;

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

: C, ( char -- ) HERE 1 ALLOT ! ;

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

( ---------- COUNTED LOOPS --------------------------------------------------------------------- )

( We can now define DO, ?DO, LOOP, +LOOP and LEAVE. It would be relatively simple if LEAVE did not
  exist, but the plan is: at first, compile LEAVE as BRANCH LEAVE. LOOP (or +LOOP) will then go to
  the beginning of the loop and scan for branches, conditional or not, going to LEAVE, and replace
  them with a proper destination.

  The loop control variables are stored on the return stack, with the counter on top and the limit
  on the bottom.

  DO -> 2>R loop-inside
            ^ the ending branch jumps here
        ^ LOOP starts scanning here

  ?DO -> (?DO) 0BRANCH LEAVE loop-inside
                             ^ the ending branch jumps here
                       ^ LOOP starts scanning here
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

: LEAVE
  POSTPONE BRANCH
  [ LATEST @ >CFA ] LITERAL ,
; IMMEDIATE

: UNLOOP
  POSTPONE 2RDROP
; IMMEDIATE

: DO
  POSTPONE 2>R
  HERE
; IMMEDIATE

: ?DO
  POSTPONE (?DO)
  POSTPONE 0BRANCH
  ['] LEAVE ,
  HERE
; IMMEDIATE

: SOME-LOOP
  POSTPONE 0BRANCH
  DUP ,
  HERE ( loop-beginning loop-end )
  POSTPONE UNLOOP
  SWAP CELL- ( loop-end curr-address )
  BEGIN
    DUP @ ['] LEAVE = IF
      ( loop-end curr-address )
      ( make sure it's preceded by a branch! )
      DUP CELL- @ ( loop-end curr-address word-before )
      DUP ['] BRANCH = SWAP ['] 0BRANCH = OR IF
        2DUP ( loop-end curr-address loop-end curr-address ) !
      THEN
    THEN

    DUP @ CASE
      ['] LIT OF 2 CELLS + ENDOF
      ['] LITSTRING OF CELL+ DUP @ CELL+ + ENDOF
      SWAP CELL+ SWAP
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

HIDE (?DO)
HIDE (LOOP)
HIDE (+LOOP)
HIDE SOME-LOOP

: LSHIFT 0 ?DO DUP + LOOP ;

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

( ---------- VARIABLES ------------------------------------------------------------------------- )

: PUSH-IMM32, $68 C, , ;
: NEXT, $AD C, $FF C, $E0 C, ;
: REL! ( value addr -- ) DUP >R CELL+ - R> ! ;
: REL@ ( addr -- value ) DUP @ CELL+ + ;

: CREATE-BARE      ( name u -- )
  HERE LATEST @ -  ( name u link )
  HERE LATEST !
  DUP $FF AND C,
  8 RSHIFT C,      ( name u )
  DUP C,
  F_LENMASK AND
  0 ?DO DUP C@ C, CHAR+ LOOP
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

: EXECUTE [ HERE 12 + ] LITERAL !
  DROP ( this DROP is overwritten by the previous line )
;

: SPACE BL EMIT ;
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

: COMPILE-ONLY
  STATE @ IF EXIT THEN
  R> CELL- ( address of COMPILE-ONLY xt )
  5 - ( address of CALL DOCOL, xt of the protected word )
  CFA>NAME TYPE ."  is compile only."
  ABORT
;

: IF       COMPILE-ONLY POSTPONE IF       ; IMMEDIATE
: ELSE     COMPILE-ONLY POSTPONE ELSE     ; IMMEDIATE
: THEN     COMPILE-ONLY POSTPONE THEN     ; IMMEDIATE
: CASE     COMPILE-ONLY POSTPONE CASE     ; IMMEDIATE
: ENDCASE  COMPILE-ONLY POSTPONE ENDCASE  ; IMMEDIATE
: OF       COMPILE-ONLY POSTPONE OF       ; IMMEDIATE
: ENDOF    COMPILE-ONLY POSTPONE ENDOF    ; IMMEDIATE
: BEGIN    COMPILE-ONLY POSTPONE BEGIN    ; IMMEDIATE
: AGAIN    COMPILE-ONLY POSTPONE AGAIN    ; IMMEDIATE
: UNTIL    COMPILE-ONLY POSTPONE UNTIL    ; IMMEDIATE
: WHILE    COMPILE-ONLY POSTPONE WHILE    ; IMMEDIATE
: REPEAT   COMPILE-ONLY POSTPONE REPEAT   ; IMMEDIATE
: DO       COMPILE-ONLY POSTPONE DO       ; IMMEDIATE
: ?DO      COMPILE-ONLY POSTPONE ?DO      ; IMMEDIATE
: LOOP     COMPILE-ONLY POSTPONE LOOP     ; IMMEDIATE
: +LOOP    COMPILE-ONLY POSTPONE +LOOP    ; IMMEDIATE
: LEAVE    COMPILE-ONLY POSTPONE LEAVE    ; IMMEDIATE
: UNLOOP   COMPILE-ONLY POSTPONE UNLOOP   ; IMMEDIATE
: POSTPONE COMPILE-ONLY POSTPONE POSTPONE ; IMMEDIATE
: LITERAL  COMPILE-ONLY POSTPONE LITERAL  ; IMMEDIATE
: [']      COMPILE-ONLY POSTPONE [']      ; IMMEDIATE
: [CHAR]   COMPILE-ONLY POSTPONE [CHAR]   ; IMMEDIATE

VARIABLE RECURSE-XT

( RECURSE calls the word that's currently being defined - using the name of the word directly will
  compile a call to the previous definition. This is also an example of how to use COMPILE-ONLY. )
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
  5 -       ( because a CALL is 5 bytes long )
  CFA>NAME TYPE ."  used before being defined with IS"
  ABORT
;

: DEFER WORD CREATE-BARE $E8 C, ['] DEFER-DEFAULT HERE CELL ALLOT REL! ;
: DEFER@ 1+ REL@ ;
: DEFER! $E9 OVER C! 1+ REL! ;

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

." 2K Linux" CR
CONCLUDE" TEST.FRT"
