\ To test the ANS Forth Core Extension word set

\ This program was written by Gerry Jackson in 2006, with contributions from
\ others where indicated, and is in the public domain - it can be distributed
\ and/or modified in any way but please retain this notice.

\ This program is distributed in the hope that it will be useful,
\ but WITHOUT ANY WARRANTY; without even the implied warranty of
\ MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

\ The tests are not claimed to be comprehensive or correct

\ ------------------------------------------------------------------------------
\ Version 0.13 28 October 2015
\              Replace <FALSE> and <TRUE> with FALSE and TRUE to avoid
\              dependence on Core tests
\              Moved SAVE-INPUT and RESTORE-INPUT tests in a file to filetest.fth
\              Use of 2VARIABLE (from optional wordset) replaced with CREATE.
\              Minor lower to upper case conversions.
\              Calls to COMPARE replaced by S= (in utilities.fth) to avoid use
\              of a word from an optional word set.
\              UNUSED tests revised as UNUSED UNUSED = may return FALSE when an
\              implementation has the data stack sharing unused dataspace.
\              Double number input dependency removed from the HOLDS tests.
\              Minor case sensitivities removed in definition names.
\         0.11 25 April 2015
\              Added tests for PARSE-NAME HOLDS BUFFER:
\              S\" tests added
\              DEFER IS ACTION-OF DEFER! DEFER@ tests added
\              Empty CASE statement test added
\              [COMPILE] tests removed because it is obsolescent in Forth 2012
\         0.10 1 August 2014
\             Added tests contributed by James Bowman for:
\                <> U> 0<> 0> NIP TUCK ROLL PICK 2>R 2R@ 2R>
\                HEX WITHIN UNUSED AGAIN MARKER
\             Added tests for:
\                .R U.R ERASE PAD REFILL SOURCE-ID
\             Removed ABORT from NeverExecuted to enable Win32
\             to continue after failure of RESTORE-INPUT.
\             Removed max-intx which is no longer used.
\         0.7 6 June 2012 Extra CASE test added
\         0.6 1 April 2012 Tests placed in the public domain.
\             SAVE-INPUT & RESTORE-INPUT tests, position
\             of T{ moved so that tests work with ttester.fs
\             CONVERT test deleted - obsolete word removed from Forth 200X
\             IMMEDIATE VALUEs tested
\             RECURSE with :NONAME tested
\             PARSE and .( tested
\             Parsing behaviour of C" added
\         0.5 14 September 2011 Removed the double [ELSE] from the
\             initial SAVE-INPUT & RESTORE-INPUT test
\         0.4 30 November 2009  max-int replaced with max-intx to
\             avoid redefinition warnings.
\         0.3  6 March 2009 { and } replaced with T{ and }T
\                           CONVERT test now independent of cell size
\         0.2  20 April 2007 ANS Forth words changed to upper case
\                            Tests qd3 to qd6 by Reinhold Straub
\         0.1  Oct 2006 First version released
\ -----------------------------------------------------------------------------
\ The tests are based on John Hayes test program for the core word set

\ Words tested in this file are:
\     .( .R 0<> 0> 2>R 2R> 2R@ :NONAME <> ?DO AGAIN C" CASE COMPILE, ENDCASE
\     ENDOF ERASE FALSE HEX MARKER NIP OF PAD PARSE PICK REFILL
\     RESTORE-INPUT ROLL SAVE-INPUT SOURCE-ID TO TRUE TUCK U.R U> UNUSED
\     VALUE WITHIN [COMPILE]

\ Words not tested or partially tested:
\     \ because it has been extensively used already and is, hence, unnecessary
\     REFILL and SOURCE-ID from the user input device which are not possible
\     when testing from a file such as this one
\     UNUSED (partially tested) as the value returned is system dependent
\     Obsolescent words #TIB CONVERT EXPECT QUERY SPAN TIB as they have been
\     removed from the Forth 2012 standard

\ Results from words that output to the user output device have to visually
\ checked for correctness. These are .R U.R .(

\ -----------------------------------------------------------------------------
\ Assumptions & dependencies:
\     - tester.fr (or ttester.fs), errorreport.fth and utilities.fth have been
\       included prior to this file
\     - the Core word set available
\ -----------------------------------------------------------------------------
TESTING Core Extension words

TESTING TRUE FALSE

T{ TRUE  -> 0 INVERT }T
T{ FALSE -> 0 }T

\ -----------------------------------------------------------------------------
TESTING <> U>   (contributed by James Bowman)

T{ 0 0 <> -> FALSE }T
T{ 1 1 <> -> FALSE }T
T{ -1 -1 <> -> FALSE }T
T{ 1 0 <> -> TRUE }T
T{ -1 0 <> -> TRUE }T
T{ 0 1 <> -> TRUE }T
T{ 0 -1 <> -> TRUE }T

T{ 0 1 U> -> FALSE }T
T{ 1 2 U> -> FALSE }T
T{ 0 MID-UINT U> -> FALSE }T
T{ 0 MAX-UINT U> -> FALSE }T
T{ MID-UINT MAX-UINT U> -> FALSE }T
T{ 0 0 U> -> FALSE }T
T{ 1 1 U> -> FALSE }T
T{ 1 0 U> -> TRUE }T
T{ 2 1 U> -> TRUE }T
T{ MID-UINT 0 U> -> TRUE }T
T{ MAX-UINT 0 U> -> TRUE }T
T{ MAX-UINT MID-UINT U> -> TRUE }T

\ -----------------------------------------------------------------------------
TESTING 0<> 0>   (contributed by James Bowman)

T{ 0 0<> -> FALSE }T
T{ 1 0<> -> TRUE }T
T{ 2 0<> -> TRUE }T
T{ -1 0<> -> TRUE }T
T{ MAX-UINT 0<> -> TRUE }T
T{ MIN-INT 0<> -> TRUE }T
T{ MAX-INT 0<> -> TRUE }T

T{ 0 0> -> FALSE }T
T{ -1 0> -> FALSE }T
T{ MIN-INT 0> -> FALSE }T
T{ 1 0> -> TRUE }T
T{ MAX-INT 0> -> TRUE }T

\ -----------------------------------------------------------------------------
TESTING NIP TUCK ROLL PICK   (contributed by James Bowman)

T{ 1 2 NIP -> 2 }T
T{ 1 2 3 NIP -> 1 3 }T

T{ 1 2 TUCK -> 2 1 2 }T
T{ 1 2 3 TUCK -> 1 3 2 3 }T

T{ : RO5 100 200 300 400 500 ; -> }T
T{ RO5 3 ROLL -> 100 300 400 500 200 }T
T{ RO5 2 ROLL -> RO5 ROT }T
T{ RO5 1 ROLL -> RO5 SWAP }T
T{ RO5 0 ROLL -> RO5 }T

T{ RO5 2 PICK -> 100 200 300 400 500 300 }T
T{ RO5 1 PICK -> RO5 OVER }T
T{ RO5 0 PICK -> RO5 DUP }T

\ -----------------------------------------------------------------------------
TESTING 2>R 2R@ 2R>   (contributed by James Bowman)

T{ : RR0 2>R 100 R> R> ; -> }T
T{ 300 400 RR0 -> 100 400 300 }T
T{ 200 300 400 RR0 -> 200 100 400 300 }T

T{ : RR1 2>R 100 2R@ R> R> ; -> }T
T{ 300 400 RR1 -> 100 300 400 400 300 }T
T{ 200 300 400 RR1 -> 200 100 300 400 400 300 }T

T{ : RR2 2>R 100 2R> ; -> }T
T{ 300 400 RR2 -> 100 300 400 }T
T{ 200 300 400 RR2 -> 200 100 300 400 }T

\ -----------------------------------------------------------------------------
TESTING WITHIN   (contributed by James Bowman)

T{ 0 0 0 WITHIN -> FALSE }T
T{ 0 0 MID-UINT WITHIN -> TRUE }T
T{ 0 0 MID-UINT+1 WITHIN -> TRUE }T
T{ 0 0 MAX-UINT WITHIN -> TRUE }T
T{ 0 MID-UINT 0 WITHIN -> FALSE }T
T{ 0 MID-UINT MID-UINT WITHIN -> FALSE }T
T{ 0 MID-UINT MID-UINT+1 WITHIN -> FALSE }T
T{ 0 MID-UINT MAX-UINT WITHIN -> FALSE }T
T{ 0 MID-UINT+1 0 WITHIN -> FALSE }T
T{ 0 MID-UINT+1 MID-UINT WITHIN -> TRUE }T
T{ 0 MID-UINT+1 MID-UINT+1 WITHIN -> FALSE }T
T{ 0 MID-UINT+1 MAX-UINT WITHIN -> FALSE }T
T{ 0 MAX-UINT 0 WITHIN -> FALSE }T
T{ 0 MAX-UINT MID-UINT WITHIN -> TRUE }T
T{ 0 MAX-UINT MID-UINT+1 WITHIN -> TRUE }T
T{ 0 MAX-UINT MAX-UINT WITHIN -> FALSE }T
T{ MID-UINT 0 0 WITHIN -> FALSE }T
T{ MID-UINT 0 MID-UINT WITHIN -> FALSE }T
T{ MID-UINT 0 MID-UINT+1 WITHIN -> TRUE }T
T{ MID-UINT 0 MAX-UINT WITHIN -> TRUE }T
T{ MID-UINT MID-UINT 0 WITHIN -> TRUE }T
T{ MID-UINT MID-UINT MID-UINT WITHIN -> FALSE }T
T{ MID-UINT MID-UINT MID-UINT+1 WITHIN -> TRUE }T
T{ MID-UINT MID-UINT MAX-UINT WITHIN -> TRUE }T
T{ MID-UINT MID-UINT+1 0 WITHIN -> FALSE }T
T{ MID-UINT MID-UINT+1 MID-UINT WITHIN -> FALSE }T
T{ MID-UINT MID-UINT+1 MID-UINT+1 WITHIN -> FALSE }T
T{ MID-UINT MID-UINT+1 MAX-UINT WITHIN -> FALSE }T
T{ MID-UINT MAX-UINT 0 WITHIN -> FALSE }T
T{ MID-UINT MAX-UINT MID-UINT WITHIN -> FALSE }T
T{ MID-UINT MAX-UINT MID-UINT+1 WITHIN -> TRUE }T
T{ MID-UINT MAX-UINT MAX-UINT WITHIN -> FALSE }T
T{ MID-UINT+1 0 0 WITHIN -> FALSE }T
T{ MID-UINT+1 0 MID-UINT WITHIN -> FALSE }T
T{ MID-UINT+1 0 MID-UINT+1 WITHIN -> FALSE }T
T{ MID-UINT+1 0 MAX-UINT WITHIN -> TRUE }T
T{ MID-UINT+1 MID-UINT 0 WITHIN -> TRUE }T
T{ MID-UINT+1 MID-UINT MID-UINT WITHIN -> FALSE }T
T{ MID-UINT+1 MID-UINT MID-UINT+1 WITHIN -> FALSE }T
T{ MID-UINT+1 MID-UINT MAX-UINT WITHIN -> TRUE }T
T{ MID-UINT+1 MID-UINT+1 0 WITHIN -> TRUE }T
T{ MID-UINT+1 MID-UINT+1 MID-UINT WITHIN -> TRUE }T
T{ MID-UINT+1 MID-UINT+1 MID-UINT+1 WITHIN -> FALSE }T
T{ MID-UINT+1 MID-UINT+1 MAX-UINT WITHIN -> TRUE }T
T{ MID-UINT+1 MAX-UINT 0 WITHIN -> FALSE }T
T{ MID-UINT+1 MAX-UINT MID-UINT WITHIN -> FALSE }T
T{ MID-UINT+1 MAX-UINT MID-UINT+1 WITHIN -> FALSE }T
T{ MID-UINT+1 MAX-UINT MAX-UINT WITHIN -> FALSE }T
T{ MAX-UINT 0 0 WITHIN -> FALSE }T
T{ MAX-UINT 0 MID-UINT WITHIN -> FALSE }T
T{ MAX-UINT 0 MID-UINT+1 WITHIN -> FALSE }T
T{ MAX-UINT 0 MAX-UINT WITHIN -> FALSE }T
T{ MAX-UINT MID-UINT 0 WITHIN -> TRUE }T
T{ MAX-UINT MID-UINT MID-UINT WITHIN -> FALSE }T
T{ MAX-UINT MID-UINT MID-UINT+1 WITHIN -> FALSE }T
T{ MAX-UINT MID-UINT MAX-UINT WITHIN -> FALSE }T
T{ MAX-UINT MID-UINT+1 0 WITHIN -> TRUE }T
T{ MAX-UINT MID-UINT+1 MID-UINT WITHIN -> TRUE }T
T{ MAX-UINT MID-UINT+1 MID-UINT+1 WITHIN -> FALSE }T
T{ MAX-UINT MID-UINT+1 MAX-UINT WITHIN -> FALSE }T
T{ MAX-UINT MAX-UINT 0 WITHIN -> TRUE }T
T{ MAX-UINT MAX-UINT MID-UINT WITHIN -> TRUE }T
T{ MAX-UINT MAX-UINT MID-UINT+1 WITHIN -> TRUE }T
T{ MAX-UINT MAX-UINT MAX-UINT WITHIN -> FALSE }T

T{ MIN-INT MIN-INT MIN-INT WITHIN -> FALSE }T
T{ MIN-INT MIN-INT 0 WITHIN -> TRUE }T
T{ MIN-INT MIN-INT 1 WITHIN -> TRUE }T
T{ MIN-INT MIN-INT MAX-INT WITHIN -> TRUE }T
T{ MIN-INT 0 MIN-INT WITHIN -> FALSE }T
T{ MIN-INT 0 0 WITHIN -> FALSE }T
T{ MIN-INT 0 1 WITHIN -> FALSE }T
T{ MIN-INT 0 MAX-INT WITHIN -> FALSE }T
T{ MIN-INT 1 MIN-INT WITHIN -> FALSE }T
T{ MIN-INT 1 0 WITHIN -> TRUE }T
T{ MIN-INT 1 1 WITHIN -> FALSE }T
T{ MIN-INT 1 MAX-INT WITHIN -> FALSE }T
T{ MIN-INT MAX-INT MIN-INT WITHIN -> FALSE }T
T{ MIN-INT MAX-INT 0 WITHIN -> TRUE }T
T{ MIN-INT MAX-INT 1 WITHIN -> TRUE }T
T{ MIN-INT MAX-INT MAX-INT WITHIN -> FALSE }T
T{ 0 MIN-INT MIN-INT WITHIN -> FALSE }T
T{ 0 MIN-INT 0 WITHIN -> FALSE }T
T{ 0 MIN-INT 1 WITHIN -> TRUE }T
T{ 0 MIN-INT MAX-INT WITHIN -> TRUE }T
T{ 0 0 MIN-INT WITHIN -> TRUE }T
T{ 0 0 0 WITHIN -> FALSE }T
T{ 0 0 1 WITHIN -> TRUE }T
T{ 0 0 MAX-INT WITHIN -> TRUE }T
T{ 0 1 MIN-INT WITHIN -> FALSE }T
T{ 0 1 0 WITHIN -> FALSE }T
T{ 0 1 1 WITHIN -> FALSE }T
T{ 0 1 MAX-INT WITHIN -> FALSE }T
T{ 0 MAX-INT MIN-INT WITHIN -> FALSE }T
T{ 0 MAX-INT 0 WITHIN -> FALSE }T
T{ 0 MAX-INT 1 WITHIN -> TRUE }T
T{ 0 MAX-INT MAX-INT WITHIN -> FALSE }T
T{ 1 MIN-INT MIN-INT WITHIN -> FALSE }T
T{ 1 MIN-INT 0 WITHIN -> FALSE }T
T{ 1 MIN-INT 1 WITHIN -> FALSE }T
T{ 1 MIN-INT MAX-INT WITHIN -> TRUE }T
T{ 1 0 MIN-INT WITHIN -> TRUE }T
T{ 1 0 0 WITHIN -> FALSE }T
T{ 1 0 1 WITHIN -> FALSE }T
T{ 1 0 MAX-INT WITHIN -> TRUE }T
T{ 1 1 MIN-INT WITHIN -> TRUE }T
T{ 1 1 0 WITHIN -> TRUE }T
T{ 1 1 1 WITHIN -> FALSE }T
T{ 1 1 MAX-INT WITHIN -> TRUE }T
T{ 1 MAX-INT MIN-INT WITHIN -> FALSE }T
T{ 1 MAX-INT 0 WITHIN -> FALSE }T
T{ 1 MAX-INT 1 WITHIN -> FALSE }T
T{ 1 MAX-INT MAX-INT WITHIN -> FALSE }T
T{ MAX-INT MIN-INT MIN-INT WITHIN -> FALSE }T
T{ MAX-INT MIN-INT 0 WITHIN -> FALSE }T
T{ MAX-INT MIN-INT 1 WITHIN -> FALSE }T
T{ MAX-INT MIN-INT MAX-INT WITHIN -> FALSE }T
T{ MAX-INT 0 MIN-INT WITHIN -> TRUE }T
T{ MAX-INT 0 0 WITHIN -> FALSE }T
T{ MAX-INT 0 1 WITHIN -> FALSE }T
T{ MAX-INT 0 MAX-INT WITHIN -> FALSE }T
T{ MAX-INT 1 MIN-INT WITHIN -> TRUE }T
T{ MAX-INT 1 0 WITHIN -> TRUE }T
T{ MAX-INT 1 1 WITHIN -> FALSE }T
T{ MAX-INT 1 MAX-INT WITHIN -> FALSE }T
T{ MAX-INT MAX-INT MIN-INT WITHIN -> TRUE }T
T{ MAX-INT MAX-INT 0 WITHIN -> TRUE }T
T{ MAX-INT MAX-INT 1 WITHIN -> TRUE }T
T{ MAX-INT MAX-INT MAX-INT WITHIN -> FALSE }T

\ -----------------------------------------------------------------------------
TESTING AGAIN   (contributed by James Bowman)

T{ : AG0 701 BEGIN DUP 7 MOD 0= IF EXIT THEN 1+ AGAIN ; -> }T
T{ AG0 -> 707 }T

\ -----------------------------------------------------------------------------
TESTING ?DO

: QD ?DO I LOOP ;
T{ 789 789 QD -> }T
T{ -9876 -9876 QD -> }T
T{ 5 0 QD -> 0 1 2 3 4 }T

: QD1 ?DO I 10 +LOOP ;
T{ 50 1 QD1 -> 1 11 21 31 41 }T
T{ 50 0 QD1 -> 0 10 20 30 40 }T

: QD2 ?DO I 3 > IF LEAVE ELSE I THEN LOOP ;
T{ 5 -1 QD2 -> -1 0 1 2 3 }T

: QD3 ?DO I 1 +LOOP ;
T{ 4  4 QD3 -> }T
T{ 4  1 QD3 -> 1 2 3 }T
T{ 2 -1 QD3 -> -1 0 1 }T

: QD4 ?DO I -1 +LOOP ;
T{  4 4 QD4 -> }T
T{  1 4 QD4 -> 4 3 2 1 }T
T{ -1 2 QD4 -> 2 1 0 -1 }T

: QD5 ?DO I -10 +LOOP ;
T{   1 50 QD5 -> 50 40 30 20 10 }T
T{   0 50 QD5 -> 50 40 30 20 10 0 }T
T{ -25 10 QD5 -> 10 0 -10 -20 }T

VARIABLE ITERS
VARIABLE INCRMNT

: QD6 ( limit start increment -- )
   INCRMNT !
   0 ITERS !
   ?DO
      1 ITERS +!
      I
      ITERS @  6 = IF LEAVE THEN
      INCRMNT @
   +LOOP ITERS @
;

T{  4  4 -1 QD6 -> 0 }T
T{  1  4 -1 QD6 -> 4 3 2 1 4 }T
T{  4  1 -1 QD6 -> 1 0 -1 -2 -3 -4 6 }T
T{  4  1  0 QD6 -> 1 1 1 1 1 1 6 }T
T{  0  0  0 QD6 -> 0 }T
T{  1  4  0 QD6 -> 4 4 4 4 4 4 6 }T
T{  1  4  1 QD6 -> 4 5 6 7 8 9 6 }T
T{  4  1  1 QD6 -> 1 2 3 3 }T
T{  4  4  1 QD6 -> 0 }T
T{  2 -1 -1 QD6 -> -1 -2 -3 -4 -5 -6 6 }T
T{ -1  2 -1 QD6 -> 2 1 0 -1 4 }T
T{  2 -1  0 QD6 -> -1 -1 -1 -1 -1 -1 6 }T
T{ -1  2  0 QD6 -> 2 2 2 2 2 2 6 }T
T{ -1  2  1 QD6 -> 2 3 4 5 6 7 6 }T
T{  2 -1  1 QD6 -> -1 0 1 3 }T

\ -----------------------------------------------------------------------------
TESTING CASE OF ENDOF ENDCASE

: CS1 CASE 1 OF 111 ENDOF
           2 OF 222 ENDOF
           3 OF 333 ENDOF
           >R 999 R>
      ENDCASE
;

T{ 1 CS1 -> 111 }T
T{ 2 CS1 -> 222 }T
T{ 3 CS1 -> 333 }T
T{ 4 CS1 -> 999 }T

\ Nested CASE's

: CS2 >R CASE -1 OF CASE R@ 1 OF 100 ENDOF
                            2 OF 200 ENDOF
                           >R -300 R>
                    ENDCASE
                 ENDOF
              -2 OF CASE R@ 1 OF -99  ENDOF
                            >R -199 R>
                    ENDCASE
                 ENDOF
                 >R 299 R>
         ENDCASE R> DROP
;

T{ -1 1 CS2 ->  100 }T
T{ -1 2 CS2 ->  200 }T
T{ -1 3 CS2 -> -300 }T
T{ -2 1 CS2 -> -99  }T
T{ -2 2 CS2 -> -199 }T
T{  0 2 CS2 ->  299 }T

\ Boolean short circuiting using CASE

: CS3  ( N1 -- N2 )
   CASE 1- FALSE OF 11 ENDOF
        1- FALSE OF 22 ENDOF
        1- FALSE OF 33 ENDOF
        44 SWAP
   ENDCASE
;

T{ 1 CS3 -> 11 }T
T{ 2 CS3 -> 22 }T
T{ 3 CS3 -> 33 }T
T{ 9 CS3 -> 44 }T

\ Empty CASE statements with/without default

T{ : CS4 CASE ENDCASE ; 1 CS4 -> }T
T{ : CS5 CASE 2 SWAP ENDCASE ; 1 CS5 -> 2 }T
T{ : CS6 CASE 1 OF ENDOF 2 ENDCASE ; 1 CS6 -> }T
T{ : CS7 CASE 3 OF ENDOF 2 ENDCASE ; 1 CS7 -> 1 }T

\ -----------------------------------------------------------------------------
TESTING :NONAME RECURSE

VARIABLE NN1
VARIABLE NN2
:NONAME 1234 ; NN1 !
:NONAME 9876 ; NN2 !
T{ NN1 @ EXECUTE -> 1234 }T
T{ NN2 @ EXECUTE -> 9876 }T

T{ :NONAME ( n -- 0,1,..n ) DUP IF DUP >R 1- RECURSE R> THEN ;
   CONSTANT RN1 -> }T
T{ 0 RN1 EXECUTE -> 0 }T
T{ 4 RN1 EXECUTE -> 0 1 2 3 4 }T

:NONAME  ( n -- n1 )    \ Multiple RECURSEs in one definition
   1- DUP
   CASE 0 OF EXIT ENDOF
        1 OF 11 SWAP RECURSE ENDOF
        2 OF 22 SWAP RECURSE ENDOF
        3 OF 33 SWAP RECURSE ENDOF
        DROP ABS RECURSE EXIT
   ENDCASE
; CONSTANT RN2

T{  1 RN2 EXECUTE -> 0 }T
T{  2 RN2 EXECUTE -> 11 0 }T
T{  4 RN2 EXECUTE -> 33 22 11 0 }T
T{ 25 RN2 EXECUTE -> 33 22 11 0 }T

\ -----------------------------------------------------------------------------
TESTING .R and U.R - has to handle different cell sizes

\ Create some large integers just below/above MAX and Min INTs
MAX-INT 73 79 */ CONSTANT LI1
MIN-INT 71 73 */ CONSTANT LI2

10 CONSTANT LENLI1

: (.R&U.R)  ( u1 u2 -- )  \ u1 <= string length, u2 is required indentation
   TUCK + >R
   LI1 OVER SPACES  . CR R@    LI1 SWAP  .R CR
   LI2 OVER SPACES  . CR R@ 1+ LI2 SWAP  .R CR
   LI1 OVER SPACES U. CR R@    LI1 SWAP U.R CR
   LI2 SWAP SPACES U. CR R>    LI2 SWAP U.R CR
;

: .R&U.R  ( -- )
   CR ." You should see lines duplicated:" CR
   ." indented by 0 spaces" CR 0      0 (.R&U.R) CR
   ." indented by 0 spaces" CR LENLI1 0 (.R&U.R) CR \ Just fits required width
   ." indented by 5 spaces" CR LENLI1 5 (.R&U.R) CR
;

CR CR ." Output from .R and U.R"
T{ .R&U.R -> }T

\ -----------------------------------------------------------------------------
TESTING DEFER DEFER@ DEFER! IS ACTION-OF (Forth 2012)
\ Adapted from the Forth 200X RfD tests

T{ DEFER DEFER1 -> }T
T{ : MY-DEFER DEFER ; -> }T
T{ : IS-DEFER1 IS DEFER1 ; -> }T
T{ : ACTION-DEFER1 ACTION-OF DEFER1 ; -> }T
T{ : DEF! DEFER! ; -> }T
T{ : DEF@ DEFER@ ; -> }T

T{ ' * ' DEFER1 DEFER! -> }T
T{ 2 3 DEFER1 -> 6 }T
T{ ' DEFER1 DEFER@ -> ' * }T
T{ ' DEFER1 DEF@ -> ' * }T
T{ ACTION-OF DEFER1 -> ' * }T
T{ ACTION-DEFER1 -> ' * }T
T{ ' + IS DEFER1 -> }T
T{ 1 2 DEFER1 -> 3 }T
T{ ' DEFER1 DEFER@ -> ' + }T
T{ ' DEFER1 DEF@ -> ' + }T
T{ ACTION-OF DEFER1 -> ' + }T
T{ ACTION-DEFER1 -> ' + }T
T{ ' - IS-DEFER1 -> }T
T{ 1 2 DEFER1 -> -1 }T
T{ ' DEFER1 DEFER@ -> ' - }T
T{ ' DEFER1 DEF@ -> ' - }T
T{ ACTION-OF DEFER1 -> ' - }T
T{ ACTION-DEFER1 -> ' - }T

T{ MY-DEFER DEFER2 -> }T
T{ ' DUP IS DEFER2 -> }T
T{ 1 DEFER2 -> 1 1 }T

FORGET BITSSET?
CONCLUDE" CPP.FRT"
