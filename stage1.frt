: NIP SWAP DROP ;
: TUCK SWAP OVER ;

: >R RP@ 4- DUP RP! ! ;
: R> RP@ DUP 4+ RP! @ ;
: RDROP RP@ 4+ RP! ;

: 2DROP DROP DROP ;
: 2DUP OVER OVER ;
: 2SWAP >R -ROT R> -ROT ;

: ?DUP DUP IF DUP THEN ;
: FALSE 0 ;
: TRUE FALSE INVERT ;
: NEGATE INVERT 1+ ;

: / /MOD NIP ;
: MOD /MOD DROP ;
: <> = INVERT ;
: >= < INVERT ;
: <= > INVERT ;
: 0<> 0= INVERT ;
: 0>= 0< INVERT ;
: 0<= 0> INVERT ;

: CELL+ 4 + ;
: CELLS 4 * ;
