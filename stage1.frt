: NIP SWAP DROP ;
: TUCK SWAP OVER ;

: 2DROP DROP DROP ;
: 2DUP OVER OVER ;
: 2SWAP >R -ROT R> -ROT ;

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

: IMMEDIATE LATEST @ 2 + DUP @ $80 OR SWAP ! ; IMMEDIATE
: [ FALSE STATE ! ; IMMEDIATE
: ] TRUE STATE ! ;
