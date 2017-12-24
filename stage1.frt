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
: NEGATE INVERT 1+ ;

: IMMEDIATE LATEST @ 2 + DUP @ $80 OR SWAP ! ; IMMEDIATE
: [ FALSE STATE ! ; IMMEDIATE
: ] TRUE STATE ! ;

: \ [ HERE @ ] KEY NL = 0BRANCH [ , ] ; IMMEDIATE

\ this should be a comment

: NIP SWAP DROP ;
: TUCK SWAP OVER ;

: 2DROP DROP DROP ;
: 2DUP OVER OVER ;
: 2SWAP >R -ROT R> -ROT ;

: / /MOD NIP ;
: MOD /MOD DROP ;
: <> = INVERT ;
: >= < INVERT ;
: <= > INVERT ;
: 0<> 0= INVERT ;
: 0>= 0< INVERT ;
: 0<= 0> INVERT ;

: HIDDEN 2 + DUP @ $20 XOR SWAP ! ;
