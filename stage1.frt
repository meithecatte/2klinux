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

Lorem ipsum dolor sit amet, yada yada yada. Blue fox jumped over the lazy dog, or something along those lines. I like trains. Why is running this in QEMU so inconsistent? The checksum command proves that the IVT isn't overwritten. I've seen similar problems long ago when I was trying to make a full-blown OS... but that was in protected mode, the BIOS didn't even touch anything.
