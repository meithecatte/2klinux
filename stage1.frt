\ BASE starts as 16
\ S0 is most commonly a variable, but here, it's a constant
: S0 7800 ;
: R0 1500 ;

\ OFFSET is the offset in the currently loaded sector of a file
: OFFSET 7C18 ;
: LATEST 7C1C ;
: HERE   7C20 ;
: BASE   7C24 ;
: STATE  7C28 ;

: aINCa  40 C, ;
: aINCc  41 C, ;
: aINCd  42 C, ;
: aINCb  43 C, ;
: aINCsp 44 C, ;
: aINCbp 45 C, ;
: aINCsi 46 C, ;
: aINCdi 47 C, ;

: aDECa  48 C, ;
: aDECc  49 C, ;
: aDECd  4A C, ;
: aDECb  4B C, ;
: aDECsp 4C C, ;
: aDECbp 4D C, ;
: aDECsi 4E C, ;
: aDECdi 4F C, ;

: aPUSHa  50 C, ;
: aPUSHc  51 C, ;
: aPUSHd  52 C, ;
: aPUSHb  53 C, ;
: aPUSHsp 54 C, ;
: aPUSHbp 55 C, ;
: aPUSHsi 56 C, ;
: aPUSHdi 57 C, ;

: aPOPa  58 C, ;
: aPOPc  59 C, ;
: aPOPd  5A C, ;
: aPOPb  5B C, ;
: aPOPsp 5C C, ;
: aPOPbp 5D C, ;
: aPOPsi 5E C, ;
: aPOPdi 5F C, ;

\ NEXT is used to end an assembly word
: aLODSD    AD C, ;
: aNEXT aLODSD FF C, 20 C, ;

\ ModR/M bytes assuming r/m, r
: A,A    C0 C, ;
: D,D    D2 C, ;
: A,B    D8 C, ;
: A,SP   E0 C, ;
: [B],A  03 C, ;
: [DI],A 07 C, ;
: [SP],A 04 C, 24 C, ;

\ ModR/M bytes assuming r, r/m
: rA,[A]   00 C, ;
: rA,[DI]  07 C, ;
: rSI,[SI] 36 C, ;

\ common pop combinations to save on typing
: aPOPabc aPOPa aPOPb aPOPc ;
: aPOPba  aPOPb aPOPa ;

\ r - r/m in op2
\ b - bytes
: aSETccAL  0F C,    C, C0 C, ;
: aMOVZXrb  0F C, B6 C, ;
: aADDi8di  83 C, C7 C, ;
: aSUBi8di  83 C, EF C, ;
: aTEST     85 C, ;
: aMOVr     8B C, ;
: aMOVb     88 C, ;
: aADDr     03 C, ;
: aMOV      89 C, ;
: aXOR      31 C, ;
: aCMP      39 C, ;
: aJZ       74 C, ;

\ stack manipulation
:A DROP aPOPa
	aNEXT
:A DUP  aPOPa
	aPUSHa
	aPUSHa
	aNEXT
:A SWAP aPOPba
	aPUSHb
	aPUSHa
	aNEXT
:A ROT  aPOPabc
	aPUSHb
	aPUSHa
	aPUSHc
	aNEXT
:A -ROT aPOPabc
	aPUSHa
	aPUSHc
	aPUSHb
	aNEXT
:A OVER aPOPba
	aPUSHa
	aPUSHb
	aPUSHa
	aNEXT
:A SP@  aMOV A,SP
	aPUSHa
	aNEXT
:A SP!  aPOPsp
	aNEXT
: NIP SWAP DROP ;
: TUCK SWAP OVER ;
: 2DROP DROP DROP ;
: 2DUP OVER OVER ;

\ return stack manipulation
:A RP@  aPUSHdi
	aNEXT
:A RP!  aPOPdi
	aNEXT
:A RDROP
	aADDi8di 04 C,
	aNEXT
:A R@   aMOVr rA,[DI]
	aPUSHa
	aNEXT
:A R>   aMOVr rA,[DI]
	aADDi8di 04 C,
	aPUSHa
	aNEXT
:A >R   aPOPa
	aSUBi8di 04 C,
	aMOV [DI],A
	aNEXT

\ memory access
:A @    aPOPa
	aMOVr rA,[A]
	aPUSHa
	aNEXT
:A !    aPOPba
	aMOV [B],A
	aNEXT
:A C@   aPOPa
	aMOVZXrb rA,[A]
	aPUSHa
	aNEXT
:A C!   aPOPba
	aMOVb [B],A
	aNEXT

\ the five operations with the same boilerplate
: doOP  aPOPa
	C, [SP],A
	aNEXT ;
:A +   01 doOP
:A -   29 doOP
:A OR  09 doOP
:A AND 21 doOP
:A XOR 31 doOP

\ unary operations
:A INVERT
	FF C, 14 C, 24 C,
	aNEXT
:A NEGATE
	FF C, 1C C, 24 C,
	aNEXT
:A 1+   aPOPa
	aINCa
	aPUSHa
	aNEXT
:A 1-   aPOPa
	aDECa
	aPUSHa
	aNEXT

\ multiply and divide
:A *    aPOPba
	F7 C, EB C,
	aPUSHa
	aNEXT
:A /MOD aXOR D,D
	aPOPba
	F7 C, FB C,
	aPUSHd
	aPUSHa
	aNEXT
: / /MOD NIP ;
: MOD /MOD DROP ;

\ comparisons
: endCC 0F C, C, C0 C, \ setcc al
	aMOVZXb A,A
	aPUSHa
	aNEXT ;
: doCC  aPOPba
	aCMP A,B
	DUP endCC ;
: zeroCC
	aPOPba
	aTEST A,B
	endCC ;

:A =   94 doCC :A 0=  zeroCC
:A <>  95 doCC :A 0<> zeroCC
:A <   9C doCC :A 0<  zeroCC
:A <=  9E doCC :A 0<= zeroCC
:A >   9F doCC :A 0>  zeroCC
:A >=  9D doCC :A 0>= zeroCC
:A U<  92 doCC
:A U<= 96 doCC
:A U>  97 doCC
:A U>= 93 doCC

: aCALL E8 C, HERE @ - CELL+ , ;
: aCLRM !!PP!!1 aCALL DUP FF AND C, 8 RSHIFT C, ;

:A EMIT aPOPa
	!!PP!!2 aCLRM
	aNEXT
:A KEY  !!PP!!3 aCALL
	aMOVZXb A,A
	aPUSHa
	aNEXT
:A WORD !!PP!!4
	aPUSHd
	aPUSHc
	aNEXT
:A NUMBER
	aPOPc
	aPOPd
	!!PP!!5
	aPUSHa
	aPUSHc
	aNEXT

\ basic flow control
:A BRANCH
	aADDr rSI,[SI]
	aNEXT
:A 0BRANCH
	aPOPa
	aTEST A,A
	aJZ 4 C,
\ the false case - skip the offset
	aLODSD \ 1 byte
	aNEXT  \ 3 bytes
\ the true case - add the offset
	aADDr rSI,[SI]
	aNEXT

: DECIMAL 10 BASE !
: HEX     16 BASE !
: CELL+ 4 + ;
: CELLS 4 * ;

: TRUE  1 ;
: FALSE 0 ;
: NOT   0= ;

: PICK 1+ CELLS SP@ + @ ;

: -! DUP @ ROT - SWAP ! ;
: +! SWAP NEGATE SWAP ;
: C@C! OVER C@ OVER C! 1+ SWAP 1+ SWAP ;
: >CFA 4+ DUP C@ 31 AND + 1+ ;
: >DFA >CFA 4+ ;
