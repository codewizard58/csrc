$L-
: $R ZERO OUTPUT ! $S ;
: PCM 6 .B ;
: DICSAV FB18 ;
: DICTOP E400 ;
: < - 0> NOT NOT ;
: >= - 0> NOT ;
: > SWAP < ;
: <= SWAP >= ;
: <> - NOT NOT ;
: .@ + @ ;
: .! + ! ;
: .. + + ;
: @@ @ @ ;
: 1+ ONE + ;
: 2+ TWO + ;
: 4+ FOUR + ;
: 1+@ 1+ @ ;
: 1+! 1+ ! ;
: 2+@ 2+ @ ;
: 2+! 2+ ! ;
: 4+@ 4+ @ ;
: 4+! 4+ ! ;
60F9 CONSTANT JUMP
60F6 CONSTANT SKIP
611A CONSTANT IFF
611D CONSTANT IFT
FA22 CONSTANT DHEADS

: TIDY 61C2 DOIT ;

DECIMAL
: :: : DHEADS @ 6 -
     DUP @B 128 OR SWAP !B
;

HEX
:: (- JTTOP @ 63F0 JTTOP ! ;

:: -) JTTOP ! ;

DECIMAL

THTYPE @ ZERO THTYPE !
:: #LIT
   STATE @
   IF LIT LIT ,C FI
   QUERY WORD FIND
   IF @
   ELSE NUMBER
    NOT IF ." ??" ERROR FI
   FI
   STATE @
   IF , FI
;

THTYPE !
HEX
: #NUM
  STATE @
  IF DUP FF00 AND
     IF #LIT LIT ,C ,
     ELSE 6249 ,C ,B
     FI
  FI
;

: CONST ::
  #LIT LIT ,C ROT ,
  #LIT (- #NUM -) ,C
  60F3 ,C
  ZERO STATE ! TIDY
;

:: ' QUERY DROP NEXTCH
     STATE @ IF 6249 ,C ,B FI
;

DECIMAL

:: /* TTLINE ;
:: //* LASTCH 2 + .S TTLINE ;

//* FPLUS

: NOEXIT #LIT ERROR THREE + ;
TWO VARIABLE WHTOP
TWO VARIABLE CSTTYP
: CSCHECK CSTTYP @ <>
  IF $R ." Control!" ERROR
  FI
;

:: WHILE CSTTYP @ WHTOP @
   DP @ ZERO
   SP WHTOP ! ONE CSTTYP !
;

: WERROR SP WHTOP @ <>
IF $R ." While!" ERROR
FI ;

:: WEND ONE CSCHECK (- WERROR -)
   JUMP ,C DP @ ZERO ,
   SWAP
   BEGIN
     DUP IF
           ROT DP @ SWAP !
           ONE -
         FI
     DUP NOT
   END
   DROP !
   WHTOP ! CSTTYP !
;

:: EXIT ONE CSCHECK (- WERROR -)
IFT ,C DP @ #LIT NOEXIT ,
SWAP 1+ SP WHTOP ! ;

DECIMAL

: $= WHILE
       OVER @B OVER @B
       OVER NOT OVER NOT OR EXIT
       OVER OVER = NOT EXIT
       DROP DROP
       1+ SWAP 1+ SWAP
     WEND
     - SWAP DROP SWAP DROP
;

:: =" STATE @
   IF JUMP ,C DP @ DUP ZERO ,
      " STR @ 1+
      DUP DP !
      SWAP ! 2+
      #LIT LIT ,C , #LIT $= ,C
   ELSE
      " DP @ $=
   FI
;

:: IN ZERO #LIT DUP ,C BEGIN
QWF IF @ DOIT
ELSE NUMBER NOT
IF $R ." ?IN " PRINT ERROR FI FI
#LIT LIT ,C ,
#LIT == ,C #LIT SWAP ,C
1+ QUERY @B ' ^ =
END NEXTCH DROP
#LIT DROP ,C
ONE - DUP
IF ZERO DO #LIT OR ,C LOOP
ELSE DROP FI
;

/* FPLUS1


HEX


: UNLINK
  DUP @ IF DUP 2+@
           OVER @ 2+!
        FI
  DUP 2+@ IF DUP @
               OVER 2+@ !
            FI
  DROP
;

: LINK OVER OVER !
       OVER 2+@ OVER 2+!
       DUP ROT 2+!
       DUP 2+@ DUP
       IF ! ELSE DROP DROP FI
;

: LADD OVER OVER 2+!
       OVER @ OVER !
       DUP ROT !
       DUP @ DUP
  IF 2+! ELSE DROP DROP FI
;


TWO VARIABLE ALLOCP

: INITMEM 20 DTOP +!
  DTOP @ ALLOCP !
  ZERO DTOP @ ! ZERO DTOP @ 2+!
  F9FF DTOP @ - DTOP @ 4+!
  -20 DTOP +!
;

: FREE OVER OVER SWAP 4+!
       SWAP ZERO OVER !
       ZERO OVER 2+! SWAP
       ALLOCP @ DUP NOT
       IF DROP DROP ALLOCP !
       ELSE BEGIN
          DUP 4+@ THREE NOVER >=
          IF SWAP DROP
             OVER OVER SWAP (- LINK -)
             ALLOCP @ =
             IF ALLOCP !
             ELSE DROP
             FI
             ONE
          ELSE DUP @ DUP
             IF SWAP DROP ZERO
             ELSE DROP SWAP DROP
                  SWAP (- LADD -) ONE
             FI
          FI
         END
       FI
;

: FREE? ALLOCP
  BEGIN
    @ DUP
    IF DUP .X 3A .B SPC
       DUP @ .X DUP 2+@ .X
       DUP 4+@ .X ZERO CRLF
    ELSE NOT
    FI
  END
;

: .R ALLOCP
  BEGIN
    @ DUP
    IF OVER FOUR - OVER =
       IF SWAP NOT SWAP FI
    ELSE NOT FI
  END DUP
  IF TWO - DUP @ SWAP TWO - SWAP
     OVER + SWAP TAB
  ELSE ." DISPOSED!" CRLF FI
;

: NEW1 ALLOCP @
  WHILE
    DUP NOT EXIT
    OVER OVER 4+@ <= EXIT
    @
  WEND
  DUP
  IF ALLOCP @ ==
     IF DUP @ ALLOCP ! FI
     DUP (- UNLINK -)
     OVER OVER 4+@ SWAP -
     OVER OVER SWAP 4+!
     EIGHT >=
     IF SWAP DROP DUP DUP 4+@
        + SWAP
        DUP 4+@ FREE
     ELSE
        SWAP DROP
     FI
  ELSE SWAP DROP
  FI
;

TWO VARIABLE TEMPPTR

: COMPACT
  ALLOCP @ TEMPPTR !
  ALLOCP @
  IF ALLOCP @ DUP @ ALLOCP !
     (- UNLINK -)
     TEMPPTR @ ZERO OVER !
     ZERO SWAP 2+!
  FI

  WHILE
    ALLOCP @ NOT EXIT
    ALLOCP @ DUP @ ALLOCP !
    DUP (- UNLINK -)

    TEMPPTR @
    BEGIN
      OVER OVER <
      IF OVER OVER SWAP (- LINK -)
         TEMPPTR @ =
         IF TEMPPTR !
         ELSE DROP
         FI ONE
      ELSE
         DUP @
         IF @ ZERO
         ELSE SWAP (- LADD -) ONE
         FI
      FI
    END
  WEND

  TEMPPTR @
  WHILE
    DUP NOT EXIT
    DUP @ OVER DUP 4+@ + =
    IF DUP @ 4+@ OVER 4+ +!
       DUP @ @ OVER !
    ELSE @
    FI
  WEND
  DROP

  ZERO ALLOCP !
  TEMPPTR @
  WHILE
    DUP NOT EXIT
    DUP @ SWAP
    DUP 4+@ FREE
  WEND
  DROP
;

: NEW 0C + FFF8 AND
  (- COMPACT -)
  DUP (- NEW1 -)
  DUP IF OVER OVER + OVER !
         SWAP OVER 2+!
         4+
      ELSE  DROP
        $R ." New! " .X
        ." bytes" ERROR
      FI
  DUP DUP TWO - @ FOUR - ZERO
  DO ZERO OVER !B 1+ LOOP DROP
;

: DISPOSE  FOUR -
  DUP DUP @ OVER - SWAP 2+@ =
  IF DUP 2+@ FREE
  ELSE $R .X ." Dispose!" ERROR
  FI
;

/* FRWD

HEX

: FRWDERROR
  $R ." Forward!" ERROR
;

: FORWARD : DP @ THREE - DP !
  C3 ,B #LIT FRWDERROR ,
  ZERO STATE ! TIDY
;

: ?FRWD
  OVER WORD FIND
  IF @ DUP DUP @B C3 = SWAP
     1+@ #LIT FRWDERROR = AND
     IF DHEADS @ @ SWAP
        1+!
     ELSE DROP
     FI
  ELSE  DROP DROP
  FI
;


: : : ?FRWD ;

$L+
MLOAD "FPLUS2"

