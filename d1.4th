HEX
$L-
: SPACE 20 ;
: CRLF D .B A .B ;
: .S WHILE DUP @B NOT EXIT
     DUP @B 7F AND
     DUP 20 <
     IF DUP D =
        IF .B
        ELSE
          DROP ' ` .B
        FI
     ELSE .B
     FI
     1 +
   WEND
   DROP
;
DECIMAL

: ASSIGNSET
  DUP TIMES >=
  OVER LTOP < AND
  OVER ANDOP = OR
  OVER OROP = OR
  SWAP XOROP = OR
;
: BINOP DUP ASSIGNSET
  IF DROP 1
  ELSE DUP COMPOP <>
    IF DUP LTOP >=
    SWAP IFOP < AND
    ELSE DROP 0
    FI
  FI
;

: ASSOCIATIVITY
  DUP NOTOP <
  IF LEFTASS
  ELSE DUP TIMES <
  IF RIGHTASS
  ELSE DUP LTOP <
  IF LEFTASS
  ELSE DUP ANDOP <
  IF NOTASS
  ELSE COMPOP ==
  IF RIGHTASS
  ELSE DUP IFOP <
  IF LEFTASS
  ELSE IFOP ==
       OVER ELSEOP = OR
       OVER CONVERT =
  IF NOTASS
  ELSE SIZEOF ==
       OVER ONESCOMP = OR
  IF RIGHTASS
  ELSE LEFTASS
  FI FI FI FI FI FI FI FI
  SWAP DROP
;


DECIMAL

: 3O+! 3 NOVER + ! ;
: @+ @ + ;
: @+! @ + ! ;
: ^. SWAP @ + @ ;

DTOP @ DUP DP !
HEX
29 ,B 0 ,B
2C ,B 0 ,B
2B ,B 0 ,B
1B ,B 1C ,B
31 ,B 1 ,B
27 ,B 1 ,B
30 ,B 5 ,B
FF ,B 0 ,B
DP @ DTOP !
CONSTANT SYPREC

DTOP @ DUP DP !
1E ,B 1 ,B
0 ,B 1 ,B
1 ,B 1 ,B
25 ,B 1 ,B
26 ,B 1 ,B
2 ,B 5 ,B
3 ,B 5 ,B
4 ,B 5 ,B
5 ,B 5 ,B
24 ,B 5 ,B
1D ,B 5 ,B
7 ,B 5 ,B
6 ,B 5 ,B
8 ,B 6 ,B
9 ,B 6 ,B
A ,B 6 ,B
C ,B 8 ,B
D ,B 8 ,B
E ,B A ,B
F ,B A ,B
10 ,B C ,B
11 ,B C ,B
12 ,B C ,B
13 ,B C ,B
14 ,B E ,B
15 ,B E ,B
16 ,B 10 ,B
23 ,B 12 ,B
17 ,B 12 ,B
18 ,B 14 ,B
19 ,B 16 ,B
1A ,B 18 ,B
1B ,B 1A ,B
1C ,B 1A ,B
20 ,B 1E ,B
FF ,B 0 ,B
DP @ DTOP !
CONSTANT OPPREC

: 1BFIND
." 1BFIND " DUP .X ." --- "
  BEGIN DUP @B FF =
    IF DROP DROP 0 1
    ELSE OVER OVER @B =
    IF 1 + @B SWAP DROP
       1 1
    ELSE 2 + 0
    FI FI
  END
OVER OVER .X .X CRLF
;

DECIMAL

FUNCTION PRECEDENCE ( T )
TRACE @ 1 AND
IF
." Precedence(" T @ .X ' ) .B
FI
T @ NOT
IF -2
ELSE T @ BRACKETS
         LEVEL @ 2 + + @
= IF -1
  ELSE T  TOKN ^.
    SYPREC 1BFIND
    NOT IF
         T OPER ^.
         OPPREC 1BFIND
         NOT IF 28
             FI
        FI
  FI
FI
TRACE @ 1 AND
IF
DUP .X CRLF
FI
;

FUNCTION ASSOCIATE ( T )
  T @ NOT
  IF NOTASS
  ELSE T TOKN ^. ASSIGN =
    IF RIGHTASS
    ELSE T OPER ^. ASSOCIATIVITY
    FI
  FI
;


PROCEDURE DISCARD ( T )
  WHILE
    T @ NOT EXIT
    T TOKN ^. STRING =
    IF T STRNG ^. DISPOSE
    FI
    (( DISCARD T 2 ^. ))
    T @ DUP @ T !
    DUP IF DISPOSE FI
  WEND
;


FUNCTION NEWEXPPTR
  LOCAL 2 T ;
TRACE @ 1 AND
IF ." NEWEXPPTR" CRLF FI

  EXPRECORD NEW DUP T !
  0 OVER !
  0 2 3O+!
  4 FLAGS 3O+!
  0 PSREGS 3O+!
  -1 STKTMP 3O+!
  OP @ OPER 3O+!
  IDTYPE @ EMODE 3O+!
  SY @ TOKN 3O+!
  SY @ IDENT =
  IF LASTID @ SENTRY 3O+!
     0 IDOFSET 3O+!
  FI
  SY @ KONSTANT =
  IF VAL @ SVALUE 3O+!
  FI
  SY @ CAST =
  IF TYPENUMBER @ EMODE 3O+!
  FI
  SY @ STRING =
  IF STRINGLIST @ STRNG 3O+!
     STRINGLENG @ STRNGLENG 3O+!
  FI
TRACE @ 1 AND
IF ." END NEWEXPPTR" DUP .X
CRLF FI
;

DECIMAL

PROCEDURE PUSHTOKEN
LOCAL 2 T 2 T1 2 P1 ;
TRACE @ 1 AND
IF ." PUSHTOKEN(" SY @ .X OP @ .X ." )"
CRLF FI

SY @
CASE
  [ LP ]
  [ LB ]
    LEVEL @ 38 <
    IF 2 LEVEL +! FI
    BRACKETS LEVEL @+ 0 OVER !
    0 SWAP 2+!
    LASTNODE LEVEL @+ 0 OVER !
    0 SWAP 2+!
    BREAK
  [ RP ]
  [ RB ]
    LEVEL @
    IF -2 LEVEL +! FI
    BRACKETS LEVEL @+
    DUP @ NOT
    IF DUP 2+@ DUP ROT !
       LASTNODE LEVEL @+!
    ELSE 2+@ DUP
       LASTNODE LEVEL @+ @ RGHT + !
       LASTNODE LEVEL @+!
    FI
    BREAK
  [ IDENT ]
  [ KONSTANT ]
  [ STRING ]
    (( NEWEXPPTR )) T1 !
    LASTNODE LEVEL @+ DUP @ NOT
    IF T1 @ BRACKETS LEVEL @+!
    ELSE T1 @ OVER @ 2+!
    FI
    T1 @ SWAP !
    BREAK
  [ DUP ]
    (( NEWEXPPTR )) T1 !
    (( PRECEDENCE T1 @ )) P1 !
    BRACKETS LEVEL @+ DUP @
    DUP T ! NOT
    IF T1 @ SWAP !
       T1 @ LASTNODE LEVEL @+!
    ELSE DROP
       (( ASSOCIATE T1 @ )) RIGHTASS <>
       IF (( PRECEDENCE T @ ))
          P1 @ <=
          IF T @ T1 @  !
             T1 @ BRACKETS LEVEL @+!
          ELSE WHILE
            (( PRECEDENCE T @ 2+@ ))
            P1 @ > NOT EXIT
            T @ 2+@ T !
            WEND
            T @ 2+@ T1 @  !
            T1 @ T @ 2+!
          FI
          T1 @ LASTNODE LEVEL @+!
       ELSE (( PRECEDENCE T @ ))
          P1 @ <
          IF T @ T1 @  !
             T1 @ BRACKETS LEVEL @+!
          ELSE WHILE
            (( PRECEDENCE T @ 2+@ ))
            P1 @ >= NOT EXIT
            T @ 2+@ T !
            WEND
            T @ RGHT + @ T1 @ !
            T1 @ T @ 2+!
          FI
          T1 @ LASTNODE LEVEL @+!
       FI
    FI
    BREAK
ESAC
TRACE @ 1 AND
IF ." END PUSHTOKEN
" FI
;

$L+

SMLOAD "C1"

