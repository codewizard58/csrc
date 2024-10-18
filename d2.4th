/* D2 15-2-86
DECIMAL
$L-
FORWARD POINTERTO

: @+@ SWAP @ + @ ;

PROCEDURE PRTTREE ( T CNT )
WHILE
  T @ NOT EXIT
  T @ .X CNT @ 0 DO SPACE .B LOOP
  T TOKN @+@ .X
  T OPER @+@ .X
  T TOKN @+@ KONSTANT ==
  IF DROP T SVALUE @+@ .X
  ELSE DROP
  FI
  T OPER @+@ PLUS ==
  IF ' + .B
  ELSE TIMES ==
  IF ' * .B
  FI FI
  DROP
  CRLF
  (( PRTTREE T LFT @+@ #
     CNT @ 1 + ))
  T RGHT @+@ T !
  1 CNT +!
WEND
;

PROCEDURE PRINTLITERAL ( LREC )
LREC @
DUP TOKN + @
CASE
  [ KONSTANT ]
    DUP EMODE + @ TYPERECORD *
    TYPETAB + BASICTYPE + @
    CASE
      [ SHORT ]
      [ KAR ]
         ."        DB "
         DUP SVALUE + @
         ." 0" .X ." H" CRLF
         BREAK
      [ INT ]
      [ UNSIGNED ]
         ."         DW "
         DUP SVALUE + @
         ." 0" .X ." H" CRLF
         BREAK
      [ DUP ]
         ." OTHERWISE..." CRLF
    ESAC
    BREAK
  [ IDENT ]
    ."         DW "
    LID OVER + .S ." +0"
    IDOFSET OVER + @ .X ." H" CRLF
    BREAK
  [ DUP ]
    DUP OPER + @ REF =
    IF ."         DW LIT.LIT+0"
       SVALUE OVER + @ .X CRLF
    FI
    BREAK
  ESAC
  DROP
;


PROCEDURE PRINTDATA
." Printdata
"
WHILE
  DATFILE @ NOT EXIT
  DATFILE @ DUP  @
  DATFILE !
  (( PRINTLITERAL DUP ))
  DISPOSE
WEND
;

PROCEDURE PRINTSTATIC
." Printstatic
"
."          DS 0
"
WHILE
  STATFIL @ NOT EXIT
  STATFIL @ DUP  @
  STATFIL !
  (( PRINTLITERAL DUP ))
  DISPOSE
WEND
;


FUNCTION BASICSIZE ( MDE )
MDE @ KAR ==
    SWAP SHORT = OR
    IF 1
ELSE MDE @ LONG =
    IF 4
ELSE 2 FI
FI
;

FUNCTION NEWEXPNODE
  ( OPR TOK )
EXPRECORD NEW
0 OVER 2+!
0 OVER  !
OPR @ OVER OPER + !
TOK @ OVER OPER + !
INT OVER EMODE + !
TOK @ IDENT =
IF 0 OVER IDOFSET + !
   0 OVER SENTRY + !
FI
;


FUNCTION INDIRECT ( N )
N @ NOT
IF ." 0 in Indirect" CRLF
   INT
ELSE N TYPERECORD * TYPETAB +
DUP BASICTYPE @
CASE
  [ pointer ] TYPEPOINTER + @
    BREAK
  [ ARRAYTYPE ] TYPENO + @
    BREAK
  [ struct ] DROP N @
    BREAK
  [ UNION ] DROP N @
    BREAK
  [ DUP ] DROP N @
    BREAK
ESAC
FI
;

PROCEDURE DATAHED ( NAME )
." GLOBAL "
NAME @ .S ." : DS 0" CRLF
;

FUNCTION LEFTOF ( T )
T @
IF T @@ ELSE 0 FI
;

FUNCTION SCALAR ( TY )
  TY @ TYPERECORD * TYPETREE +
  2+@ SHORT ==
  OVER KAR = OR
  OVER UNSIGNED = OR
  OVER INT = OR
  OVER LONG = OR
  OVER REEL = OR
  OVER DOUBLE = OR
  OVER POINTER = OR
  SWAP FIELD = OR
;

FUNCTION TYPECALL ( T )
." TYPECALL
"
INT
;

/* VAR T
PROCEDURE TYPEINDEX ( T )
LOCAL 2 N 2 T1 2 T2 ;
." TYPEINDEX
"
;


FUNCTION TYPETREE ( T )
LOCAL 2 T1
      2 TYP
      2 N
      2 N1 ;
INT TYP !
INT N !
T @@
IF T @@ DUP 6+@
  CASE
    [ IDENT ]
      (( POINTERTO DUP SENTRY .@ THISMODE .@ ))
      DUP DUP TYP ! N !
      TYPERECORD * TYPETAB + 2+@
      struct ==
      OVER ARRAYTYPE = OR
      SWAP UNION = OR
      IF (( NEWEXPNODE REF # UNARY ))
         DUP T1 !
         N @ OVER 8+!
         T @@ OVER 2+!
         N @ T @@ 8+!
         T @!
      FI
      BREAK

    [ KONSTANT ]
      DUP 8+@ DUP TYP ! N !
      BREAK
    [ STRING ]
      (( POINTERTO KAR ))
      DUP TYP ! N !
      BREAK
    [ call ]
      (( TYPECALL DUP ))
      DUP TYP ! N !
      BREAK
    [ INDEXOP ]
      (( TYPEINDEX T @ ))
      DROP T @@
      DUP 8+@ TYP !
      (( SCALAR (( INDIRECT TYP @ )) ))
      IF (( NEWEXPNODE DEREF # UNARY ))
         DUP T1 !
         TYP @ OVER 8+!
         T @@ OVER 2+!
         T @!
      FI
      TYP @ N !
      BREAK
    [ ASSIGN ]
      ." ASSIGN IN TYPETREE
"
      BREAK
    [ DUP ]
      ." OTHERWISE IN TYPETREE
"
      BREAK
  ESAC
  DROP
FI
T @@
IF TYP @ REEL =
  IF DOUBLE TYP !
  FI
  TYP @ T @@ 8+!
FI
N @
." Typetree" CRLF
(( PRTTREE T @@ # 1 ))
CRLF
;

PROCEDURE PROCHEADER ( LID )
." FUNCTION "
LID @ NAME + .S CRLF
;


PROCEDURE jump ( LAB )
 SP 8 + SP 4 + TAB
 ." JUMP(" LAB @ .X ." )" CRLF
;

PROCEDURE OUTJUMP
  ( LAB COND )
  ." OUTJUMP "
  COND @ .X LAB @ .X
  CRLF
;

PROCEDURE OUTCODE
  ." OUT CODE" CRLF
  LEVEL 0!
  LASTNODE 0!
  0 BRACKETS 2+!
;

PROCEDURE OUTEXPRESSION
 ." OUTEXPRESSION" CRLF
 (( OUTCODE ))
 (( PRTTREE BRACKETS @ #
    1 ))
 ." -----" CRLF
 (( DISCARD BRACKETS @ ))
 BRACKETS 0!
 LEVEL 0!
;
PROCEDURE LABELIT ( LAB )
  ." LABEL(" LAB @ .X ." )" CRLF
 ;
PROCEDURE STACKSET
  ." STACKSET" CRLF
;


PROCEDURE RETURNCODE
  ( T )
  ." RETURN CODE" T @ .X CRLF
;

PROCEDURE EXITCODE
." Exitcode" CRLF
;

$L+
SMLOAD "C2"
