/* 22-JAN-86 C4.
$L-
DECIMAL

PROCEDURE STMTNTS
  ( LPS BRK )
  BEGIN
    (( STMTNT LPS @ # BRK @ ))
    SY@ ENDSYM =
  END
;

PROCEDURE PRIM2 ( PRI )
LOCAL
INTEGER SY1
INTEGER POS
INTEGER STATE
INTEGER MODE
POINTER TYPEHEAD ;
  OP@ DUP SELECTOR = SWAP
  REFSELECTOR = OR
  IF TRUE PRI ! FI
  WHILE
    PRI @ NOT EXIT
    SY@
    CASE
    [ LP ]
      call SY!
      (( PUSHTOKEN ))
      LP SY!
      (( PUSHTOKEN ))
      THISMODE IR LASTID @ . .@
      NULLTYPE =
      IF
        (( FUNCRETURNING INT ))
        THISMODE IR LASTID @
        . .!
        EXTERNVAR THISSTATE IR
        LASTID @ . .!
      FI
      INSYMBOL
      (( PARAMLIST ))
      (( NEXTSYMBOL RP # 24 ))
      FALSE POSIVALUE !
      BREAK
    [ LB ]
      INDEX INDEXOP OPSY!
      (( PUSHTOKEN ))
      NOOP LB OPSY!
      (( PUSHTOKEN ))
      INSYMBOL
      (( EXPRESSION ))
      (( SKIP RB ))
      TRUE POSIVALUE !
      (( PUSHTOKEN ))
      INSYMBOL
      BREAK
    [ TERMOP ]
      OP@ DUP REFSELECTOR =
      SWAP SELECTOR = OR
      IF
        (( PUSHTOKEN ))
        INSYMBOL
        TRUE POSIVALUE !
        SY@ IDENT =
        IF
          (( PREVUNKNOWN ))
          (( ISDEFINED
             STRUCTDICT @ #
             ID # POS @ #
             STATE @ # MODE @ #
             LASTID @ )) NOT
          IF 3 CERROR FI
          (( PUSHTOKEN ))
          INSYMBOL
        ELSE
          KONSTANT SY@ =
          IF
            (( PUSHTOKEN ))
            INSYMBOL
          ELSE
            25 CERROR
          FI
        FI
      ELSE
        OP@ DUP INCR = SWAP
        DECR = OR
        IF
          OP@ INCR =
          IF POSTINC OP!
          ELSE POSTDEC OP! FI
          (( PUSHTOKEN ))
          INSYMBOL
        ELSE
          FALSE PRI !
        FI
      FI
      BREAK
    [ DUP ] FALSE PRI !
    ESAC
  WEND
;

PROCEDURE PRIM
LOCAL
BOOLEAN PRI
INTEGER SY1
INTEGER OP1
INTEGER POS
INTEGER STATE
INTEGER MODE ;
  TRUE PRI !
  SY@
  CASE
  [ IDENT ]
    (( UNKNOWNID ))
    TRUE POSIVALUE !
    (( PUSHTOKEN ))
    INSYMBOL
    BREAK
  [ TYPESYM ]
    (( SIZEOFTYPE IDTYPE ))
    VAL !
    KONSTANT SY!
    (( PUSHTOKEN ))
    INSYMBOL
    BREAK
  [ KONSTANT ]
    VALUE @ VAL !
    (( PUSHTOKEN ))
    INSYMBOL
    BREAK
  [ STRING ]
    (( PUSHTOKEN ))
    INSYMBOL
    BREAK
  [ LP ]
    INSYMBOL
    (( ABSDECLAR ))
    IF
      SY@ SY1 !
      CAST SY!
      (( PUSHTOKEN ))
      SY1 @ SY!
      FALSE PRI !
      (( NEXTSYMBOL RP # 26 ))
      (( EXP ))
    ELSE
      SY@ SY1 !
      LP SY!
      (( PUSHTOKEN ))
      SY1 @ SY!
      (( EXPRESSION ))
      (( PUSHTOKEN ))
      (( NEXTSYMBOL RP # 27 ))
      TRUE POSIVALUE !
      TRUE PRI !
    FI
    BREAK
  [ MULTOP ]
    OP@ TIMES =
    IF
      TRUE POSIVALUE !
      DEREF OP!
      (( PUSHTOKEN ))
      INSYMBOL
      (( EXP ))
      TRUE POSIVALUE !
    ELSE
      69 CERROR
    FI
    BREAK
  [ DUP ]
    FALSE PRI !
    ." PRIM SY:" SY@ .X
    70 CERROR
  ESAC
  (( PRIM2 PRI ))
;


PROCEDURE EXP2
LOCAL
INTEGER SY1
INTEGER SY2
INTEGER OP1
INTEGER OP2 ;
  POSIVALUE @
  IF
    OP@ DUP INCR = SWAP
    DECR = OR
    IF
      OP@ INCR =
      IF POSTINC OP!
      ELSE POSTDEC OP!
      FI
      (( PUSHTOKEN ))
      INSYMBOL
    ELSE
      OP@ BINOP =
      IF
        OP@ OP1 !
        SY@ SY1 !
        INSYMBOL
        SY@ ASSIGNSET =
        IF
          OP1 @ OP!
          (( PUSHTOKEN ))
          INSYMBOL
          (( EXP ))
        ELSE
          SY@ SY2 !
          OP@ OP2 !
          SY1 @ SY!
          OP1 @ OP!
          (( PUSHTOKEN ))
          SY2 @ SY!
          OP2 @ OP!
          (( EXP ))
        FI
      ELSE
        SY@ ASSIGNSET =
        IF
          POSIVALUE @ NOT
          IF 60 CERROR FI
          (( PUSHTOKEN ))
          INSYMBOL
          (( EXP ))
        FI
      FI
    FI
  FI
;

PROCEDURE POSTEXPRESSION
LOCAL
INTEGER SY1 ;
  OP@ DUP DUP REF > SWAP
  IFOP < AND SWAP XOROP = OR
  IF
    (( PUSHTOKEN ))
    INSYMBOL
    (( EXP ))
  ELSE
    OP@ IFOP =
    IF
      (( PUSHTOKEN ))
      LP SY!
      (( PUSHTOKEN ))
      INSYMBOL
      (( EXPRESSION ))
      SY@ SY1 !
      RP SY!
      (( PUSHTOKEN ))
      SY1 @ SY!
      (( PUSHTOKEN ))
      OP@ ELSEOP =
      IF INSYMBOL FI
      SY@ SY1 !
      LP SY!
      (( PUSHTOKEN ))
      SY1 @ SY!
      (( EXP ))
      SY@ SY1 !
      RP SY!
      (( PUSHTOKEN ))
      SY1 @ SY!
    FI
  FI
;

PROCEDURE EXP
LOCAL
INTEGER SY1
INTEGER OP1
INTEGER PRI ;
  0 POSIVALUE !
  OP@
  CASE
  [ minus ]
  [ NOTOP ]
  [ COMPOP ]
  [ ANDOP ]
    OP@
    CASE
    [ ANDOP ]
      REF OP!
      BREAK
    [ COMPOP ]
      ONESCOMP OP!
      BREAK
    [ minus ]
      neg OP!
      BREAK
    ESAC
    (( PUSHTOKEN ))
    INSYMBOL
    (( EXP ))
    BREAK
  [ SIZEOF ]
    (( PUSHTOKEN ))
    INSYMBOL
    SY@ LP =
    IF
      (( PUSHTOKEN ))
      INSYMBOL
      (( ABSDECLAR ))
      IF
        SY@ SY1 !
        KONSTANT SY!
        (( PUSHTOKEN ))
        (( POINTERTO
           TYPENUMBER ))
        EMODE LEVEL @ POINTER *
        LASTNODE
        .@ .!
        SY1 @ SY!
        (( PUSHTOKEN ))
        (( NEXTSYMBOL
           RP # 26 ))
      ELSE
        (( EXPRESSION ))
        (( PUSHTOKEN ))
        (( NEXTSYMBOL
           RP # 27 ))
        1 POSIVALUE !
        1 PRI !
        (( PRIM2 PRI ))
      FI
    ELSE
      (( EXP ))
    FI
    BREAK
  [ INCR ]
  [ DECR ]
    (( PUSHTOKEN ))
    INSYMBOL
    (( PRIM ))
    POSIVALUE @ NOT
    IF
      28 CERROR
    FI
    BREAK
  [ DUP ]
    (( PRIM ))
    (( EXP2 ))
  ESAC
  (( POSTEXPRESSION ))
;

PROCEDURE EXPRESSION
  (( EXP ))
  WHILE
    SY@ COMMA <> EXIT
    comma OP!
    (( PUSHTOKEN ))
    INSYMBOL
    (( EXP ))
  WEND
;

: RETURNSTMTNT
  INSYMBOL
  SY@ SEMI <>
  IF (( EXPRESSION ))
     (( OUTCODE ))
  FI
  (( RETURNCODE BRACKETS @ ))
  (( NEXTSYMBOL SEMI # 38 ))
;

$L+
SMLOAD "C3"
