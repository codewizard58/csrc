/* C3 2-FEB-86
DECIMAL

PROCEDURE COMPOUNDST
  ( LPS BRK )
  (( NEXTSYMBOL BEGINSYM # 53 ))
  (( NEWSYMTAB CURRENT ))
  (( NEWSYMTAB STRUCTDICT ))
  TYPENUMBER 0!
  AUTOVAR SC !
  WHILE
    (( NAMEOFTYPE TYPENUMBER ))
    (( NAMEOFSC )) OR NOT EXIT
    (( DECLTIN ))
    TYPENUMBER 0!
    AUTOVAR SC !
  WEND
  (( STACKSET ))
  (( STMTNTS LPS @ #
     BRK @ ))
  SY@ ENDSYM =
  IF INSYMBOL
  ELSE WHILE
         SY@ ENDSYM = EXIT
         INSYMBOL
       WEND
  FI
  CURRENT @ LASTLEVEL .@
    CURRENT !
  STRUCTDICT @ LASTLEVEL .@
    STRUCTDICT !
  (( DISSYMTAB STRUCTDICT @@ ))
  0 STRUCTDICT @!
;

PROCEDURE IFSTMTNT
  ( LPS BRK )
  INSYMBOL
  (( NEXTSYMBOL LP # 29 ))
  (( GIVELABEL ))
  (( EXPRESSION ))
  (( OUTJUMP DUP # 1 ))
  (( NEXTSYMBOL RP # 30 ))
  (( STMTNT LPS @ #
     BRK @ ))
  SY@ ELSESYM =
  IF
    (( GIVELABEL ))
    (( jump DUP ))
    (( LABELIT OVER ))
    INSYMBOL
    (( STMTNT LPS @ #
       BRK @ ))
    (( LABELIT DUP ))
    (( STACKSET ))
  ELSE
    (( LABELIT DUP ))
    (( STACKSET ))
  FI
;

PROCEDURE WHILESTMTNT
LOCAL
INTEGER STARTWHILE
INTEGER ENDWHILE ;
  INSYMBOL
  (( GIVELABEL )) STARTWHILE !
  (( GIVELABEL )) ENDWHILE !
  (( NEXTSYMBOL LP # 0 ))
  (( LABELIT STARTWHILE @ ))
  (( STACKSET ))
  (( EXPRESSION ))
  (( OUTJUMP
     ENDWHILE @ # TRUE ))
  (( NEXTSYMBOL RP # 31 ))
  (( STMTNT
     STARTWHILE @ #
     ENDWHILE @ ))
  (( JUMP STARTWHILE @ ))
  (( LABELIT ENDWHILE @ ))
  (( STACKSET ))
;

PROCEDURE FORSTMTNT
LOCAL
INTEGER STARTTEST
INTEGER STARTSTAMT
INTEGER STARTINC
INTEGER ENDSTAMT ;
  INSYMBOL
  (( NEXTSYMBOL LP # 32 ))
  SY@ SEMI <>
  IF
    (( EXPRESSION ))
    (( OUTEXPRESSION ))
  FI
  (( NEXTSYMBOL SEMI # 33 ))
  (( GIVELABEL )) STARTTEST !
  (( LABELIT STARTTEST @ ))
  (( STACKSET ))
  (( GIVELABEL )) STARTSTAMT !
  (( GIVELABEL )) ENDSTAMT !
  SY@ SEMI <>
  IF
    (( EXPRESSION ))
    (( OUTJUMP
       ENDSTAMT @ # TRUE ))
  FI
  (( NEXTSYMBOL SEMI # 34 ))
  SY@ RP =
  IF
    STARTTEST @ STARTINC !
  ELSE
    (( JUMP STARTSTAMT @ ))
    (( GIVELABEL )) STARTINC !
    (( STACKSET ))
    (( EXPRESSION ))
    (( OUTEXPRESSION ))
    (( JUMP STARTTEST @ ))
  FI
  (( NEXTSYMBOL RP # 35 ))
  (( STMTNT
     STARTINC @ # ENDSTAMT @ ))
  (( JUMP STARTINC @ ))
  (( LABELIT ENDSTAMT @ ))
  (( STACKSET ))
;

PROCEDURE DOSTMTNT
LOCAL
INTEGER WHILELAB
INTEGER STARTLAB
INTEGER ENDLAB ;
  INSYMBOL
  (( GIVELABEL )) STARTLAB !
  (( LABELIT STARTLAB @ ))
  (( GIVELABEL )) WHILELAB !
  (( GIVELABEL )) ENDLAB !
  (( STMTNT
     WHILELAB @ # ENDLAB @ ))
  (( LABELIT WHILELAB @ ))
  (( STACKSET ))
  (( NEXTSYMBOL WHILESYM # 36 ))
  (( NEXTSYMBOL LP # 36 ))
  (( EXPRESSION ))
  (( OUTJUMP
     STARTLAB @ # FALSE ))
  (( NEXTSYMBOL RP # 37 ))
  (( LABELIT ENDLAB @ ))
  (( STACKSET ))
;

PROCEDURE CORDSTMTNT
( SWIDLAB SWIDEND DEFLAB TID
  POS state MODE LOOPSTART )
LOCAL
INTEGER TOP
INTEGER TSY
INTEGER JLAB ;
SY@ DUP DUP
CASESYM = SWAP
IDENT = OR SWAP
DEFAULT = OR
IF
  SY@ DEFAULT =
  IF
    (( LABELIT DEFLAB @ ))
    0 DEFLAB !
    INSYMBOL
    OP@ ELSEOP =
    IF INSYMBOL FI
  ELSE
    SY@ IDENT =
    IF
      FALSE POSIVALUE !
      ID TID @ ALFA STRNCPY
      INSYMBOL
      (( PREVUNKNOWN ))
      OP@ ELSEOP <>
      IF
        (( ISDEFINED
           CURRENT @ # TID @ #
           POS @ # state @ #
           MODE @ #
           LASTID @ )) NOT
        IF
          SY@ LP =
          IF
            (( PUTINSYMBOLTABLE
               CURRENT #
               TID @ #
               EXTERNVAR #
               (( FUNCRETURNING
                  INT )) ))
            LASTID !
            (( FUNCRETURNING
               INT )) MODE !
          ELSE
            (( PUTINSYMBOLTABLE
               CURRENT #
               TID @ #
               AUTOVAR # INT ))
            LASTID !
          FI
        FI
        SY@ TSY !
        OP@ TOP !
        NOOP IDENT OPSY!
        MODE @ TYPENUMBER !
        (( PUSHTOKEN ))
        TOP @ TSY @ OPSY!
        TRUE POSIVALUE !
        (( PRIM2 TRUE ))
        (( EXP2 ))
        (( POSTEXPRESSION ))
        WHILE
          SY@ COMMA <> EXIT
          comma OP!
          (( PUSHTOKEN ))
          INSYMBOL
          (( EXP ))
        WEND
        (( OUTEXPRESSION ))
      ELSE
        (( ISDEFINED
           GLOBALSYMBOL @@ #
           TID @ #
           POS @ # state @ #
           MODE @ #
           LASTID @ )) NOT
        IF
          (( PUTINSYMBOLTABLE
             GLOBALSYMBOL @ #
             TID @ # LABELL #
             NULLTYPE ))
          LASTID !
          (( GIVELABEL ))
          OFFSET IR LASTID
          @ . .!
        FI
        (( LABELIT OFFSET IR
           LASTID @ . .@ ))
        (( STACKSET ))
        INSYMBOL
      FI
    ELSE
      (( GIVELABEL )) JLAB !
      (( JUMP JLAB @ ))
      (( LABELIT SWIDLAB @ ))
      WHILE
        SY@ CASESYM <> EXIT
        INSYMBOL
        (( EXPRESSION ))
        OP@ TOP !
        SY@ TSY !
        EQOP EQUALITY OPSY!
        (( PUSHTOKEN ))
        TOP @ TSY @ OPSY!
        OP@ ELSEOP =
        IF INSYMBOL FI
        SY@ CASESYM <>
        IF (( OUTJUMP SWIDLAB @
              # TRUE ))
        ELSE (( OUTJUMP JLAB @ #
             FALSE ))
        FI
        (( LABELIT JLAB @ ))
      WEND
      (( CORDSTMTNT SWIDLAB @ #
         SWIDEND @ # DEFLAB @
         TID @ # POS @ #
         state @ # MODE @ #
         LOOPSTART @ ))
    FI
  FI
ELSE
  (( STMTNT LOOPSTART @ #
     SWIDEND @ ))
FI
;

PROCEDURE CASESTMTNT
( SWIDLAB SWIDEND DEFLAB TID
  POS state MODE LOOPSTART )
LOCAL INTEGER TLAB ;
DEFLAB @ TLAB !
SY@ BEGINSYM =
IF
  INSYMBOL
  (( NEWSYMTAB CURRENT ))
  (( NEWSYMTAB STRUCTDICT ))
  AUTOVAR SC !
  NULLTYPE TYPENUMBER !
  WHILE
    WHILE
    (( NAMEOFTYPE
       TYPENUMBER ))
    (( NAMEOFSC )) OR NOT EXIT
    WEND
    SC @ TYPEVAR =
    IF (( DECLTOR TRUE #
          STRUCTDICT @ ))
    ELSE (( DECLTOR TRUE #
            CURRENT @ ))
    FI
  WEND
  WHILE
    SY@ ENDSYM = EXIT
    (( CORDSTMTNT SWIDLAB @ #
       SWIDEND @ # TLAB @ #
       TID @ # POS @ # state @
       # MODE @ #
       LOOPSTART @ ))
  WEND
  LASTLEVEL CURRENT @ .@
  CURRENT !
  LASTLEVEL STRUCTDICT @ .@
  STRUCTDICT !
  (( DISSYMTAB
     CURRENT @@ ))
  (( DISSYMTAB
     STRUCTDICT @@ ))
  NIL CURRENT @!
  NIL STRUCTDICT @!
  INSYMBOL
ELSE
  (( CORDSTMTNT SWIDLAB @ #
     SWIDEND @ # TLAB @ #
     TID @ # POS @ # state @ #
     MODE @ # LOOPSTART @ ))
FI
(( JUMP SWIDEND @ ))
(( LABELIT SWIDLAB @ ))
TLAB @ NOT
IF (( JUMP DEFLAB @ )) FI
;

2 VARIABLE SWIDPTR

PROCEDURE SWITCHSTMTNT
LOCAL 2 SWIDLAB 2 SWIDEND
      2 DEFLAB ALFA TID ;
SY@ SWITCH =
IF INSYMBOL
   (( NEXTSYMBOL LP # 41 ))
   TREERECORD NEW DUP SWIDPTR !
   DUP 0!
   DUP 2+!
   SPACE OVER NAME .!
   AUTOVAR OVER THISSTATE .!
   CURRENT @ SWITCHLEV + @ OVER OFFSET .!
   INT OVER THISMODE .!
   LASTID !
   ID TID ALFA STRNCPY
   SY@ OP@
   IDENT SY !
   NOOP OP !
   (( PUSHTOKEN ))
   ASSIGN SY !
   (( PUSHTOKEN ))
   TID ID ALFA STRNCPY
   OP !
   SY !
   (( EXPRESSION ))
   (( OUTEXPRESSION ))
   (( NEXTSYMBOL RP # 42 ))
   (( GIVELABEL )) DEFLAB !
   (( GIVELABEL )) SWIDEND !
   (( GIVELABEL )) SWIDLAB !
   (( jump SWIDLAB @ ))
   (( CASESTMTNT SWIDLAB @ #
        SWIDEND @ # DEFLAB @ ))
   (( LABELIT SWIDEND @ ))
   SWIDPTR  @ DISPOSE
FI
;

PROCEDURE STMTNT ( LPS
                   BRK )
LOCAL
INTEGER POS
INTEGER state
INTEGER MODE
ALFA TID
2 TSY
2 TOP ;
  NIL SWIDPTR !
  FALSE POSIVALUE !
  NIL LASTID !
  SY@
  CASE
    [ BEGINSYM ]
      (( COMPOUNDST
         LPS @ #
         BRK @ ))
      BREAK
    [ IFSYM ]
      (( IFSTMTNT LPS @ #
         BRK @ ))
      BREAK
    [ WHILESYM ]
      (( WHILESTMTNT ))
      BREAK
    [ DOSYM ]
      (( DOSTMTNT ))
      (( NEXTSYMBOL
         SEMI # 45 ))
      BREAK
    [ forsym ]
      (( FORSTMTNT ))
      BREAK
    [ break ]
      INSYMBOL
      BRK @
      IF (( jump BRK @ ))
      ELSE
        44 CERROR
        (( NEXTSYMBOL
           SEMI # 45 ))
      FI
      BREAK
    [ continue ]
      INSYMBOL
      LPS @
      IF (( jump LPS @ ))
      ELSE
        46 CERROR
        (( NEXTSYMBOL
           SEMI # 47 ))
      FI
      BREAK

    [ GOTOSYM ]
      INSYMBOL
      IDENT SY@ <>
      IF 48 CERROR
      ELSE
        (( ISDEFINED
           CURRENT @ # ID #
           POS # state # MODE #
           LASTID )) NOT
        IF
          (( PUTINSYMBOLTABLE
             GLOBALSYMBOL @
             # ID #
             LABELL #
             NULLTYPE ))
          LASTID !
          (( GIVELABEL ))
          OFFSET IR LASTID
          @ . .!
        FI
        (( jump
           OFFSET IR LASTID @
           . .@ ))
        INSYMBOL
      FI
      (( NEXTSYMBOL
         SEMI # 50 ))
      BREAK
    [ SWITCH ]
      (( SWITCHSTMTNT ))
      BREAK
    [ RETURN ]
      (( RETURNSTMTNT ))
      BREAK

    [ IDENT ]
      (( PREVUNKNOWN ))
      FALSE POSIVALUE !
      ID TID ALFA STRNCPY
      INSYMBOL
      ELSEOP OP@ <>
      IF
        (( ISDEFINED
           CURRENT @ # TID #
           POS @ # state @ #
           MODE @ #
           LASTID @ )) NOT
        IF
          SY@ LP =
          IF
            (( PUTINSYMBOLTABLE
               CURRENT #
               TID #
               EXTERNVAR #
               (( FUNCRETURNING
                  INT )) ))
            LASTID !
            (( FUNCRETURNING
               INT ))
            MODE !
          ELSE
            (( PUTINSYMBOLTABLE
               CURRENT # TID #
               EXTERNVAR # INT
            ))
            LASTID !
            TRUE POSIVALUE !
            INT MODE !
            1 WARN
          FI
        FI
        SY@ TSY !
        OP@ TOP !
        NOOP IDENT OPSY!
        MODE @ TYPENUMBER !
        (( PUSHTOKEN ))
        TOP @ TSY @ OPSY!
        SY@ IDENT =
        SY@ KONSTANT =
        SY@ STRING =
        SY@ RP =
        OR OR OR
        IF 50 CERROR FI
        TRUE POSIVALUE !
        (( PRIM2 1 ))
        (( EXP2 ))
        (( POSTEXPRESSION ))
        WHILE
          SY@ COMMA <> EXIT
          comma OP!
          (( PUSHTOKEN ))
          INSYMBOL
          (( EXP ))
        WEND
        (( OUTEXPRESSION ))
      ELSE
        (( ISDEFINED
           GLOBALSYMBOL @@ #
           TID # POS @ #
           state @ #
           MODE @ # LASTID @ ))
        NOT
        IF
          (( PUTINSYMBOLTABLE
             GLOBALSYMBOL @ #
             TID # LABELL #
             NULLTYPE ))
          LASTID !
          (( GIVELABEL ))
          OFFSET IR LASTID
          @ . .!
        FI
        (( LABELIT
           OFFSET IR LASTID @
           . .@ ))
        (( STACKSET ))
        INSYMBOL
        (( STMTNT
           LPS @ #
           BRK @ ))
      FI
      BREAK
    [ KONSTANT ]
      TRUE POSIVALUE !
      VALUE VAL !
      (( PUSHTOKEN ))
      INSYMBOL
      TRUE POSIVALUE !
      (( PRIM2 1 ))
      (( EXP2 ))
      (( POSTEXPRESSION ))
      WHILE
        SY@ COMMA <> EXIT
        comma OP!
        (( PUSHTOKEN ))
        INSYMBOL
        (( EXP2 ))
      WEND
      (( OUTEXPRESSION ))
      BREAK
    [ TERMOP ]
    [ ADDOP ]
    [ complem ]
    [ LP ]
    [ ANDSYM ]
    [ MULTOP ]
      (( EXPRESSION ))
      (( OUTEXPRESSION ))
      SY@ DUP SEMI = SWAP
      ENDSYM = OR NOT
      IF 51 CERROR
      ELSE INSYMBOL
      FI
      BREAK
    [ SEMI ]
      INSYMBOL
      BREAK
    [ ENDSYM ] BREAK
    [ EOSSYM ]
      61 CERROR
      BREAK
    [ DUP ] 52 CERROR
      BREAK

  ESAC
  SY@ SEMI =
  IF INSYMBOL FI
;

PROCEDURE TYPDLLIST
WHILE
  SY@ BEGINSYM <>
  SY@ EOSSYM <> AND NOT EXIT
  0 TYPENUMBER !
  AUTOVAR SC !
  WHILE
    (( NAMEOFTYPE TYPENUMBER ))
    (( NAMEOFSC )) OR NOT EXIT
  WEND
  SC @ AUTOVAR <>
  IF 81 CERROR
     AUTOVAR SC !
  FI
  (( DECLTOR 1 # CURRENT ))
  WHILE
    SY@ COMMA <> EXIT
    INSYMBOL
    (( DECLTOR 1 # CURRENT ))
  WEND
  SY@ SEMI = IF INSYMBOL FI
WEND
;

PROCEDURE FILLINPARAMOFFSETS
LOCAL 2 T 2 T1
      2 MODE 2 OFF ;
0 OFF !
0 CURRENT @ AUTOSIZE .!
WHILE
  PLIST @ .X CRLF
  PLIST @ NOT EXIT
  (( FINDID CURRENT @ TREE .@ #
            PLIST @ SENTRY .@
               NAME + ))
  T1 !
  T1 @
  IF T1 @ THISMODE .@ MODE !
     PARAMVAR T1 @ THISSTATE .!
  MODE @ TYPERECORD * TYPETAB +
  BASICTYPE OVER .@
     ARRAYTYPE =
     IF INDEXRANGE OVER .@ NOT
      IF (( POINTERTO
           (( INDIRECT MODE @ )) ))
          DUP T1 @ THISMODE .!
          MODE !
       FI
     FI DROP
     CURRENT @ AUTOSIZE .@
     T1 @ OFFSET .!
     (( SIZEOFTYPE MODE @ ))
        CURRENT @ AUTOSIZE + +!
  ELSE
     (( PUTINSYMBOLTABLE
          CURRENT #
          PLIST @ SENTRY .@
            NAME + #
          PARAMVAR # INT ))
     T1 !
     INT MODE !
  FI
  PLIST @ T !
  PLIST @ LFT .@ PLIST !
  T @ DISPOSE
  OFF T1 @ OFFSET .!
  (( SIZEOFTYPE MODE @ )) OFF +!
 WEND
 (( DISSYMTAB TSYMTAB @ ))
 OFF @ FUNCID @ OFFSET .!
 0 CURRENT @ AUTOSIZE .!
;

PROCEDURE FUNCTIONBODY
1 INFUNCDEF !
(( NEWSYMTAB CURRENT ))
(( NEWSYMTAB STRUCTDICT ))
(( TYPDLLIST ))
(( FILLINPARAMOFFSETS ))
(( COMPOUNDST 0 # 0 ))
CURRENT @ LASTLEVEL .@
  CURRENT !
STRUCTDICT @ LASTLEVEL .@
  STRUCTDICT !
(( DISSYMTAB STRUCTDICT @@ ))
0 STRUCTDICT @!
(( EXITCODE ))
;

PROCEDURE DATADEF
." Datadef
"
IDENTNAME @B SPACE <>
SC @ TYPEVAR <> AND
IF (( INLIZER ))
FI
WHILE
  SY@ COMMA <> EXIT
  SC @ EXTERNVAR <>
  IF (( PRINTSTATIC ))
     0 REACHABLE !
  FI
  INSYMBOL
  (( INTDLR ))
WEND
SY@ SEMI <> IF 55 CERROR
            ELSE INSYMBOL
            FI
;

PRINTER
DTOP @ .X CRLF SCREEN

FUNCTION INITSYMTAB
SYMBOLTABLE NEW
0 OVER AUTOSIZE .!
0 OVER TREE .!
0 OVER 2+!
0 OVER !
;

: INLISE
INITMEM
FREE?
(( INITSYMTAB )) GLOBALSYMBOL !

GLOBALSYMBOL @ CURRENT !
(( INITSYMTAB )) STRUCTDICT !
(( INITSYMTAB )) TEMPDICT !
FUNK 1 + 0
DO TYPETAB I TYPERECORD * +
   0 OVER FMASK .!
   0 OVER FOFFSET .!
   I FIELD <=
   IF I
     OVER BASICTYPE .!
     (( BASICSIZE I ))
     SWAP SIZE .!
   ELSE 0 OVER BASICTYPE .!
     0 SWAP SIZE .!
   FI
LOOP

TREERECORD NEW ENDNODE !
ALFA 0 DO 32 I NAME ENDNODE @
  . + !B LOOP
0 10 NAME ENDNODE @ . + !B
0 OFFSET ENDNODE @ .!
0 THISMODE ENDNODE @ .!
UNDEFINED THISSTATE ENDNODE @ .!

FUNK TYPEREF !
0 LEVEL !
0 BRACKETS !
0 BRACKETS 2+!
0 LASTLEVEL !
0 UNKNOWN !
0 STATFIL !
0 LITFILE !
0 DATFILE !

;

PROCEDURE EXTERNALDEF
REACHABLE 0!
GLOBALSYMBOL @ CURRENT !
SC 0!
TYPENUMBER 0!
INFUNCDEF 0!
WHILE
  (( NAMEOFTYPE TYPENUMBER ))
  (( NAMEOFSC )) OR NOT EXIT
WEND
SY@ MULTOP ==
OVER LP = OR
SWAP IDENT = OR
IF WHILE
     SY@ COMMA = IF INSYMBOL FI
     SC @ TYPEVAR =
     IF (( DECLTOR 0 # STRUCTDICT ))
     ELSE (( DECLTOR 0 # CURRENT ))
     FI
     TYY @ TYPERECORD *
     TYPETAB + BASICTYPE .@
     FUNK =
     IF INFUNCDEF @
        SC @ UNDEFINED == SWAP
           EXTSTATIC = OR AND
        (( PROCHEADER LASTID @ ))
        LASTID @ FUNCID !
        (( FUNCTIONBODY ))
        (( PRINTSTATIC ))
     ELSE INFUNCDEF 0!
        IDENTNAME @B SPACE <>
        SC @  TYPEVAR <> AND
        IF (( DATADEF )) FI
        SC @ EXTERNVAR <>
        IF (( PRINTSTATIC ))
           REACHABLE 0!
        FI
     FI

     SY@ COMMA <> EXIT
   WEND
ELSE WHILE SY@ SEMI ==
           OVER TYPESYM = OR
           OVER LP = OR
           OVER IDENT = OR
           SWAP MULTOP = OR EXIT
           INSYMBOL
     WEND
FI
(( DISSYMTAB GLOBALSYMBOL @@ ))
CURRENT @ GLOBALSYMBOL !
0 CURRENT @!
;

PROCEDURE CPROGRAM
WHILE
  SY@ EOSSYM = EXIT
  SY@ SEMI = IF INSYMBOL FI
  (( EXTERNALDEF ))
  SY@ SEMI = IF INSYMBOL FI
WEND
;

$L+

: PASS2
  INLISE
  NEXTLAB 0!
  NCH INSYMBOL
  (( CPROGRAM ))
;
