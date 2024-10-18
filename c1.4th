/* C1 2-FEB-86  P.J CHURCHYARD
DECIMAL
$L-
: @CH CHR @B ;

: @NCH NEXTCH DUP CHR !B ;

: NCH @NCH DROP ;

/* VERB USED IN INSYMBOL TO MARK
/* END OF FORTH CODE.
: */ ;


PROCEDURE NEXTINLIST ( SHEAD SNEXT
                       SY1 OP1 )
SHEAD @ @ NOT
IF (( NEWEXPNODE OP1 @ #
         SY1 @ ))
   DUP SHEAD @ !
   SNEXT @ !
ELSE (( NEWEXPNODE OP1 @ #
         SY1 @ ))
   DUP SNEXT @ @ !
       SNEXT @ !
FI
;

: ALPHA
  @CH DUP ' A >= SWAP ' Z <= AND
  @CH DUP ' a >= SWAP ' z <= AND
  OR
;
DECIMAL

: NUMERIC
  @CH DUP ' 0 >= SWAP ' 9 <= AND
  IF
    @CH ' 0 - VCH !B 1
  ELSE
    CBASE @ 16 =
    IF
      @CH DUP ' A >=
      SWAP ' F <= AND
      DUP IF
            @CH ' A - 10 +
            VCH !B
          FI
      @CH DUP ' a >=
      SWAP ' f <= AND
      DUP IF
            @CH ' a - 10 +
            VCH !B
          FI
      OR
    ELSE
      0
    FI
  FI
;

: WARN $L+ SCREEN
  ." WARNING " .X CRLF
;

: CERROR $L+ SCREEN
  ." ERROR " .X CRLF ERROR
;

: ESCAPECHAR
  @CH
  CASE
    [ ' n ] 10 NCH BREAK
    [ ' t ] 9  NCH BREAK
    [ ' b ] 8  NCH BREAK
    [ ' r ] 13 NCH BREAK
    [ ' f ] 12 NCH BREAK
    [ ' l ] 10 NCH BREAK
    [ DUP ] 1 NUMERIC
            IF
              0 WHILE
                  NUMERIC
                  3 NOVER 4 <
                  AND NOT EXIT
                  8 * @CH 48 -
                  + SWAP 1 +
                  SWAP NCH
                WEND
            ELSE
              @CH NCH
            FI
            SWAP DROP
  ESAC
;

: NORM NOT OVER * ;

: OP! OP ! ;

: SY! SY ! ;

: OPSY! SY! OP! ;

: SY@ SY @ ;

: OP@ OP @ ;

: IDTYPE! IDTYPE ! ;

PROCEDURE INSTRING
LOCAL 2 S 2 S1 2 CH1 2 CH2 ;
1 STRINGLENG !
S 0! S1 0! STRINGLIST 0!
WHILE
  @CH ' " == SWAP '  = OR EXIT
  @CH ' \ =
  IF NCH  ESCAPECHAR CH1 !
  ELSE @CH CH1 ! NCH
  FI
  (( NEXTINLIST S # S1 # KONSTANT # NOOP ))
  S1 @ KAR OVER EMODE + !
  CH1 @ SWAP SVALUE + !
  1 STRINGLENG +!
WEND
@CH '  = IF 67 CERROR FI
(( NEXTINLIST S # S1 # KONSTANT # NOOP ))
S1 @ KAR OVER EMODE + !
SVALUE + 0!
S @ STRINGLIST !
;

DTOP @ DUP DP !

" if" STR @ 1 + DP !
  IFSYM ,B 0 ,B
" do" STR @ 1 + DP !
  DOSYM ,B 0 ,B
" int" STR @ 1 + DP !
  TYPESYM ,B INT ,B
" for" STR @ 1 + DP !
  forsym ,B 0 ,B
" char" STR @ 1 + DP !
  TYPESYM ,B KAR ,B
" else" STR @ 1 + DP !
  ELSESYM ,B 0 ,B
" case" STR @ 1 + DP !
  CASESYM ,B 0 ,B
" goto" STR @ 1 + DP !
  GOTOSYM ,B 0 ,B
" long" STR @ 1 + DP !
  TYPESYM ,B LONG ,B
" void" STR @ 1 + DP !
  TYPESYM ,B 4 ,B
" auto"  STR @ 1 + DP !
  AUTO ,B 0 ,B
" break" STR @ 1 + DP !
  break ,B 0 ,B
" short" STR @ 1 + DP !
  TYPESYM ,B SHORT ,B
" iovar" STR @ 1 + DP !
  TYPESYM ,B KAR ,B
" float" STR @ 1 + DP !
  TYPESYM ,B REEL ,B
" union" STR @ 1 + DP !
  TYPESYM ,B UNION ,B
" while" STR @ 1 + DP !
  WHILESYM ,B 0 ,B
" return" STR @ 1 + DP !
  RETURN ,B 0 ,B
" switch" STR @ 1 + DP !
  SWITCH ,B 0 ,B
" double" STR @ 1 + DP !
  TYPESYM ,B DOUBLE ,B
" struct" STR @ 1 + DP !
  TYPESYM ,B struct ,B
" static" STR @ 1 + DP !
  STATIC ,B 0 ,B
" extern" STR @ 1 + DP !
  EXTERNSYM ,B 0 ,B
" sizeof" STR @ 1 + DP !
  TERMOP ,B 0 ,B
" typedef" STR @ 1 + DP !
  TYPEDEF ,B 0 ,B
" default" STR @ 1 + DP !
  DEFAULT ,B 0 ,B
" register" STR @ 1 + DP !
  REGISTER ,B 0 .B
" unsigned" STR @ 1 + DP !
  TYPESYM ,B UNSIGNED ,B
" continue" STR @ 1 + DP !
  continue ,B 0 ,B
0 ,
DP @ DTOP !
CONSTANT RESTABLE


: RESERVED
  NOOP OP!
  RESTABLE
  WHILE
    DUP @B NOT EXIT
    ID OVER $= NOT EXIT
    BEGIN
      DUP 1 + SWAP
      @B NOT
    END
    2 +
  WEND
  DUP @B NOT
  IF IDENT SY!
  ELSE
    1 + DUP @B SY!
    1 + DUP IDTYPE!
    SY@ TERMOP =
    IF SIZEOF OP! FI
  FI

;
PRINTER RESTABLE .X DTOP @ .X CRLF SCREEN


HEX
: ISSPACE @CH
  20 ==
  OVER D = OR
  OVER 1F = OR
  SWAP 1C = OR
;

DECIMAL
: INSYMBOL
  BEGIN
    WHILE
      ISSPACE NOT EXIT
      NCH
    WEND
  ALPHA
  IF
    1 IDCOUNT !
    @CH ID !B
    NCH
    WHILE
      ALPHA NUMERIC OR NOT EXIT
      IDCOUNT @ 10 = NOT
      IF
        @CH IDCOUNT @ ID + !B
	 1 IDCOUNT +!
      FI
	NCH
    WEND
    0 IDCOUNT @ ID + !B
    RESERVED
  ELSE NUMERIC
  IF 0 VALU !
     10 CBASE !
     @CH ' 0 =
     IF @NCH DUP ' X =
        SWAP ' x = OR
        IF NCH
	    16 CBASE !
        ELSE
	    8 CBASE !
        FI
     FI
     WHILE NUMERIC NOT EXIT
	VALU @ CBASE @ *
       VCH @B + VALU !
       NCH
     WEND
     NOOP KONSTANT OPSY!
     INT IDTYPE!
  ELSE
  @CH CASE
    [ ' ; ] NOOP SEMI OPSY!
	     NCH
            BREAK
    [ ' - ] @NCH ' - =
            IF DECR TERMOP OPSY!
		NCH
	     ELSE
              @CH ' > =
              IF
		 REFSELECTOR
                TERMOP OPSY!
                NCH
	       ELSE
		 minus ADDOP
                OPSY!
	       FI
	     FI
	     NULLTYPE IDTYPE!
	     BREAK
    [ ' + ] @NCH ' + =
	     IF INCR TERMOP OPSY!
		NCH
	     ELSE
	        PLUS ADDOP OPSY!
	     FI
	     NULLTYPE IDTYPE!
	     BREAK
    [ ' / ] @NCH ' * =
	     IF NCH
               BEGIN
                 @CH ' $ =
                 IF @NCH ' F ==
                    SWAP ' f = OR
                    IF ." CForth
" BEGIN
    QWF IF DUP @ #LIT */ =
          IF 1 ELSE
           EXECUTE 0
          FI
        ELSE NUMBER
           IF STATE @
             IF #LIT LIT ,C , FI
           0
           ELSE PRINT ." Unknown! WORD
" 1        FI
        FI
  END
." Back to C
"                    1
                    ELSE 0
                    FI
                 ELSE
                  @CH ' * =
                  IF @NCH ' / =
                  ELSE NCH 0
                  FI
                 FI
               END
	        COMMENTOP SY!
	        NCH
	     ELSE
		DIVIDE MULTOP
               OPSY!
		NULLTYPE IDTYPE!
	     FI
	     BREAK
    [ ' ! ] @NCH ' = =
	     IF NEOP EQUALITY
		NCH
	     ELSE
		NOTOP TERMOP
	     FI
            OPSY!
	     BREAK
    [ ' = ] @NCH
	     NOOP ASSIGN OPSY!
	     CASE
              [ ' = ]
		 EQOP EQUALITY
                OPSY!
		 BREAK
	       [ ' + ]
		 PLUS OP!
		 BREAK
	       [ ' - ]
		 minus OP!
		 BREAK
	       [ ' | ]
		 OROP OP!
		 BREAK
	       [ ' ^ ]
		 XOROP OP!
		 BREAK
	       [ ' * ]
		 TIMES OP!
		 BREAK
	       [ ' / ]
		 DIVIDE OP!
		 BREAK
	       [ ' & ]
		 ANDOP OP!
		 BREAK
	       [ ' < ]
		 @NCH ' < =
		 IF
		   LEFTSHIFT OP!
		 FI
		 BREAK
	       [ ' > ]
		 @NCH ' > =
		 IF
		   RIGHTSHIFT OP!
		 FI
		 BREAK
	     ESAC
	     OP@ DUP NOOP = SWAP
	     EQOP = OR NOT
	     IF 2 WARN
	     FI
	     OP@ NOOP <>
	     IF NCH
	        NULLTYPE IDTYPE!
	     FI
	     BREAK
    [ ' | ] @NCH ' | =
	     IF ORFOP ORFSYM
		NCH
	     ELSE
		OROP ORSYM
	     FI
            OPSY!
	     BREAK
    [ ' < ] @NCH DUP ' = =
	     IF LEOP COMP OPSY!
		NCH
	     ELSE
	       ' < =
	       IF LEFTSHIFT
                 SHIFTOP OPSY!
		  NCH
              ELSE
	          LTOP COMP OPSY!
	       FI
	     FI
	     BREAK
    [ ' > ] @NCH DUP ' = =
	     IF GEOP COMP OPSY!
		NCH
	     ELSE
	       ' > =
	       IF RIGHTSHIFT
                 SHIFTOP
		  NCH
	       ELSE
		  GTOP COMP
	       FI
              OPSY!
	     FI
	     BREAK
    [ ' & ] @NCH ' & =
	     IF ANDFOP ANDFSYM
		NCH
	     ELSE
		ANDOP ANDSYM
	     FI
            OPSY!
	     BREAK
    [ ' ( ] NOOP LP OPSY!
	     NCH
	     BREAK
    [ ' ) ] NOOP RP OPSY!
	     NCH
	     BREAK
    [ ' [ ] NOOP LB OPSY!
	     NCH
	     BREAK
    [ ' ] ] NOOP RB OPSY!
	     NCH
	     BREAK
    [ ' , ] NOOP COMMA OPSY!
	     NCH
	     BREAK
    [ ' ? ] IFOP IFOPSYM OPSY!
	     NCH
	     BREAK
    [ ' : ] ELSEOP IFOPSYM OPSY!
	     NCH
	     BREAK
    [ ' ^ ] XOROP XORSYM OPSY!
	     NCH
	     BREAK
    [ ' ~ ] COMPOP complem OPSY!
	     NCH
	     BREAK
    [ ' % ] MODOP MULTOP OPSY!
	     NCH
	     BREAK
    [ ' * ] TIMES MULTOP OPSY!
	     NCH
	     BREAK
    [ ' { ] NOOP BEGINSYM OPSY!
	     NCH
	     BREAK
    [ ' } ] NOOP ENDSYM OPSY!
	     NCH
	     BREAK
    [ ' " ] @NCH WHILE
		   ' " = EXIT
                  @NCH
	         WEND
	     NCH
	     NOOP STRING OPSY!
	     BREAK
    [ ' ' ] @NCH ' \ =
	     IF NCH
	        ESCAPECHAR
               VALUE !
	     ELSE @CH VALUE !
	          NCH
	     FI
	     @CH ' ' =
	     IF NCH
	     ELSE 0 CERROR
	     FI
	     NOOP KONSTANT OPSY!
	     KAR IDTYPE!
	     BREAK
    [ ' . ] SELECTOR TERMOP
            OPSY!
	     NCH
	     BREAK
    [ '  ] NOOP EOSSYM OPSY!
            BREAK
    [ DUP ] @CH .X 32 .B
            3 WARN
	     COMMENTOP SY!
	     NCH
	     BREAK
  ESAC
  FI
  FI
  SY@ COMMENTOP = NOT
  END
;

$L+
SMLOAD "D2"
