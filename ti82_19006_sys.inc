; TI-82 (19.006) Equates
; Found by comparison with TI-83 Plus
; CVSoft, 2021-08-30
; -----------------------------------

;Included for TASM Compatibility
;-------------------------------
#define equ .equ
#define EQU .equ
#define end .end
#define END .end

.define bcall(xxxx) call xxxx
.define bjump(xxxx) jp xxxx

__bank_call	.equ 2FA3h
__bank_jump	.equ 2FD2h

_CreateRList	.equ 2484h
_CreateReal	.equ 2478h
;_DelVar	.equ h ; not found in 19.006, maybe near $2A40
_FindSym	.equ 226Eh
_Mov9ToOP1	.equ 1BB3h
_MovFrOP1	.equ 1BBEh
_OP2ToOP1	.equ 1B3Ah
;_SetXXOP1	.equ h ; no entry point, but lives at 11A69h
; if you really want it, call __bank_call \ .dw 5A69h \ .db 04h
_SetXXXXOP2	.equ 36DFh; entry point at 11A89h
_cphlde		.equ 0095h
_lcd_busy	.equ 083ah

rFindSym	.equ 10h
rMov9ToOP1	.equ 20h


; RAM Equates
plotSScreen	.equ 88B8h
contrast	.equ 8008h
curRow		.equ 800Ch
FPS		.equ 8D10h
asm_exec_ram	.equ 9104h ; for CrASH 19.006




; flags
;--------------------------------
graphflags		equ	3
graphdraw		equ	0
graphcursor		equ	2

;Data Type Equates (ripped from ti83plus.inc)
---------------------------------
RealObj			equ 0
ListObj			equ 1
MatObj			equ 2


; Tokens from TI-83, many of these are invalid
;# Tokens in Use = 249
;--------------------------------
;length (+80h for leading blank)
;(+40h for trailing blank)
EOSSTART		equ	0

;Display Conversions Come Immediately Before 'tstore'
;----------------------------------------------------
DCONV			equ	01h
;
TTODMS			equ	DCONV ;01h
TTODEC			equ	DCONV+1 ;02h
TTOAbc			equ	DCONV+2 ;03h > A b/c
;
TSTORE			equ	DCONV+3 ;04h Lstore 01
;
TBOXPLOT		equ	05h
;
BRACKS			equ	06h
;
tLBrack			equ	BRACKS ;06h '['
tRBrack			equ	BRACKS+1 ;07h ']'
tLBrace			equ	BRACKS+2 ;08h '{'
tRBrace			equ	BRACKS+3 ; 09h '}'
;
TPOST1			equ	BRACKS+4
;
tFromRad		equ	TPOST1 ;0Ah Lradian
tFromDeg		equ	TPOST1+1 ;0Bh Ldegree
tRecip			equ	TPOST1+2 ;0Ch Linverse
tSqr			equ	TPOST1+3 ;0Dh Lsquare
tTrnspos		equ	TPOST1+4 ;0Eh Ltranspose
TCUBE			equ	TPOST1+5 ;0Fh '^3'
;
tLParen			equ	10h ;10h '('
tRParen			equ	11h ;11h ')'

;Immediate Unary Tokens Go From Here To 'EOSEL-1'
;--------------------------------------------------------
IMUN			equ	12h
;
tRound			equ	IMUN ;12h 'round'
TPXTST			equ	IMUN+1 ;13h 'PXL-TEST'
tAug			equ	IMUN+2 ;14h 'aug'
tRowSwap		equ	IMUN+3 ;15h 'rSwap'
tRowPlus		equ	IMUN+4 ;16h 'rAdd'
tmRow			equ	IMUN+5 ;17h 'multR'
tmRowPlus		equ	IMUN+6 ;18h 'mRAdd'
tMax			equ	IMUN+7 ;19h 'max'
tMin			equ	IMUN+8 ;1Ah 'min'
TRTOPr			equ	IMUN+9 ;1Bh 'R>Pr
TRTOPo			equ	IMUN+10 ;1Ch 'R>Po
TPTORx			equ	IMUN+11 ;1Dh 'P>Rx
TPTORy			equ	IMUN+12 ;1Eh 'P>Ry
tMEDIAN			equ	IMUN+13 ;1Fh 'MEDIAN
tRandM			equ	IMUN+14 ;20h 'randM'
TMEAN			equ	IMUN+15 ;21h
TROOT			equ	IMUN+16 ;22h 'ROOT'
tSeries			equ	IMUN+17 ;23h 'seq'
tFnInt			equ	IMUN+18 ;24h 'fnInt'
tNDeriv			equ	IMUN+19 ;25h 'fnIr'
TEVALF			equ	IMUN+20 ;26h
TFMIN			equ	IMUN+21 ;27h
TFMAX			equ	IMUN+22 ;28h

;Token 29h Cannot Be An EOS Function Since EL(=29h Already
;---------------------------------------------------------
tEOSEL			equ	IMUN+23
;
TSPACE			equ	tEOSEL ;29h ' '
tString			equ	tEOSEL+1 ;2Ah '"'
tComma			equ	tEOSEL+2 ;2Bh ','

Tii			equ	2Ch ;i

;Postfix Functions
;---------------------------------------------
TPOST			equ	2Dh
;
tFact			equ	TPOST ;2Dh '!'

TCUBICR			equ	2Eh
TQUARTR			equ	2Fh

;Number Tokens
;--------------------------------------------
NUMS			equ	30h
;
T0			equ	NUMS ;30h
T1			equ	NUMS+1 ;31h
T2			equ	NUMS+2 ;32h
T3			equ	NUMS+3 ;33h
T4			equ	NUMS+4 ;34h
T5			equ	NUMS+5 ;35h
T6			equ	NUMS+6 ;36h
T7			equ	NUMS+7 ;37h
T8			equ	NUMS+8 ;38h
T9			equ	NUMS+9 ;39h
TDECPT			equ	NUMS+10 ;3Ah
Tee			equ	NUMS+11 ;3Bh

;Binary OP
;----------------------------------------------
tOr			equ	3Ch ;3Ch '_or_'
TXOR			equ	3Dh ;3Dh
;
tColon			equ	3Eh ;3Eh ':'
tEnter			equ	3Fh ;3Fh Lenter

;Binary OP
;--------------------------------------------------
tAnd			equ	40h ;40h '_and_' 05

;Letter Tokens
;------------------------------------------
LET			equ	41h
;
TA			equ	LET ;41h
TB			equ	LET+1 ;42h
TC			equ	LET+2 ;43h
TD			equ	LET+3 ;44h
TE			equ	LET+4 ;45h
TF			equ	LET+5 ;46h
TG			equ	LET+6 ;47h
TH			equ	LET+7 ;48h
TI			equ	LET+8 ;49h
TJ			equ	LET+9 ;4Ah
TK			equ	LET+10 ;4Bh
TL			equ	LET+11 ;4Ch
TM			equ	LET+12 ;4Dh
TN			equ	LET+13 ;4Eh
TO			equ	LET+14 ;4Fh
TP			equ	LET+15 ;50h
TQ			equ	LET+16 ;51h
TR			equ	LET+17 ;52h
TS			equ	LET+18 ;53h
TT			equ	LET+19 ;54h
TU			equ	LET+20 ;55h
TV			equ	LET+21 ;56h
TW			equ	LET+22 ;57h
TX			equ	LET+23 ;58h
TY			equ	LET+24 ;59h
TZ			equ	LET+25 ;5Ah
TTHETA			equ	LET+26 ;5Bh

;These Var Tokens Are 1st Of A Double Token
; See 'vars.doc' For More Info On Vars
;------------------------------------------
VTOKS			equ	LET+27

;User Matrix Token, 2nd Token Needed For Name
;--------------------------------------------
TVARMAT			equ	VTOKS ;5Ch

;User List Token, 2nd Token Needed For Name
;-------------------------------------------
TVARLST			equ	VTOKS+1 ;5Dh

;User Equation Token, 2nd Token Needed For Name
;----------------------------------------------
TVAREQU			equ	VTOKS+2 ;5Eh

;This Is A Single Token, I Will Have To Read The Name In
;Every Single Time I Execute
;-------------------------------------------
TPROG			equ	VTOKS+3 ;5Fh

;User Pict Token, 2nd Token Needed For Name
;-------------------------------------------
TVARPICT		equ	VTOKS+4 ;60h

;User GDB Token, 2nd Token Needed For Name
;-------------------------------------------
TVARGDB			equ	VTOKS+5 ;61h

;System Vars That Are Used For Output Only
;Cannot Be Stored To By The User
;-------------------------------------------
tVarOut			equ	VTOKS+6 ;62h

;System Vars That Are Not In The Symbol Table
;But The User Can Store To Them
;--------------------------------------------
tVarSys			equ	VTOKS+7 ;63h


;Mode Setting Commands
;------------------------------------------------------
MODESA			equ	VTOKS+8 ;64h
;
tRad			equ	MODESA ;64h 'Radian'
tDeg			equ	MODESA+1 ; 65h 'Degree'
tNormF			equ	MODESA+2 ;66h 'Normal'
tSci			equ	MODESA+3 ;67h 'Sci'
tEng			equ	MODESA+4 ;68h 'Eng'
tFloat			equ	MODESA+5 ;69h 'Float'
;
CMPS			equ	6Ah
;
tEQ			equ	CMPS ;6Ah '=='
tLT			equ	CMPS+1 ;6Bh '<'
tGT			equ	CMPS+2 ;6Ch '>'
tLE			equ	CMPS+3 ;6Dh LLE
tGE			equ	CMPS+4 ;6Eh LGE
tNE			equ	CMPS+5 ;6Fh LNE

;Binary OP
;-------------------------------------------
tAdd			equ	70h ;70h '+'
tSub			equ	71h ;71h '-'
;
TANS			equ	72h ;72h

;Mode Setting Commands
;----------------------------------
MODES			equ	73h
;
tFix			equ	MODES ;73h 'Fix_'
TSPLITON		equ	MODES+1 ;74h
TFULLSCREEN		equ	MODES+2 ;75h
tStndrd			equ	MODES+3 ;76h 'Func'
tParam			equ	MODES+4 ;77h 'Param'
tPolar			equ	MODES+5 ;78h 'Pol'
TSEQG			equ	MODES+6 ;79h
TAFILLON		equ	MODES+7 ;7Ah 'AUTO FILL ON
TAFILLOFF		equ	MODES+8 ;7Bh
TACALCON		equ	MODES+9 ;7Ch
TACALCOFF		equ	MODES+10 ;7Dh 'AUTOFILL OFF

;Graph Format Tokens Are 2 Byte Tokens
;--------------------------------------------
TGFORMAT		equ	MODES+11 ;7Eh
;
TBOXICON		equ	7Fh
TCROSSICON		equ	80h
TDOTICON		equ	81h

;Binary OP
;---------------------------
tMul			equ	82h ;82h '*'
tDiv			equ	83h ;83h '/'

;Some Graph Commands
;-------------------------
GRCMDS			equ	84h
;
tTrace			equ	GRCMDS ;84h 'Trace'
tClDrw			equ	GRCMDS+1 ;85h 'ClDrw'
tZoomStd		equ	GRCMDS+2 ;86h 'ZStd'
tZoomtrg		equ	GRCMDS+3 ;87h 'Ztrg'
tZOOMBOX		equ	GRCMDS+4 ;88h 'ZBOX'
tZoomIn			equ	GRCMDS+5 ;89h 'ZIn'
tZoomOut		equ	GRCMDS+6 ;8Ah 'ZOut'
tZoomSqr		equ	GRCMDS+7 ;8Bh 'ZSqr'
tZoomInt		equ	GRCMDS+8 ;8Ch 'ZInt'
tZoomPrev		equ	GRCMDS+9 ;8Dh 'ZPrev'
tZoomDec		equ	GRCMDS+10 ;8Eh 'ZDecm'
TZOOMSTAT		equ	GRCMDS+11 ;8Fh 'ZSTAT
tUsrZm			equ	GRCMDS+12 ;90h 'ZRcl'
tPrtScrn		equ	GRCMDS+13 ;91h 'PrtScrn'
tZOOMSTO		equ	GRCMDS+14 ;92h  ZOOM STORE
TTEXT			equ	GRCMDS+15 ;93h

;Binary OP
;----------------------------
tnPr			equ	GRCMDS+16 ;94h '_nPr_'
tnCr			equ	GRCMDS+17 ;95h '_nCr_'

;More Graph Commands
;--------------------------------
tYOn			equ	GRCMDS+18 ;96h 'FnOn_'
tYOff			equ	GRCMDS+19 ;97h 'FnOff_'
tStPic			equ	GRCMDS+20 ;98h 'StPic_'
tRcPic			equ	GRCMDS+21 ;99h 'RcPic_'
tStoDB			equ	GRCMDS+22 ;9Ah 'StGDB_'
tRclDB			equ	GRCMDS+23 ;9Bh 'RcGDB_'
tLine			equ	GRCMDS+24 ;9Ch 'Line'
tVert			equ	GRCMDS+25 ;9Dh 'Vert_'
tPtOn			equ	GRCMDS+26 ;9Eh 'PtOn'
tPtOff			equ	GRCMDS+27 ;9Fh 'PtOff'

;Token A0 Cannot Be An EOS Function Since Low MULT=A0 Already
;------------------------------------------------------------
tPtChg			equ	GRCMDS+28 ;A0h 'PtChg'
TPXON			equ	GRCMDS+29 ;A1h
TPXOFF			equ	GRCMDS+30 ;A2h
TPXCHG			equ	GRCMDS+31 ;A3h
tShade			equ	GRCMDS+32 ;A4h 'Shade'
tCircl			equ	GRCMDS+33 ;A5h 'Circl'
tHORZ			equ	GRCMDS+34 ;A6h 'HORIZONTAL'
tTanLn			equ	GRCMDS+35 ;A7h 'TanLn'
tDrInv			equ	GRCMDS+36 ;A8h 'DrInv_'
tDrawF			equ	GRCMDS+37 ;A9h 'DrawF_'
;
TVARSTRNG		equ	0AAh

;Functions With No Arguments
;------------------------------------------------------
NOARG			equ	0ABh
;
tRand			equ	NOARG ;ABh 'rand'
tPi			equ	NOARG+1 ;ACh  Lpi
tGetKey			equ	NOARG+2 ;ADh 'getKy'
;
TAPOST			equ	TGETKEY+1 ;Apostrophe
TQUEST			equ	TAPOST+1 ;Question Mark

;Unary Functions
;---------------------------------------------------
;Unary: These unary functions take a copmbination of
;        arguments, real, list, matrix
UNARY			equ	TQUEST+1 ;B0h
;
tChs			equ	UNARY ;B0h
tInt			equ	UNARY+1 ;B1h
TABS			equ	UNARY+2 ;B2h
tDet			equ	UNARY+3 ;B3h
tIdent			equ	UNARY+4 ;B4h
tDim			equ	UNARY+5 ;B5h
tSum			equ	UNARY+6 ;B6h
tProd			equ	UNARY+7 ;B7h
tNot			equ	UNARY+8 ;B8h
tIPart			equ	UNARY+9 ;B9h
tFPart			equ	UNARY+10 ;BAh

;New 2 Byte Tokens
;-----------------------------------
T2BYTETOK		equ	0BBh

;UNARYLR: Unary functions that only work with
;          real numbers and lists
;----------------------------------------------
UNARYLR EQU     UNARY+12
;
tSqrt			equ	UNARYLR ;BCh
TCUBRT			equ	UNARYLR+1 ;BDh
tLn			equ	UNARYLR+2 ;BEh
tExp			equ	UNARYLR+3 ;BFh
tLog			equ	UNARYLR+4 ;C0h
tALog			equ	UNARYLR+5 ;C1h
tSin			equ	UNARYLR+6 ;C2h
tASin			equ	UNARYLR+7 ;C3h
tCos			equ	UNARYLR+8 ;C4h
tACos			equ	UNARYLR+9 ;C5h
tTan			equ	UNARYLR+10 ;C6h
tATan			equ	UNARYLR+11 ;C7h
tSinh			equ	UNARYLR+12 ;C8h
tASinh			equ	UNARYLR+13 ;C9h
tCosh			equ	UNARYLR+14 ;CAh
tACosh			equ	UNARYLR+15 ;CBh
tTanh			equ	UNARYLR+16 ;CCh
tATanh			equ	UNARYLR+17 ;CDh

;Some Programming Commands
;--------------------------------------------------------
PROGTOK			equ	UNARYLR+18
;
tIf			equ	PROGTOK ;CEh
tThen			equ	PROGTOK+1 ;CFh
tElse			equ	PROGTOK+2 ;D0h
tWhile			equ	PROGTOK+3 ;D1h
tRepeat			equ	PROGTOK+4 ;D2h
tFor			equ	PROGTOK+5 ;D3h
tEnd			equ	PROGTOK+6 ;D4h
tReturn			equ	PROGTOK+7 ;D5h
tLbl			equ	PROGTOK+8 ;D6h 'Lbl_'
tGoto			equ	PROGTOK+9 ;D7h 'Goto_'
tPause			equ	PROGTOK+10 ;D8h 'Pause_'
tStop			equ	PROGTOK+11 ;D9h 'Stop'
tISG			equ	PROGTOK+12 ;DAh 'IS>'
tDSL			equ	PROGTOK+13 ;DBh 'DS<'
tInput			equ	PROGTOK+14 ;DCh 'Input_'
tPrompt			equ	PROGTOK+15 ;DDh 'Prompt_'
tDisp			equ	PROGTOK+16 ;DEh 'Disp_'
tDispG			equ	PROGTOK+17 ;DFh 'DispG'

;Token E0 Cannot Be An EOS Function Since HI MU
;---------------------------------------------
tOutput			equ	PROGTOK+18 ;E0h 'Outpt'
tClLCD			equ	PROGTOK+19 ;E1h 'ClLCD'
tConst			equ	PROGTOK+20 ;E2h 'Fill'
tSortA			equ	PROGTOK+21 ;E3h 'sortA_'
tSortD			equ	PROGTOK+22 ;E4h 'sortD_'
TDISPTAB		equ	PROGTOK+23 ;E5h 'DISP TABLE
tMenu			equ	PROGTOK+24 ;E6h 'Menu'
tSENDMBL		equ	PROGTOK+25 ;E7h 'SEND'
tGETMBL			equ	PROGTOK+26 ;E8h 'GET'

;Stat Plot Commands
;-------------------------
STATPCMD		equ	PROGTOK+27
;
TPLOTON			equ	STATPCMD ;E9h ' PLOTSON'
TPLOTOFF		equ	STATPCMD+1 ;EAh ' PLOTSOFF
;
TLISTNAME		equ	0EBh ;list designator

TPLOT1			equ	0ECh
TPLOT2			equ	0EDh
TPLOT3			equ	0EEh

tPower			equ	0F0h ;'^'
tXRoot			equ	0F1h ;LsupX,Lroot

STATCMD			equ	0F2h
;
tOneVar			equ	STATCMD ;F2h 'OneVar_'
TTWOVAR			equ	STATCMD+1 ;F3h
tLR			equ	STATCMD+2 ;F4h 'LinR(A+BX
tLRExp			equ	STATCMD+3 ;F5h 'ExpR_'
tLRLn			equ	STATCMD+4 ;F6h 'LnR_'
tLRPwr			equ	STATCMD+5 ;F7h 'PwrR_'
TMEDMED			equ	STATCMD+6 ;F8h
TQUAD			equ	STATCMD+7 ;F9h
tCLRLST			equ	STATCMD+8 ;FAh 'CLEAR LIST
TCLRTBL			equ	STATCMD+9 ;FBH 'CLEAR TABLE
tHist			equ	STATCMD+10 ;FCh 'Hist_'
txyLine			equ	STATCMD+11 ;FDH 'xyline_'
tScatter		equ	STATCMD+12 ;FEh 'Scatter_'
tLR1			equ	STATCMD+13 ;FFh 'LINR(AX+B

;2nd Half Of Graph Format Tokens
;--------------------------------------------------
;Format Settings Commands
GFMT			equ	0
;
tSeq			equ	GFMT ;'SeqG'
tSimulG			equ	GFMT+1 ;'SimulG'
tPolarG			equ	GFMT+2 ;'PolarGC'
tRectG			equ	GFMT+3 ;'RectGC'
tCoordOn		equ	GFMT+4 ;'CoordOn'
tCoordOff		equ	GFMT+5 ;'CoordOff'
tDrawLine		equ	GFMT+6 ;'DrawLine'
tDrawDot		equ	GFMT+7 ;'DrawDot'
tAxisOn			equ	GFMT+8 ;'AxesOn'
tAxisOff		equ	GFMT+9 ;'AxesOff'
tGridOn			equ	GFMT+10 ;'GridOn'
tGridOff		equ	GFMT+11 ;'GridOff'
tLblOn			equ	GFMT+12 ;'LabelOn'
tLblOff			equ	GFMT+13 ;'LabelOff'
tWebOn			equ	GFMT+14 ;'WebOn'
tWebOff			equ	GFMT+15 ;'WebOFF'
TUV			equ	GFMT+16 ;U vs V
TVW			equ	GFMT+17 ;V vs W
TUW			equ	GFMT+18 ;U vs W

;2nd Half Of User Matrix Tokens
;-----------------------------------------
TMATA			equ	00h ;MAT A
TMATB			equ	01h ;MAT B
TMATC			equ	02h ;MAT C
TMATD			equ	03h ;MAT D
TMATE			equ	04h ;MAT E
TMATF			equ	05h ;MAT F
TMATG			equ	06h ;MAT G
TMATH			equ	07h ;MAT H
TMATI			equ	08h ;MAT I
TMATJ			equ	09h ;MAT J

;2nd Half Of User List Tokens
;------------------------------------------
TL1			equ	00h ;List 1
TL2			equ	01h ;List 2
TL3			equ	02h ;List 3
TL4			equ	03h ;List 4
TL5			equ	04h ;List 5
TL6			equ	05h ;List 6

;2nd Half Of User Equation Tokens
;--------------------------------------
;"Y" Equations Have Bit 4 Set
TY1			equ	10h ;Y1
TY2			equ	11h ;Y2
TY3			equ	12h ;Y3
TY4			equ	13h ;Y4
TY5			equ	14h ;Y5
TY6			equ	15h ;Y6
TY7			equ	16h ;Y7
TY8			equ	17h ;Y8
TY9			equ	18h ;Y9
TY0			equ	19h ;Y0

;Param Equations Have Bit 5 Set
;---------------------------------------
TX1T			equ	20h ;X1t
TY1T			equ	21h ;Y1t
TX2T			equ	22h ;X2t
TY2T			equ	23h ;Y2t
TX3T			equ	24h ;X3t
TY3T			equ	25h ;Y3t
TX4T			equ	26h ;X4t
TY4T			equ	27h ;Y4t
TX5T			equ	28h ;X5t
TY5T			equ	29h ;Y5t
TX6T			equ	2Ah ;X6t
TY6T			equ	2Bh ;Y6t

;Polar Equations Have Bit 6 Set
;--------------------------------------
TR1			equ	40h ;R1
TR2			equ	41h ;R2
TR3			equ	42h ;R3
TR4			equ	43h ;R4
TR5			equ	44h ;R5
TR6			equ	45h ;R6

;Recursion Equations Have Bit 7 Set
;--------------------------------------
TUN			equ	80h ;Un
TVN			equ	81h ;Vn
TWN			equ	82h ;Wn

;2nd Half Of User Picture Tokens
;----------------------------------------
TPIC1			equ	00h ;PIC1
TPIC2			equ	01h ;PIC2
TPIC3			equ	02h ;PIC3
TPIC4			equ	03h ;PIC4
TPIC5			equ	04h ;PIC5
TPIC6			equ	05h ;PIC6
TPIC7			equ	06h ;PIC7
TPIC8			equ	07h ;PIC8
TPIC9			equ	08h ;PIC9
TPIC0			equ	09h ;PIC0

;2nd Half Of User Graph Database Tokens
;----------------------------------------
TGDB1			equ	00h ;GDB1
TGDB2			equ	01h ;GDB2
TGDB3			equ	02h ;GDB3
TGDB4			equ	03h ;GDB4
TGDB5			equ	04h ;GDB5
TGDB6			equ	05h ;GDB6
TGDB7			equ	06h ;GDB7
TGDB8			equ	07h ;GDB8
TGDB9			equ	08h ;GDB9
TGDB0			equ	09h ;GDB0

;2nd Half Of String Vars
;----------------------------------
TSTR1			equ	00h
TSTR2			equ	01h
TSTR3			equ	02h
TSTR4			equ	03h
TSTR5			equ	04h
TSTR6			equ	05h
TSTR7			equ	06h
TSTR8			equ	07h
TSTR9			equ	08h
TSTR0			equ	09h

;2nd Half Of System Output Only Variables
;--------------------------------------------------------------------
;OPEN			equ	00h
;
TREGEQ			equ	01h ;REGRESSION EQUATION
;
TSTATN			equ	02h ;STATISTICS N
;
TXMEAN			equ	03h ;X MEAN
TSUMX			equ	04h ;SUM(X)
TSUMXSQR		equ	05h ;SUM(X^2)
TSTDX			equ	06h ;STANDARD DEV X
TSTDPX			equ	07h ;STANDARD DEV POP X
TMINX			equ	08h ;MIN X VALUE
TMAXX			equ	09h ;MAX X VALUE
;
TMINY			equ	0Ah ;MIN Y VALUE
TMAXY			equ	0Bh ;MAX Y VALUE
TYMEAN			equ	0Ch ;Y MEAN
TSUMY			equ	0Dh ;SUM(Y)
TSUMYSQR		equ	0Eh ;SUM(Y^2)
TSTDY			equ	0Fh ;STANDARD DEV Y
TSTDPY			equ	10h ;STANDARD DEV POP Y
;
TSUMXY			equ	11h ;SUM(XY)
TCORR			equ	12h ;CORRELATION
TMEDX			equ	13h ;MED(X)
TQ1			equ	14h ;1ST QUADRANT OF X
TQ3			equ	15h ;3RD QUADRANT OF X
TQUADa			equ	16h ;1ST TERM OF QUAD POLY REG/ Y-INT
TQUADb			equ	17h ;2ND TERM OF QUAD POLY REG/ SLOPE
TQUADc			equ	18h ;3RD TERM OF QUAD POLY REG
TCUBEd			equ	19h ;4TH TERM OF CUBIC POLY REG
TQUARTe			equ	1Ah ;5TH TERM OF QUART POLY REG
TMEDX1			equ	1Bh ;x1 FOR MED-MED
TMEDX2			equ	1Ch ;x2 FOR MED-MED
TMEDX3			equ	1Dh ;x3 FOR MED-MED
TMEDY1			equ	1Eh ;y1 FOR MED-MED
TMEDY2			equ	1Fh ;y2 FOR MED-MED
TMEDY3			equ	20h ;y3 FOR MED-MED
;
TRECURN			equ	21h ;RECURSION N
TSTATP			equ	22h
TSTATZ			equ	23h
TSTATT			equ	24h
TSTATCHI		equ	25h
TSTATF			equ	26h
TSTATDF			equ	27h
TSTATPHAT		equ	28h
TSTATPHAT1		equ	29h
TSTATPHAT2		equ	2Ah
TSTATMEANX1		equ	2Bh
TSTATSTDX1		equ	2Ch
TSTATN1			equ	2Dh
TSTATMEANX2		equ	2Eh
TSTATSTDX2		equ	2Fh
TSTATN2			equ	30h
TSTATSTDXP		equ	31h
TSTATLOWER		equ	32h
TSTATUPPER		equ	33h
TSTAT_S			equ	34h
TLRSQR			equ	35h ;r^2
TBRSQR			equ	36h ;R^2

;These next tokens are only used to acess the data.
;They are display only and the user cannot access them at all.
;-------------------------------------------------------------
TF_DF			equ	37h ;ANOFAV FACTOR DF
TF_SS			equ	38h ;ANOFAV FACTOR SS
TF_MS			equ	39h ;ANOFAV FACTOR MS
TE_DF			equ	3Ah ;ANOFAV ERROR DF
TE_SS			equ	3Bh ;ANOFAV ERROR SS
TE_MS			equ	3Ch ;ANOFAV ERROR MS

;2nd Half Of System Input/Output Variables
;----------------------------------------------------
;System Variable Equates
;
;Vars With No Restriction
TUXSCL			equ	0
TUYSCL			equ	1
TXSCL			equ	2
TYSCL			equ	3
TRECURU0		equ	4 ;U 1ST INITIAL COND
TRECURV0		equ	5 ;V 1ST INITIAL COND
TUN1			equ	6 ;U(N-1) ;NOT USED
TVN1			equ	7 ;V(N-1) ;NOT USED
TURECURU0		equ	8
TURECURV0		equ	9

;Vars Rounded To 10 Digits
;----------------------------------
TXMIN			equ	0Ah
TXMAX			equ	0Bh
TYMIN			equ	0Ch
TYMAX			equ	0Dh
TTMIN			equ	0Eh
TTMAX			equ	0Fh
TTHETAMIN		equ	10h
TTHETAMAX		equ	11h
TUXMIN			equ	12h
TUXMAX			equ	13h
TUYMIN			equ	14h
TUYMAX			equ	15h
TUTHETMIN		equ	16h
TUTHETMAX		equ	17h
TUTMIN			equ	18h
TUTMAX			equ	19h
TTBLMIN			equ	1Ah
TPLOTSTART		equ	1Bh
TUPLOTSTART		equ	1Ch
TNMAX			equ	1Dh
TUNMAX			equ	1Eh
TNMIN			equ	1Fh
TUNMIN			equ	20h

;Vars Cannot Be 0
;----------------------------------
TTBLSTEP		equ	21h
TTSTEP			equ	22h
TTHETASTEP		equ	23h
TUTSTEP			equ	24h
TUTHETSTEP		equ	25h

;Special Limits On These
;----------------------------------
TDELTAX			equ	26h
TDELTAY			equ	27h

;Special Limits On These
;----------------------------------
TXFACT			equ	28h
TYFACT			equ	29h

TTBLINPUT		equ	2Ah

;Any Restrictions?
;-----------------------------------------
TFINn			equ	2Bh
TFINi			equ	2Ch
TFINpv			equ	2Dh
TFINpmt			equ	2Eh
TFINfv			equ	2Fh
TFINpy			equ	30h
TFINcy			equ	31h
;
TRECURW0		equ	32h ;W0(1)
TURECURW0		equ	33h
;
TPLOTSTEP		equ	34h
TUPLOTSTEP		equ	35h
;
TXRES			equ	36h
TUXRES			equ	37h

;All The Tokens Below Do Not Have Strings Associated.
;They Are Here Just For Storing And Recalling.
;----------------------------------------------------
TRECURU02		equ	38h ;U0(2)
TURECURU02		equ	39h
TRECURV02		equ	3Ch ;V0(2)
TURECURV02		equ	3Dh
TRECURW02		equ	3Eh ;W0(2)
TURECURW02		equ	3Fh

;No Restrictions
;------------------------------------------
;TWN1			equ	40h ;W(N-1)
;UN2			equ	41h ;U(N-2)
;VN2			equ	42h ;V(N-2)
;WN2			equ	43h ;W(N-2)

;Don't Know If Need These Yet?
;-----------------------------
;TUI12
;TUI22
;TUUI12
;TUUI22

TFINnpv			equ	00h ;immediate unary
TFINirr			equ	01h ;immediate unary
TFINbal			equ	02h ;immediate unary
TFINprn			equ	03h ;immediate unary
TFINint			equ	04h ;immediate unary
TFINtonom		equ	05h ;immediate unary
TFINtoeff		equ	06h ;immediate unary
TFINdbd			equ	07h ;immediate unary
TLCM			equ	08h ;immediate unary
TGCD			equ	09h ;immediate unary
TRANDINT		equ	0Ah ;immediate unary
TRANDBIN		equ	0Bh ;immediate unary
TSUBSTRNG		equ	0Ch ;immediate unary
TSTDDEV			equ	0Dh ;immediate unary
TVARIANCE		equ	0Eh ;immediate unary
TINSTRNG		equ	0Fh ;immediate unary
TDNORMAL		equ	10h ;immediate unary
TINVNORM		equ	11h ;immediate unary
TDT			equ	12h ;immediate unary
TCHI			equ	13h ;immediate unary
TDF			equ	14h ;immediate unary
TBINPDF			equ	15h ;immediate unary
TBINCDF			equ	16h ;immediate unary
TPOIPDF			equ	17h ;immediate unary
TPOICDF			equ	18h ;immediate unary
TGEOPDF			equ	19h ;immediate unary
TGEOCDF			equ	1Ah ;immediate unary
TNORMALPDF		equ	1Bh ;immediate unary
TTPDF			equ	1Ch ;immediate unary
TCHIPDF			equ	1Dh ;immediate unary
TFPDF			equ	1Eh ;immediate unary
;TRANDBIN		equ	1Fh ;immediate unary
TRANDNORM		equ	1Fh ;immediate unary
TFINFpmt		equ	20h ;0 or multy arg function
TFINFi			equ	21h ;0 or multy arg function
TFINFpv			equ	22h ;0 or multy arg function
TFINFn			equ	23h ;0 or multy arg function
TFINFfv			equ	24h ;0 or multy arg function
TCONJ			equ	25h ;unary
TREAL			equ	26h ;unary
TIMAG			equ	27h ;unary
TANGLE			equ	28h ;unary
TCUMSUM			equ	29h ;unary
TEXPR			equ	2Ah ;unary
TLENGTH			equ	2Bh ;unary
TDELTALST		equ	2Ch ;unary
TREF			equ	2Dh ;unary
TRREF			equ	2Eh ;unary
TTORECT			equ	2Fh ;display conversion
TTOPOLAR		equ	30h ;display conversion
TCONSTE			equ	31h ;function no arg
TSINREG			equ	32h ;command opt push
TLOGISTIC		equ	33h ;command opt push
TLINREGTTEST		equ	34h ;command opt push
TSHADENORM		equ	35h ;command pushed
TSHADET			equ	36h ;command pushed
TSHADECHI		equ	37h ;command pushed
TSHADEF			equ	38h ;command pushed
TMATTOLST		equ	39h ;command pushed
TLSTTOMAT		equ	3Ah ;command pushed
TZTEST			equ	3Bh ;command pushed
TTTEST			equ	3Ch ;command pushed
T2SAMPZTEST		equ	3Dh ;command pushed
T1PROPZTEST		equ	3Eh ;command pushed
T2PROPZTEST		equ	3Fh ;command pushed
TCHITEST		equ	40h ;command pushed
TZINTVAL		equ	41h ;command pushed
T2SAMPZINT		equ	42h ;command pushed
T1PROPZINT		equ	43h ;command pushed
T2PROPZINT		equ	44h ;command pushed
TGRAPHSTYLE		equ	45h ;command pushed
T2SAMPTTEST		equ	46h ;command opt pushed
T2SAMPFTEST		equ	47h ;command opt pushed
TTINTVAL		equ	48h ;command opt pushed
T2SAMPTINT		equ	49h ;command opt pushed
TSETUPLST		equ	4Ah ;command opt pushed
TFINpmtend		equ	4Bh ;mode command
TFINpmtbeg		equ	4Ch ;mode command
TREALM			equ	4Dh ;mode command
TPOLARM			equ	4Eh ;mode command
TRECTM			equ	4Fh ;mode command
TEXPRON			equ	50h ;mode command
TEXPROFF		equ	51h ;mode command
TCLRALLLST		equ	52h ;command not pushed
TGETCALC		equ	53h ;command not pushed
TDELVAR			equ	54h ;command not pushed
TEQUTOSTRNG		equ	55h ;command not pushed
TSTRNGTOEQU		equ	56h ;command not pushed
tdellast		equ	57h ;command not pushed
TSELECT			equ	58h ;command not pushed
TANOVA			equ	59h ;command not pushed
TMODBOX			equ	5Ah ;icon
TNORMPROB		equ	5Bh ;icon

;Tokens 5Ch to 63h cannot be the 2nd Byte of
; two byte tokens. These are already the 1st byte value
; of two byte tokens. Reading tokens backwards Will not
; work if this restriction is not in place.
;------------------------------------------------------
;NO USE			equ	5Ch
;NO USE			equ	5Dh
;NO USE			equ	5Eh
;NO USE			equ	5Fh
;NO USE			equ	60h
;NO USE			equ	61h
;NO USE			equ	62h
;NO USE			equ	63h

TMGT			equ	64h ;VERTICAL SPLIT
TZFIT			equ	65h ;ZOOM FIT
TDIAG_ON		equ	66h ;DIANOSTIC DISPLAY ON
TDIAG_OFF		equ	67h ;DIANOSTIC DISPLAY OFF