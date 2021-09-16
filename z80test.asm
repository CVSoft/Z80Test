.define TI84PCSE

.define VERSION 5
; make it run on many calculators
.ifdef TI73 ; very broken; Brass probably needs an external packager
 .echoln "Building for TI-73 Nostub"
 #include "ti73.inc"
 .binarymode ti73
 .variablename "Z80TEST"
 .define Flash
 .define T6A04
 .define _DelVarArc _DelVar
 .define scratchMem appbackupscreen
 ; TI-73 nostub has a header
 .db ProgTok,t0,t1
 ; program size (can Brass even calculate this?)
 .db t0,t0,t0
 .db tEnter
 .org asm_exec_ram
.elseifdef Mallard
 .echoln "Building for TI-73 with Mallard"
; #include "mallard.inc"
 #include "ti73.inc"
 .binarymode ti73
 .variablename "Z80TEST"
 .define TI73
 .define Flash
 .define T6A04
 .define _DelVarArc _DelVar
 .define scratchMem appBackupScreen
 .org userMem
 ; TI-73 Mallard has a header
 .db tStop,0,"Duck"
 .dw SetVersion ; entry point, beginning of executable code
 .db "Z80 Tester",0
.elseifdef TI8219006
 .echoln "Building for TI-82 CrASH 19.006"
 #include "ti82_19006_sys.inc"
 .binarymode ti82
 .variablename "Z80TEST"
 .define TI82
 .define TI83
 .define T6A04
 .define USES_PLOT
 .define _DelVarArc _DelVar
 .define scratchMem plotSScreen ; mark as dirty!
 .org asm_exec_ram ; CrASH 19.006
 .db "Z80Test",0
.elseifdef TI83
 .echoln "Building for TI-83 Nostub"
 #include "ti83.inc"
 .binarymode ti83
 .variablename "Z80TEST"
 .define T6A04
 .define USES_PLOT
 .define bcall(xxxx) call xxxx
 .define _DelVarArc _DelVar
 .define scratchMem plotSScreen
 .unsquish ; nostub for Send(9
 .org asm_exec_ram
.elseifdef TI84PCSE
 .echoln "Building for TI-84 Plus C SE Nostub"
 #include "ti84pcse.inc"
 .binarymode ti8x
 .variablename "Z80TEST"
 .define Flash
 .define TI8X
 .define WideScreen
 .define scratchMem tempSwapArea ; better saferam available?
 .org userMem-2
 .db tExtTok,tAsm84CCmp
.elseifdef TI8X
 .echoln "Building for TI-83 Plus/TI-84 Plus Nostub"
 #include "ti83plus.inc"
 .binarymode ti8x
 .variablename "Z80TEST"
 .define Flash
 .define T6A04
 .define scratchMem appbackupscreen
 ; TI-83 Plus nostub has a header
 .org userMem-2
 .db t2ByteTok,tasmCmp
.elseifdef TI86
 .echoln "Building for TI-86 Nostub"
 #include "ti86.inc"
 .binarymode ti86
 .variablename "Z80Test"
 .define WideScreen
 .define USES_PLOT
 .define bcall(xxxx) call xxxx
 .define _DelVarArc _DelVar
 .define _Mov9ToOP1 _Mov10ToOP1 ; floating points are wider
 .define _MovFrOP1 _ABS_MovFrOP1 ; 86 uses 24 bit absolute addressing
 .define curcol _curCol
 .define scratchMem _plotsscreen ; mark as dirty! (done)
 .org _asm_exec_ram-2
 .db $8E,$28
.endif


SetVersion: ;L1(1)
 ld a,VERSION
 call PutAInResults

TestSLL: ;L1(2)
 ld a,%01010101
 .db $CB, $37
 cp %10101011
 call DispMsg

TestInternalCarry: ;L1(3)
 ld a,0
 dec a
 daa
 cp $99 ; Z180 should be $F9, DAA has an internal carry issue
 call DispMsg

TestMLT: ;L1(4)
 ; do something to see if MLT rr instruction exists (Z180 test)
 ld h,2
 ld l,2
 .db $ED,$C6 ; mlt hl
 ld a,l
 cp 2
 call DispMsg

TestRLDUsesA: ;L1(6)
 ld a,$50
 ld hl,ScratchWord
 ld (hl),$30
 rld ; Z80, Z flag shouldn't be set. Z180, Z flag should be set.
 push af
 pop bc
 ld a,c
 and %01000000
 cp %00000000 ; desired behavior is Z unset
 call DispMsg
 
TestBit3: ;L1(7)
 call TestBit35
 and %00001000
 cp %00001000
 call DispMsg
 
TestBit5: ;L1(8)
 call TestBit35
 and %00100000
 cp %00100000
 call DispMsg

TestSCFSCF: ;L1(9) (bitmasked)
 ld hl,0
 ld (ScratchWord),hl
 ld hl,ScratchWord
 ; test scf \ scf sets bits 5 and 3 of flags from accumulator value
 di
 ld a,%00101000
 scf
 scf
 push af
 pop de
 ld a,e ; we use E instead of C (seen later) because in test 2,
 ; we can only know if bits were cleared if they 
 ; can be set in the first place. so, we save this
 ; result in E and never use it elsewhere. 
 and %00101000 
 cp %00101000  
 jr nz,TestSCFSCF1Fail
 set 0,(hl)
 TestSCFSCF1Fail:
 cpl
 ld e,a ; bits 5 and 3 of E unset if test passes.
 ; OR this with the subresult of next test to invalidate the result
 ; if test 1 fails.
 ;
 ; test scf \ scf clears bits 5 and 3 of flags from accumulator value
 ld a,%00101000
 scf
 scf
 ld a,0
 scf
 scf
 push af
 pop bc
 ld a,c
 or e ; check if bits are settable in the first place
 and %00101000
 cp 0
 jr nz,TestSCFSCF2Fail
 set 1,(hl)
 TestSCFSCF2Fail:
 ; test (non-flag-modifying op) \ scf ORs bits 5 and 3 of flags from a
 ld a,%00100000
 scf
 scf
 ld a,%00001000
 nop
 scf
 push af
 pop bc
 ld a,c
 and %00101000
 cp %00101000
 jr nz,TestSCFSCF3Fail
 set 2,(hl)
 TestSCFSCF3Fail:
 ld a,%00001000
 scf
 scf
 ld a,%00100000
 nop
 scf
 push af
 pop bc
 ld a,c
 and %00101000
 cp %00101000
 jr nz,TestSCFSCF4Fail
 set 3,(hl)
 TestSCFSCF4Fail:
 ei
 ld hl,(scratchWord)
 ld a,l
 ld (Result),a
 cp %00001111
 call DispMsg
 
TestUndocNeg: ;L1(10)
 ld a,$2F
 .db $ED,$5C ; undocumented NEG
 cp $D1
 call DispMsg

TestIFFBug: ;L1(11)
 ei
 ld a,0
 ld bc,0
 ld d,$17
 Loop:
  ld a,i
  jp po,TestIFFBugPass
  djnz Loop
  dec c
  jp nz,Loop
  dec d
  jp nz,Loop
 TestIFFBugFail:
 ld a,0
 call PutAInResults
 jr TestASICVersion
 TestIFFBugPass:
 ld a,1
 call PutAInResults

TestASICVersion: ;L1(12)
 ; check if port 05 is shadowed
 .ifdef TI8X
  in a,($02)
  and %10000000
  cp 0
  jr nz,TestASICVersionPass
  xor a
  ccf
  jr TestASICVersionFail
  TestASICVersionPass:
  in a,($15)
  TestASICVersionFail:
  ld (Result),a
  call PutAInResults
 .else
  ld a,0
  ld (Result),a
  call PutAInResults
 .endif

TestLCDBusyDelay:
 ; try to measure (roughly) how long LCD delay is
 ; expect ~ 60cycle on old HW and ~ 4cycle on new HW
 .ifdef TI86
  ld a,1
  ld (Result),a
  call PutAInResults
 .elseifdef TI84PCSE ; might be worth testing *something*
  ; maybe this?
  ; xor a
  ; out ($10),a
  ; out ($10),a (16-bit?)
  ; ld a,$11
  ; in h,(a)
  ; in l,(a)
  ; ld de,$9335
  ; bcall(_cphlde)
  ; call DispMsg
  ld a,1
  ld (Result),a
  call PutAInResults
 .else ; port-mapped grayscale display
 ; this goes from 1 to 2 at 62 cycles
  ; make sure this bit isn't permanently high for some silly reason
  call lcd_busy
  in a,($10)
  rla
  jr c,TestLCDBusyDelayLoopInfiniteLoopCatcher
  ld b,0
  ; do something that takes time to process
  call lcd_busy
  ld a,$02
  di
  out ($10),a
  TestLCDBusyDelayLoop:
   in a,($10)
   inc b
   rla
   jr c,TestLCDBusyDelayLoop
   jp pe,TestLCDBusyDelayLoopExit ; overflow is a real risk now lol
  TestLCDBusyDelayLoopExit:
  ei
  dec b ; only count complete loops
  ; safely wait until LCD is ready, then put old contrast value back
  call lcd_busy
  ld a,$03
  out ($10),a
  call lcd_busy
  ld a,b
  call PutAInResults
  jr TestLCDBusyDelayLoopDone ; next block

  TestLCDBusyDelayLoopInfiniteLoopCatcher:
   ld a,$0FF
   call PutAInResults
  TestLCDBusyDelayLoopDone:
  ; delay is 31*r-50 <= true < 31*r-19 cycles
 .endif

.ifdef T6A04
TestLCDFunctions:
 ld hl,saveSScreen
 push hl
  .ifdef TI8X
   bcall(_SaveDisp)
  .else
   bcall(_SaveOScreen)
  .endif
   di ;bcall(_SaveDisp) disables interrupts, but maybe not on 83/82
  .ifndef DEBUG
   call DisableLCD
  .endif
  call TestInAKillsVRAMPointer
  call PutAInResults
  call TestValidColumns
  call PutAInResults
  .ifndef DEBUG
   call EnableLCD
  .endif
 pop hl
 .ifdef TI8X
  ld b,64
  bcall(_RestoreDisp)
 .else
  bcall(_RstrOScreen)
 .endif
 ei
.else
TestLCDFunctions:
 xor a
 call PutAInResults
 call PutAInResults
.endif

DoneExecuting:
 .ifdef USES_PLOT
  set graphDraw,(iy+graphFlags)
 .endif
 jp ListSetup

;=============================================================================
; Subtests

TestBit35:
 ; put bits 3 and 5 of flags in known state
 ld a,$FF
 cp $aa
FintoA:
 push bc
 push af
 pop bc
 ld a,c
 pop bc
 ret
 
; ------
; LCD subtests
; ------

.ifdef T6A04

TestInAKillsVRAMPointer:
 .define USES_PLOT
 ; Runs with interrupts disabled!
 ld hl,$0000
 call SetCursorPosition
 ld b,2
 M_OuterLoop:
  push bc
  xor a
  ld b,64
  M_Loop:
   out ($11),a
   call evil_lcd_busy
   inc a
   djnz M_Loop
  pop bc
  djnz M_OuterLoop
 .ifdef DEBUG
  call WaitForKey
 .endif
 ld hl,$0000
 call SetCursorPosition
 ld hl,plotSScreen
 push hl
 ld b,64
 call ReadStorageFromLCD
 pop hl
 ld b,64
 ld c,0
 CheckLoop:
  ld a,(hl)
  inc hl
  cp c
  jr nz,CheckLoopFail
  inc c
  djnz CheckLoop
 xor a
 ret
 CheckLoopFail:
 ld a,1
 ret 

TestValidColumns:
 ; Runs with interrupts disabled!
 ; copy one column of counter to plotSScreen, then a column of blanks to plotSScreen+64
 ; TODO make this use the upper half of appBackupScreen
 .define USES_PLOT
 ld a,0
 ld b,64
 ld hl,plotSScreen
 push hl
  M_CounterLoop:
   ld (hl),a
   inc a
   inc hl
   djnz M_CounterLoop
  xor a
  ld b,64
  M_BlankLoop:
   ld (hl),a
   inc hl
   djnz m_BlankLoop
 pop hl
 ld b,0
 M_TestLoop:
  push bc
  call TestColumn
  pop bc
  cp 0
  jr z,M_TestExit
  inc b
  ld a,b
  cp $20
  jr z,M_TestExit
  jr M_TestLoop
 M_TestExit:
 ld a,b
 ret

lcd_busy:
 ; TI's busy routine. works pretty well. 
 ; inputs  : none
 ; destroys: none
 ; outputs : a not-busy LCD
 ;           60 cycles to execute, including the call lcd_busy that brought you here
 push af
 inc hl
 dec hl
 pop af
 ret

evil_lcd_busy:
 ; iFastCopy's busy routine. problematic on Kinpo LCD controllers.
 ; inputs  : none
 ; destroys: none, or maybe your LCD controller registers
 ; outputs : a not-busy LCD
 push af
 ELB_Loop:
  in a,($10)
  rla
  jr c,ELB_Loop
 pop af
 ret

DisableLCD:
 ; Turn off the LCD (it remains powered)
 push af
 ld a,$02
 DoLCDCommand:
 out ($10),a
 pop af
 jp lcd_busy

EnableLCD:
 push af
 ld a,$03
 jp DoLCDCommand

SetCursorPosition:
 ; inputs  : h: column number, starting at 0
 ;           l: row number, starting at 0
 ; outputs : overflow-corrected position is stored in hl ~~and LastPosition~~
 ; destroys: none (or hl, depending on your definition)
 ; TODO: just bit-mask instead of range checking
 push af
 push de
 ld a,h
 ld e,$20
 add a,e
 cp $3F
 jr c,SCP_NoColOverflow
 ld a,$3F
 SCP_NoColOverflow:
 out ($10),a
 call lcd_busy
 sub e
 ld h,a
 ld a,l
 ld e,$80
 add a,e
 jr nc,SCP_NoRowOverflow
 ld a,$BF
 SCP_NoRowOverflow:
 out ($10),a
 sub e
 ld l,a
; ld (LastPosition),hl
 pop de
 pop af
 jp lcd_busy

WriteStorageToLCD:
 ; it just sends data safely to the LCD
 ; inputs  : hl points to RAM to write to the LCD
 ;           b contains number of rows to write
 ; destroys: a b hl
 ; outputs : LastPosition is incremented properly, but slowly
 ;           hl points to the byte after the last one written
  ld a,(hl)
  inc hl
  out ($11),a
  call lcd_busy
  djnz WriteStorageToLCD
 ret

ReadStorageFromLCD:
 ; ReadFromLCD
 ; it just sends data safely from the LCD
 ; inputs  : hl points to where you want data to come to
 ;           b contains number of rows to read
 ; destroys: a b hl
 ; outputs : hl points to the byte after the last one saved
 in a,($11) ; dummy read (does this work?)
 call lcd_busy
 ReadFromLCD:
  in a,($11)
  call lcd_busy
  ld (hl),a
  inc hl
  djnz ReadFromLCD
 ret

TestColumn:
 ; Test a column to see if it retains data
 ; Inputs  : b is column number to test (in range [0-1F])
 ; Destroys: all but de?
 ; Outputs : a=1 if pass, a=0 if fail
 .define USES_PLOT
 ld h,b
 ld l,0
 push hl
  call SetCursorPosition
  ld hl,plotSScreen
  ld b,64
  call WriteStorageToLCD
 pop hl
 call SetCursorPosition
 ld hl,plotSScreen+128
 push hl
  ld b,64
  call ReadStorageFromLCD
 pop hl
 ld b,64
 ld c,0
 TC_CheckLoop:
  ld a,(hl)
  inc hl
  cp c
  jr nz,TC_Fail
  inc c
  djnz TC_CheckLoop
 ld a,1
 ret
 TC_Fail:
 xor a
 ret

.endif

; Exit Code and stuff

DispMsg:
 ; success in carry flag
 ; msg in hl (registers destroyed)
 ; automatically increments test result counter and stores result
 ; this is spaghetti code now, it used to fancily display the result
 jr z,DispTestPass
; ld a,0
 ld a,(Result)
 DispMsgStatus:
 jp PutAInResults

DispTestPass:
 ; put result in a (if not result, 1 in a) and go back to code to put into results
 ld a,(Result)
 cp 0
 jr nz,DispTestPassWithResult
 ld a,1
 DispTestPassWithResult:
 push af
 ld a,0
 ld (Result),a
 pop af
 jp PutAInResults

PutResultInResults:
 ld a,(Result)
PutAInResults:
 ld hl,scratchMem
 ld de,(ResultsPtr)
 add hl,de
 ld (hl),a
 inc de
 ld (ResultsPtr),de
 ld a,0
 ld (Result),a
 ret

ListSetup:
 ; entry here: create L1 (delete if exists), put values from
 ;             scratchMem into L1. Num. entries
 ld hl,L1name
 .ifdef Flash
  rst rMov9ToOP1
  rst rFindSym
 .elseifdef TI83
  bcall(_Mov9ToOP1)
  rst rFindSym
 .else
  bcall(_Mov9ToOP1)
  bcall(_FindSym)
 .endif
 jr c,ListDontDelete
 .ifndef TI82
  bcall(_DelVarArc)
 .endif
ListDontDelete:
 ld hl,(ResultsPtr)
 bcall(_createrlist)
 .ifdef TI86
  ; on the TI-86, BDE points to data, we can't just inc de \ inc de
  ;  and call it good. We also need to use an absolute address
  ;  pointer instead of BDE directly for movfrop1
  bcall(_ex_ahl_bde)
  bcall(_inc_ptr_ahl)
  bcall(_inc_ptr_ahl)
  bcall(_set_abs_dest_addr)
 .else
  inc de
  inc de ; skip length info
 .endif
 ld hl,(ResultsPtr)
 ld b,l ; let's not run more than 255 tests
 ld a,b
 cp 0
 jr z,ListEmpty
 ld hl,scratchMem ; start of list in hl
 ListSetValues:
  push bc ; preserve loop counter in b
   push hl ; preserve results pointer?
    push de ; preserve list element pointer, we use this first
     ld a,(hl)
     .ifndef TI82
      bcall(_setxxop1)
     .else
      push hl
       ld h,0
       ld a,l
       bcall(_setxxxxop2)
       bcall(_op2toop1)
      pop hl
     .endif
    pop de
     bcall(_movfrop1) ; put OP1 in list, inc de by 9
   pop hl
   inc hl ; next byte in results
  pop bc
  djnz ListSetValues
ListExit:
 ; if we didn't use appBackupScreen, mark the graph memory as dirty
 .ifdef TI83
  set graphdraw,(iy+graphflags)
 .elseifdef TI86
  set graphdraw,(iy+graphflags)
 .endif
 ret
ListEmpty: ; should never be called ; WILL KILL VAT ON 86 RIGHT NOW
 ld a,0
 push de
 .ifndef TI82
  bcall(_setxxop1)
 .else
  push hl
  ld hl,0
  ; try to find romcall for 0 in op1
  bcall(_setxxxxop2)
  bcall(_op2toop1)
  pop hl
 .endif
 pop de
 ListEmptyLoop:
  push bc
  bcall(_movfrop1)
  pop bc
  djnz ListEmptyLoop
 jr ListExit

L1name:
 .ifdef TI73
  .db 0 ; format????????????
 .elseifdef TI83
  .db ListObj,tVarLst,tL1,0,0
 .elseifdef TI8X
  .db ListObj,tVarLst,tL1,0,0
 .elseifdef TI86
  .db 1,2,"L1",0
 .endif
 
 
ResultsPtr:
 .dw 0 ; index of scratchMem
ScratchWord:
 .dw 0 ; memory we can abuse
Result:
 .dw 0 ; lets us put non-booleans in results list
fralcdcol: ; column to use for fast LCD read test
 .db $25

.ifdef TI83
 ; TI-83 nostub has a squished footer
 .squish
 .db tenter,tend,tenter,t0,t0,t0,t0,tenter,tend
.endif

.end