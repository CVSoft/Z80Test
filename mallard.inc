#include "ti73.inc"

;Included for TASM Compatibility
;-------------------------------
#define equ .equ
#define EQU .equ
#define end .end
#define END .end

;SAFERAM
saferam1 .equ appBackUpScreen	;768 bytes free for use
saferam2 .equ savesscreen		;768 bytes - APD mem
saferam3 .equ tempswaparea	;~232 bytes or so.
saferam4 .equ OP1
saferam5 .equ imathptr1		

;SHELL EQUATES
Version .equ statVars
Random .equ statVars+3
PutSprite .equ statVars+6
LargeSprite .equ statVars+9
GetPixel .equ statVars+12
Decompress .equ statVars+15
FastCopy .equ statVars+18
LevelDetect .equ statVars+21

;Equates to allow 83+ ION programs to be easily recompiled for Mallard.
ionVersion .equ Version
ionRandom .equ Random
ionPutSprite .equ PutSprite
ionLargeSprite .equ LargeSprite
ionGetPixel .equ GetPixel
ionDecompress .equ Decompress
ionFastCopy .equ FastCopy
ionDetect .equ LevelDetect