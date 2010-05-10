;+
; NAME: 
;	DATATYPE()
; PURPOSE: 
;	Returns the data type of a variable.
; EXPLANATION: 
;	This routine returns the data type of a variable in a format specified
;	by the optional flag parameter.
; CALLING SEQUENCE         : 
;	Result = DATATYPE( VAR  [, FLAG ] )
; INPUTS: 
;	VAR	= Variable to examine.
; OPTIONAL INPUT PARAMETERS: 
;	FLAG	= Output format flag as explained below.  The default is 0.
; OUTPUT PARAMETRS: 
;	The result of the function is the either a string or integer giving the
;	data type of VAR.  Depending on the value of FLAG, the result will be
;	one of the values from the following table:
;
;		FLAG = 0       FLAG = 1           FLAG = 2       FLAG = 3
;
;		UND            Undefined          0              UND
;		BYT            Byte               1              BYT
;		INT            Integer            2              INT
;		LON            Long               3              LON
;		FLO            Float              4              FLT
;		DOU            Double             5              DBL
;		COM            Complex            6              COMPLEX
;		STR            String             7              STR
;		STC            Structure          8              STC
;		DCO            DComplex           9              DCOMPLEX
;		PTR            Pointer           10              POINTER
;		OBJ            Object            11              OBJECT
;
; OPTIONAL INPUT KEYWORD: 
;	HELP	= If set, then a short explanation is printed out.
; RESTRICTIONS: 
;	FLAG, if passed, must be an integer between 0 and 3.
;
; Copyright (C) 1985, Johns Hopkins University/Applied Physics Laboratory
; This software may be used, copied, or redistributed as long as it is not
; sold and this copyright notice is reproduced on each copy made.  This
; routine is provided as is without any express or implied warranties
; whatsoever.  Other limitations apply as described in the file disclaimer.txt.
;
; REVISION HISTORY: 
;	R. Sterner, JHU/APL, 24 October 1985.
;       R. Sterner, 18 Mar, 1993 --- Added /DESCRIPTOR.
;       R. Sterner, 1995 Jul 24 --- Added DCOMPLEX for data type 9.
;       D. Zarro,   1997 Jul 19 --- Added PTR and OBJ for data type 10 and 11.
;-
;-------------------------------------------------------------
 
	function datatype,var, flag0, descriptor=desc, help=hlp
 
	if (n_params(0) lt 1) or keyword_set(hlp) then begin
	  print,' Datatype of variable as a string (3 char or spelled out).'
	  print,' typ = datatype(var, [flag])'
	  print,'   var = variable to examine.         in'
	  print,'   flag = output format flag (def=0). in'
	  print,'   typ = datatype string or number.   out'
	  print,'      flag=0    flag=1      flag=2    flag=3'
	  print,'      UND       Undefined   0         UND'
	  print,'      BYT       Byte        1         BYT'
	  print,'      INT       Integer     2         INT'
	  print,'      LON       Long        3         LON'
	  print,'      FLO       Float       4         FLT'
	  print,'      DOU       Double      5         DBL'
	  print,'      COM       Complex     6         COMPLEX'
	  print,'      STR       String      7         STR'
	  print,'      STC       Structure   8         STC'
	  print,'      DCO       DComplex    9         DCOMPLEX'
	  print,'      PTR       Pointer    10         POINTER'
	  print,'      OBJ       Object     11         OBJECT'
	  print,' Keywords:'
	  print,'   /DESCRIPTOR returns a descriptor for the given variable.'
 	  print,'     If the variable is a scalar the value is returned as'
 	  print,'     a string.  If it is an array a description is return'
 	  print,'     just like the HELP command gives.  Ex:'
 	  print,'     datatype(fltarr(2,3,5),/desc) gives'
 	  print,'       FLTARR(2,3,5)  (flag always defaults to 3 for /DESC).'
	  return, -1
	endif 
 
	if n_params(0) lt 2 then flag0 = 0	; Default flag.
	flag = flag0				; Make a copy.
 
	if n_elements(var) eq 0 then begin
	  s = [0,0]
	endif else begin
	  s = size(var)
	endelse
 
	if keyword_set(desc) then flag = 3
 
	if flag eq 2 then typ = s(s(0)+1)
 
	if flag eq 0 then begin
	  case s(s(0)+1) of
   0:	    typ = 'UND'
   1:       typ = 'BYT'
   2:       typ = 'INT'
   4:       typ = 'FLO'
   3:       typ = 'LON'
   5:       typ = 'DOU'
   6:       typ = 'COM'
   7:       typ = 'STR'
   8:       typ = 'STC'
   9:       typ = 'DCO'
  10:       typ = 'PTR'
  11:       typ = 'OBJ'

else:       print,'Error in datatype'
	  endcase
	endif else if flag eq 1 then begin
	  case s(s(0)+1) of
   0:	    typ = 'Undefined'
   1:       typ = 'Byte'
   2:       typ = 'Integer'
   4:       typ = 'Float'
   3:       typ = 'Long'
   5:       typ = 'Double'
   6:       typ = 'Complex'
   7:       typ = 'String'
   8:       typ = 'Structure'
   9:       typ = 'DComplex'
  10:       typ = 'Pointer'
  11:       typ = 'Object'
else:       print,'Error in datatype'
	  endcase
	endif else if flag eq 3 then begin
	  case s(s(0)+1) of
   0:	    typ = 'UND'
   1:       typ = 'BYT'
   2:       typ = 'INT'
   4:       typ = 'FLT'
   3:       typ = 'LON'
   5:       typ = 'DBL'
   6:       typ = 'COMPLEX'
   7:       typ = 'STR'
   8:       typ = 'STC'
   9:       typ = 'DCOMPLEX'
  10:       typ = 'POINTER'
  11:       typ = 'OBJECT'
else:       print,'Error in datatype'
	  endcase
	endif
 
	if not keyword_set(desc) then begin
	  return, typ					; Return data type.
	endif else begin
	  if s(0) eq 0 then return,strtrim(var,2)	; Return scalar desc.
	  aa = typ+'ARR('
          for i = 1, s(0) do begin                      
            aa = aa + strtrim(s(i),2)                 
            if i lt s(0) then aa = aa + ','          
            endfor                                     
          aa = aa+')'                                   
	  return, aa
	endelse
 
	end
