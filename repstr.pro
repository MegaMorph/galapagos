function repstr,obj,in,out
;+
; NAME:
;	REPSTR
; PURPOSE:
;	Replace all occurences of one substring by another.
; EXPLANATION:
;	Meant to emulate the string substitution capabilities of text editors
; CALLING SEQUENCE:
;	result = repstr( obj, in, out )
;
; INPUT PARAMETERS:
;	obj    = object string for editing, scalar or array
;	in     = substring of 'obj' to be replaced, scalar 
;
; OPTIONAL INPUT PARMETER:
;	out    = what 'in' is replaced with, scalar.   If not supplied
;		then out = '', i.e. 'in' is not replaced by anything. 
;
; OUTPUT PARAMETERS:
;	Result returned as function value.  Input object string
;	not changed unless assignment done in calling program.
;
; PROCEDURE:
;	Searches for 'in', splits 'obj' into 3 pieces, reassembles
;	with 'out' in place of 'in'.  Repeats until all cases done.
;
; EXAMPLE:
;	If a = 'I am what I am' then print,repstr(a,'am','was')
;	will give 'I was what I was'.
;
; MODIFICATION HISTORY:
;	Written by Robert S. Hill, ST Systems Corp., 12 April 1989.
;	Accept vector object strings, W. Landsman   HSTX,   April, 1996
;	Converted to IDL V5.0   W. Landsman   September 1997
;       Convert loop to LONG, vectorize STRLEN call W. Landsman June 2002
;       Correct bug in optimization, case where STRLEN(OBJ) EQ
;         STRLEN(IN), C. Markwardt, Jan 2003
;-
 On_error,2
 if N_params() LT 3 then begin
	print,'Syntax - result = REPSTR( obj, in, out )'
	return, obj
 endif

 if N_elements(out) EQ 0 then out = ''
 l1 = strlen(in)
 l2 = strlen(out)
 Nstring = N_elements(obj)
 object = obj
 lo = strlen(object) - l1             ;Last character needed to look at 
 for i= 0L ,Nstring-1 do begin
 last_pos = 0
 pos = 0
 while ( pos LE lo[i]) and (pos GE 0) do begin
   pos = strpos(object[i],in,last_pos)
   if (pos GE 0) then begin
	      first_part = strmid(object[i],0,pos)
	      last_part  = strmid(object[i],pos+l1,9999)
	      object[i] = first_part + out + last_part
              lo[i] = lo[i]+(l2-l1)
   endif 
  last_pos = pos + l2
 endwhile
 endfor

 return,object

 end
