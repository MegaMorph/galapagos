;-------------------------------------------------------------
;+
; NAME:
;       HLP
; PURPOSE:
;       Variant of HELP.  Gives array min, max.
; CATEGORY:
; CALLING SEQUENCE:
;       hlp, a1, [a2, ..., a9]
; INPUTS:
;       a1, [...] = input variables.    in
; KEYWORD PARAMETERS:
; OUTPUTS:
; COMMON BLOCKS:
; NOTES:
; MODIFICATION HISTORY:
;       R. Sterner, 11 Dec, 1989
;
; Copyright (C) 1989, Johns Hopkins University/Applied Physics Laboratory
; This software may be used, copied, or redistributed as long as it is not
; sold and this copyright notice is reproduced on each copy made.  This
; routine is provided as is without any express or implied warranties
; whatsoever.  Other limitations apply as described in the file disclaimer.txt.
;-
;-------------------------------------------------------------

pro hlp, a1, a2, a3, a4, a5, a6, a7, a8, a9, help=hlp
  
  if (n_params(0) lt 1) or keyword_set(hlp) then begin
    print,' Variant of HELP.  Gives array min, max.'
    print,' hlp, a1, [a2, ..., a9]'
    print,'   a1, [...] = input variables.    in'
    return
  endif
  
  np = n_params(0)              ; How many args?
  for k = 1, np do begin        ; Loop through all args.
    txt = 'i = n_elements(a'+strtrim(k,2)+')' ; Test if ai defined.
    tmp = execute(txt)
    if i eq 0 then begin
      print,k,'   Undefined'
      goto, skip
    endif
    txt = 'a = a'+strtrim(k,2)  ; Set up a = ai
    tmp = execute(txt)          ; Put ai in a.
    
    t = datatype(a,1)
    f = isarray(a)
    
    aa = ''
    if f eq 1 then begin
      sz = size(a)
      aa = ' array ('
      for i = 1, sz(0) do begin
        aa = aa + strtrim(sz(i),2)
        if i lt sz(0) then aa = aa + ', '
      endfor
      aa = aa+').'
      if (t ne 'String') and (t ne 'Complex') then begin
        if t eq 'Byte' then begin
          mn = min(fix(a),max=mx)
        endif else begin
          mn = min(a,max=mx)
        endelse
        mn = strtrim(mn,2)
        mx = strtrim(mx,2)
        aa = aa + '   Min = '+mn+',  Max = '+mx
      endif
    endif else begin
      if t ne 'Undefined' then aa = ' = '+strtrim(a,2)
    endelse
    
    print,k,'   '+t+aa
skip: 
  endfor                        ; k
  
  return
end
