PRO add_tag, struct, tagname, tagtype, newstr, structyp=structyp

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;+
;
; NAME:
;    ADD_TAG
;       
; PURPOSE:
;    Add a new tag to the structure. NOTE: if you want to add more
;    than one tag at once, use ADD_TAGS
;
; CALLING SEQUENCE:
;    add_tag, oldstruct, tagname, tagtype, newstruct, structyp=structyp
;
; INPUTS: 
;    oldstruct: The original structure (or array of structures)
;    tagname: string containing the new tag name
;    tagtype: the initial value of the new tag, e.g. fltarr(5)
;           or [3,5,6], or 0L, etc.
;
; KEYWORD PARAMETERS:
;   structyp: a string with the name of the new structure.
;     if already defined the program will crash.
;
; OUTPUTS: 
;    newstruct: The structure with the new tag it it.
;
; OPTIONAL OUTPUT
;    NONE
;
; CALLED ROUTINES:
;    COMBINE_STRUCTS
; 
; PROCEDURE: 
;    
;       
;
; REVISION HISTORY:
;    25-OCT-2000, Judith Racusin.
;       
;                                      
;-                                       
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  IF n_params() LT 3 THEN BEGIN 
      print,'Syntax - add_tag,struct, tagname, tagtype, newstr, structyp=structyp'
      print,'Use doc_library,"add_tag"  for more help.'  
      return
  END

  t=tag_names(struct)
  w=where(t EQ strupcase(tagname),nw)
  IF nw NE 0 THEN BEGIN 
      print,'Tag ',tagname,' Already Exists'
      return
  END

  tmpstr=create_struct(tagname,tagtype)
  tmpstr=replicate(tmpstr,n_elements(struct))
  combine_structs,struct,temporary(tmpstr),newstr, structyp=structyp


  return
END

