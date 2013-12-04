;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;+
;
; NAME:
;    REMOVE_TAGS()
;       
; PURPOSE:
;    remove the specified tags from input structure
;
; CALLING SEQUENCE:
;    newstruct = remove_tags(oldstruct, tagnames)
;
; INPUTS: 
;    oldstruct: the original structure
;    tagnames: the names of tags to be removed (can be an array)
;
; OUTPUTS: 
;    newstruct: the new structure without tags.
;
; CALLED ROUTINES:
;    MATCH
;    
; PROCEDURE: 
;    
;       
;
; REVISION HISTORY:
;    ????? Judith Racusin
;    25-OCT-2000 Modified to handle arbitrary tag types. Also error 
;          handling. Erin Scott Sheldon
;    2006-May-31: Converted to a function. Return -1 when all tags removed.
;       
;                                      
;-                                       
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

FUNCTION remove_tags, struct, tagnames

  IF n_params() EQ 0 THEN BEGIN 
      print,'Syntax - newstruct = remove_tags(struct, tagnames)'
      print
      print,'Use doc_library,"remove_tags"  for more help.'  
      return, -1
  END

  ;; Figure out which tags get removed

  tags=tag_names(struct)
  n=n_elements(tags)
  tagnames=strupcase(tagnames)
  nt=n_elements(tagnames)
  IF nt EQ 1 THEN BEGIN
      t=where(tags NE tagnames[0],nw) 
      IF nw EQ n THEN BEGIN
          message,'Tag did not match, structure unchanged',/inf
          newstruct = struct
          return, newstruct
      ENDIF 
  ENDIF ELSE BEGIN 
      match,tags,tagnames,m
      IF m[0] EQ -1 THEN BEGIN
          message,'No tags matched, structure unchanged',/inf
          newstruct=struct
          return, newstruct
      ENDIF 
      nm=n_elements(m)
      IF nm EQ n THEN BEGIN 
          message,'All tags removed.  Returning -1',/inf
          return, -1
      ENDIF 
      t=lindgen(n)
      remove, m, t
  ENDELSE 
      
  ;; create new structure
  tags=tags[t]
  n=n_elements(tags)

  newstruct=create_struct(tags[0],struct[0].(t[0]))
  
  FOR i=1L, n-1 DO newstruct = create_struct(temporary(newstruct), $
                                             tags[i], struct[0].(t[i]) )

  newstruct=replicate( temporary(newstruct), n_elements(struct) )
  struct_assign,struct,newstruct

  return, newstruct
END

