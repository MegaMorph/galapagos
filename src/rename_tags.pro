;+
; NAME:
;  RENAME_TAGS()
;
; PURPOSE:
;  Rename tags in a structure
;
; CALLING SEQUENCE:
;  newstruct = rename_tags(struct, oldtagnames, newtagnames)
;
; INPUTS:
;  struct: The original structure. May be an array.
;  oldtagnames: names of tags to change
;  newtagnames: new names for tags
;
; OUTPUTS:
;  A new structure with tags renamed.
;
; MODIFICATION HISTORY:
;  Early 2006: Erin Sheldon, NYU
;-

FUNCTION rename_tags, struct, oldtags_in, newtags_in

  IF n_params() LT 3 THEN BEGIN 
      print,'-Syntax: newstruct = rename_tags(struct, oldtags, newtags)'
      return, -1
  ENDIF 

  oldtags = strupcase(oldtags_in)
  newtags = strupcase(newtags_in)

  nst = n_elements(struct)
  nold = n_elements(oldtags)
  nnew = n_elements(newtags)

  IF nold NE nnew THEN BEGIN 
      print,'# of old tags ne # of new tags'
      return, -1
  ENDIF 

  tags = tag_names(struct)
  ntags = n_elements(tags)

  ;; create new structure
  FOR i=0L, ntags-1 DO BEGIN 

      w=where(oldtags EQ tags[i], nw)
      
      IF nw NE 0 THEN BEGIN 
          taguse = newtags[w[0]]
          print,'Changing tag "'+tags[i]+'" to "'+taguse+'"'
      ENDIF ELSE BEGIN 
          taguse = tags[i]
      ENDELSE 
      
      IF i EQ 0 THEN BEGIN 
          newst = create_struct(taguse, struct[0].(i))
      ENDIF ELSE BEGIN 
          newst = create_struct(newst, taguse, struct[0].(i))
      ENDELSE 

  ENDFOR 
  
  ;; copy in the values
  newstruct = replicate(newst, nst)
  FOR i=0L, ntags-1 DO BEGIN 
      newstruct.(i) = struct.(i)
  ENDFOR 

  return, newstruct
END 


