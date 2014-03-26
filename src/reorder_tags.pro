;+
; NAME:
;  REORDER_TAGS()
;
; PURPOSE:
;  Reorder the tags in a structure
;
; CALLING SEQUENCE:
;  newstruct = reorder_tags(struct, ordered_tagnames)
;
; INPUTS:
;  struct: The original structure. May be an array.
;  ordered_tagnames: Tags to be placed in the front of the struct in the
;    specified order.  Other tags are placed at the end in their original
;    order.
;
; OUTPUTS:
;  A new structure with tags reordered.
;
; Example:
;  st = {a: 35, b: 66.0, c: 25.2, d: [3.5, 2.2]}
;  nst = reorder_tags(st, ['c','a'])
;  print,tag_names(nst)
;    C A B D
;
; MODIFICATION HISTORY:
;  Early 2006: Erin Sheldon, NYU
;-

FUNCTION reorder_tags, struct, ordered_tags_in

  on_error, 2
  IF n_params() LT 2 THEN BEGIN 
      print,'-Syntax: newst = reorder_tags(struct, ordered_tags)'
      print,'Unlisted tags are placed at the end.  Unmatched tags are ignored'
      print
      message,'Halting'
  ENDIF 
  otags = strupcase( ordered_tags_in )
  notags = n_elements(otags)
  otagid = lindgen(notags)

  tags = tag_names(struct)  
  ntags = n_elements(tags)


  match, otags, tags, mo, mt
  IF mo[0] EQ -1 THEN message,'None of the tags matched'

  nmatched = n_elements(mo)

  ;; First build up struct from ordered tags
  s = sort(mo)
  mo = mo[s]
  mt = mt[s]


  FOR i=0L, nmatched-1 DO BEGIN 
      tag = otags[mo[i]]
      value = struct[0].(mt[i])
      IF n_elements(newstruct) EQ 0 THEN BEGIN 
          newstruct = create_struct(tag, value)
      ENDIF ELSE BEGIN 
          newstruct = create_struct(newstruct, tag, value)
      ENDELSE 
  ENDFOR 
  ;; Now tac on the tags that didn't match
  IF nmatched LT ntags THEN BEGIN 
      tunmatched = lindgen(ntags)
      remove, mt, tunmatched
      
      nunmatched = n_elements(tunmatched)
      FOR i=0L, nunmatched-1 DO BEGIN 
          tag = tags( tunmatched[i] )
          value = struct[0].( tunmatched[i] )
          newstruct = create_struct(newstruct, tag, value)
      ENDFOR 
  ENDIF 

  nstruct = n_elements(struct)
  IF nstruct GT 1 THEN BEGIN 
      newstruct = replicate(newstruct,  nstruct)
  ENDIF 

  copy_struct, struct, newstruct

  return, newstruct

END 
