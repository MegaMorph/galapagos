;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;+
;
; NAME:
;    ADD_TAGS
;       
; PURPOSE:
;    Add new tags to the structure. Similar to 
;    add_tag, but with these major differences:
;       --Unlike the program ADD_TAG, ADD_TAGS can add
;         multiple new tags.
;       --Unlike add_tag, which takes an actual
;         value for the new tag, ADD_TAGS takes in string 
;         indicators for each new tag, such as 'fltarr(3)', 
;         or '10', and uses MRD_STRUCT
;       --Because of this, ADD_TAGS cannot add specific values
;         for non-scalars.
;       --Also because of this, ADD_TAGS cannot add a substructure.
;         If you want to add a substructure to a structure, use
;         ADD_TAG: add_tag, oldstruct, tagname, substruct, newstruct 
;
; CALLING SEQUENCE:
;    add_tags, oldstruct, tagnames, values, newstruct
;
; INPUTS: 
;    oldstruct: The original structure (or array of structures)
;    tagnames: new tag name(s), can be an array
;    values: string containing values for tagnames. must be same size
;            array as tagnames. Same format as MRD_STRUCT.PRO
; OPTIONAL INPUTS:
;    NONE
;
; KEYWORD PARAMETERS:
;    structyp: a string with the name of the new structure.
;     if already defined the program will crash.
;       
; OUTPUTS: 
;    newstruct: The structure with the new tags in it.
;
; OPTIONAL OUTPUTS:
;    NONE
;
; EXAMPLE: 
;    tagnames=['ra', 'dec', 'image', 'name']
;    values  =['0d', '0d',  'intarr(1000, 1000)', "'NGC3035'"]
;    add_tags, oldstruct, tagnames, values
;
; CALLED ROUTINES:
;    MRD_STRUCT
;    DATATYPE
;    COMBINE_STRUCTS
; 
; PROCEDURE: 
;    Use mrd_struct to create a new structure.
;	
;
; REVISION HISTORY:
;    25-OCT-2000, Erin Scott Sheldon
;       
;                                      
;-                                       
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

PRO add_tags, struct, tagnames, values, newstr, structyp=structyp

  IF n_params() LT 3 THEN BEGIN 
      print,'Syntax - add_tags, struct, tagnames, values, newstr, structyp=structyp'
      print,'Use doc_library,"add_tags"  for more help.'  
      return
  END
  
  newstr=0
  nt=n_elements(tagnames)
  nv=n_elements(values)
  IF nt NE nv THEN BEGIN 
      print,'Number of tagnames not equal to number of tag values'
      return
  ENDIF 
  IF datatype(tagnames) NE 'STR' THEN BEGIN
      print,'tagnames must be a string array'
      return
  ENDIF 
  IF datatype(values) NE 'STR' THEN BEGIN
      print,'values must be a string array'
      return
  ENDIF 

  n_struct = n_elements(struct)

  tmpstr = mrd_struct(tagnames, values, n_struct)
  IF datatype(tmpstr) EQ 'INT' THEN BEGIN 
      print,'Error: MRD_STRUCT exited with error'
      return
  ENDIF 
  combine_structs,struct,temporary(tmpstr),newstr, structyp=structyp

  return
END
