PRO tiling_helper, cat
; .run tiling_helper
; tiling_helper, '/mnt/users/haeussler/CANDELS/galapagos/mwl_cosmos_30mas_all_detect/table_before_start.sav'
  restore, cat
  
  print, 'found a total of '+strtrim(n_elements(sexcat.frame),2)+' objects'
  print, '  '
  
; find UNIQUE file names
  names = sexcat.frame
  names = names[uniq(names,sort(names))]
  
; go through all filenames and find number of objects to be fitted.
  total = 0
  FOR i=0,n_elements(names)-1 DO BEGIN
     print, 'image '+names[i]+' : '+strtrim(n_elements(where(sexcat.frame EQ names[i])),2)+' objects'
     total += n_elements(where(sexcat.frame EQ names[i]))
  ENDFOR
END
