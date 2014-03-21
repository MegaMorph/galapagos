FUNCTION invert_index, array, idx
;given an index (idx) of an array, find the inverse of the index,
;i.e. an index of all elements of array that are not listed in the
;input index
   n = n_elements(array)
   IF n EQ n_elements(idx) AND total(lindgen(n)) EQ total(idx) THEN out = -1 $
   ELSE BEGIN
      out = 0
      FOR i = 0l, long(n-1) DO BEGIN
         dum = where(i EQ idx, count)
         IF count EQ 0 THEN out = [out, i]
      ENDFOR
   ENDELSE
   IF n_elements(out) EQ 1 THEN out = -1 $
   ELSE out = out[1:n_elements(out)-1]
   return, out
END
