FUNCTION strrep, str, replace, with
;in a given string STR replace any occurence of REPLACE with WITH
  out = str
  len_rep = strlen(replace)
  len_wth = strlen(with)
  i = -1
  WHILE (i = strpos(out, replace, i+1)) NE -1 DO BEGIN
    len_out = strlen(out)
    out = strmid(out, 0, i)+with+strmid(out, i+len_rep, len_out)
    i += len_wth
  ENDWHILE
  return, out
END
