FUNCTION round_digit, number, digit, SIGNIFICANT = significant, $
                 FIRST = first, string = string, space = space, L64=L64
;round number after digit
;SIGNIFICANT: keep DIGIT digits significant (5.532 -> 5.5, 2
;significant digits or 234567.89 -> 230000.00, 2 significant digits)
  n = n_elements(number)
  digita = fltarr(n)
  FOR i = 0, n-1 DO BEGIN
    IF keyword_set(significant) THEN BEGIN
      log = alog10(abs(number[i]))
      IF log LE 0 THEN a = 1 ELSE a = 0
      digita[i] = digit-fix(log)-1+a
    ENDIF ELSE digita = digit
  ENDFOR
  IF keyword_set(first) THEN digita = digita[0]
  result = round(number*10.^digita, L64=L64)/10.^digita
  IF keyword_set(string) THEN BEGIN
    result = strtrim(string(result), 2)
    FOR i = 0, n_elements(result)-1 DO BEGIN
      result[i] = strtrim_str(string(result[i]), '0', 1)
      pos = strpos(result[i], '.')
      len = strlen(result[i])
      IF len-pos LE digit THEN BEGIN
        FOR j = 0, digit-len+pos DO result[i] = result[i]+'0'
      ENDIF
      pos = strpos(result[i], '.')
      len = strlen(result[i])-1
      IF pos EQ len THEN result[i] = strmid(result[i], 0, pos)
;      print, pos, len, len-pos, '   ', result[i]
      IF keyword_set(space) THEN result[i] = ' '+result[i]+' '
    ENDFOR
  ENDIF
  return, result
END
