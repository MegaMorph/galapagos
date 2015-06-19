;requires pros from galapagos

PRO sex2ds9reg, sexcat, param, regfile, radius, num=num, $
                color=color, tag=tag, add_column=add_column
;add_column: 2d string array: [[label0, value0],[label1, value1],...]
;e.g.:       add_col = ['FILE', '" "']
;num does not work together with add_column - number has preference
;add_column does not work together with tag - add_column has preference

;sex2ds9reg, '/data/data2/stages/gala/cat/sexcomb', '/data/data2/stages/gala/a901-01/a01.outparam', '/data/data2/stages/gala/cat/comb.reg', 0.75, add_col = ['FILE', '" "']
;sex2ds9reg, '/data/data2/stages/gala/a901-01/a01.outcat', '/data/data2/stages/gala/a901-01/a01.outparam', '/data/data2/stages/gala/a901-01/a01.reg', 0.5, tag = '01'

   tab = read_sex_table(sexcat, param, add_column=add_column)
   print, 'catalogue contains a total of '+strtrim(n_elements(tab.alpha_J2000),2)+' objects '

   openw, 1, regfile
   printf, 1, '# Region file format: DS9 version 3.0'
   printf, 1, '# Filename: '+sexcat

   IF n_elements(color) GT 0 THEN col = color ELSE col = 'green'
   printf, 1, 'global color='+col+' font="helvetica 10 normal" ' + $
           'select=1 edit=1 move=1 delete=1 include=1 fixed=0 source'

   n = n_elements(tab)

   IF n_elements(add_column) GT 0 THEN BEGIN
      tag = strarr(n)
      FOR i=0ul, n-1 DO $
       tag[i] = strmid(tab[i].file, $
                       strpos(tab[i].file, '/', /reverse_search)+1, $
                       strlen(tab[i].file))
   ENDIF

   IF n_elements(tag) GT 0 THEN tagstr = ' # tag={'+tag+'}' ELSE tagstr = ''

   IF keyword_set(num) THEN BEGIN
      tagstr = strarr(n)
      FOR i=0ul, n-1 DO tagstr[i] = ' # tag={'+strtrim(tab[i].number, 2)+'}'
      IF n_elements(add_column) EQ 0 THEN add_column = 1
   ENDIF
   
   IF n_elements(add_column) GT 0 THEN $
    FOR i=0ul, n-1 DO $
     printf, 1, 'fk5;circle('+strtrim(tab[i].alpha_j2000, 2)+','+ $
             strtrim(tab[i].delta_j2000, 2)+','+strtrim(radius, 2)+'p)'+ $
             tagstr[i] $
   ELSE $
    FOR i=0ul, n-1 DO $
     printf, 1, 'fk5;circle('+strtrim(tab[i].alpha_j2000, 2)+','+ $
             strtrim(tab[i].delta_j2000, 2)+','+strtrim(radius, 2)+'p)'+ $
             tagstr

   close, 1
END
