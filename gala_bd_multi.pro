PRO bd_fit, obj_fitstab_file, no_fit=no_fit

;   num = '21_17.346'
;   obj_fitstab_file = '/home/barden/Desktop/multi/BD_objects/t'+num+'_gf.fits'

   fit_info = mrdfits(obj_fitstab_file, 'FIT_INFO', /silent)
   tmp = mrdfits(obj_fitstab_file, 'MODEL_r', model)

   tab = mrdfits(obj_fitstab_file, 'FINAL_BAND')

;   fit_info.initfile = '/home/barden/Desktop/multi/BD_objects/t'+num+'_obj'
;   fit_info.constrnt = '/home/barden/Desktop/multi/BD_objects/t'+num+'_constr'
   
   band_info = mrdfits(obj_fitstab_file, 'BAND_INFO', /silent)
   
   obj_file = strtrim(fit_info.initfile,2)
   constr_file = strtrim(fit_info.constrnt,2)

   band_str = strupcase(strtrim(band_info.band,2))
   nband = n_elements(band_str)

;extract info from SS-fit
   ss_mult = read_sersic_results(obj_fitstab_file, nband)

   openw, filew, obj_file+'_bd', /get_lun
   openr, filer, obj_file, /get_lun
   line = ''
   REPEAT BEGIN
      readf, filer, line
      IF strpos(strtrim(line, 2), 'B) ') EQ 0 THEN BEGIN
         line = 'B) '+strrep(obj_fitstab_file, '.fits', $
                             '_bd.fits    # output file name')
      ENDIF
      IF strpos(strtrim(line, 2), 'G) ') EQ 0 THEN BEGIN
         line = 'G) '+constr_file+'_bd'
      ENDIF
      printf, filew, line
   ENDREP UNTIL strpos(strtrim(line, 2), '# Sersic function') EQ 0

   printf, filew
   printf, filew, ' 0) sersic             # Object type --- DISC'
   x = string(ss_mult.X_GALFIT_BAND, $
                format = '('+string(nband)+'(A,","))')
   x = strtrim(strcompress(x, /remove_all), 2)
   x = ' 1) '+strmid(x, 0, strlen(x)-1)+'  1  band   # position x     [pixel]'
   printf, filew, x
   y = string(ss_mult.Y_GALFIT_BAND, $
                format = '('+string(nband)+'(A,","))')
   y = strtrim(strcompress(y, /remove_all), 2)
   y = ' 2) '+strmid(y, 0, strlen(y)-1)+'  1  band   # position y     [pixel]'
   printf, filew, y
   mag = string(ss_mult.MAG_GALFIT_BAND+2.5*alog10(2), $
                format = '('+string(nband)+'(A,","))')
   mag = strtrim(strcompress(mag, /remove_all), 2)
   mag = ' 3) '+strmid(mag, 0, strlen(mag)-1)+'   9   band    # total magnitude'
   printf, filew, mag
   re = string(ss_mult.RE_GALFIT_BAND, $
                format = '('+string(nband)+'(A,","))')
   re = strtrim(strcompress(re, /remove_all), 2)
   re = ' 4) '+strmid(re, 0, strlen(re)-1)+ $
        '    3   band       #     R_e              [Pixels]'
   printf, filew, re
   n = string(1+fltarr(nband), $
                format = '('+string(nband)+'(A,","))')
   n = strtrim(strcompress(n, /remove_all), 2)
   n = ' 5) '+strmid(n, 0, strlen(n)-1)+ $
        '   0   band       # Sersic exponent (deVauc=4, expdisk=1)'
   printf, filew, n
   q = string(ss_mult.Q_GALFIT_BAND, $
                format = '('+string(nband)+'(A,","))')
   q = strtrim(strcompress(q, /remove_all), 2)
   q = ' 9) '+strmid(q, 0, strlen(q)-1)+'    1   band       # axis ratio (b/a)'
   printf, filew, q
   pa = string(ss_mult.PA_GALFIT_BAND, $
                format = '('+string(nband)+'(A,","))')
   pa = strtrim(strcompress(pa, /remove_all), 2)
   pa = ' 10) '+strmid(pa, 0, strlen(pa)-1)+ $
        '    1   band       # position angle (PA) [Degrees: Up=0, Left=90]'
   printf, filew, pa

   printf, filew, ' Z) 0                  # output image (see above)'

   printf, filew
   printf, filew
   printf, filew, '# Sersic function'
   printf, filew
   printf, filew, ' 0) sersic             # Object type --- BULGE'
   printf, filew, x
   printf, filew, y
   printf, filew, mag
   re = string(ss_mult.RE_GALFIT_BAND*0.75, $
                format = '('+string(nband)+'(A,","))')
   re = strtrim(strcompress(re, /remove_all), 2)
   re = ' 4) '+strmid(re, 0, strlen(re)-1)+ $
        '    3   band       #     R_e              [Pixels]'
   printf, filew, re
   n = string((ss_mult.N_GALFIT_BAND >1.5), $
                format = '('+string(nband)+'(A,","))')
   n = strtrim(strcompress(n, /remove_all), 2)
   n = ' 5) '+strmid(n, 0, strlen(n)-1)+ $
        '   3   band       # Sersic exponent (deVauc=4, expdisk=1)'
   printf, filew, n
   q = string(ss_mult.Q_GALFIT_BAND >0.6, $
                format = '('+string(nband)+'(A,","))')
   q = strtrim(strcompress(q, /remove_all), 2)
   q = ' 9) '+strmid(q, 0, strlen(q)-1)+'    1   band       # axis ratio (b/a)'
   printf, filew, q
   printf, filew, pa

   printf, filew, ' Z) 0                  # output image (see above)'

   maxcomp = 2
   REPEAT BEGIN
      maxcomp++
      a = where(strpos(model,strtrim(maxcomp, 2)+'_PA_R') eq 0, ct)
   ENDREP UNTIL ct LE 0
   maxcomp--
;maxcomp=2 means primary only

   IF maxcomp GT 2 THEN BEGIN
      FOR comp=0, maxcomp-3 DO BEGIN
         printf, filew
         printf, filew
         printf, filew, '# Sersic function'
         printf, filew
         printf, filew, ' 0) sersic             # Object type'

         x = string(tab.(37+comp*3*7), $
                format = '('+string(nband)+'(A,","))')
         x = strtrim(strcompress(x, /remove_all), 2)
         x = ' 1) '+strmid(x, 0, strlen(x)-1)+ $
             '  0  band   # position x     [pixel]'
         printf, filew, x

         y = string(tab.(40+comp*3*7), $
                format = '('+string(nband)+'(A,","))')
         y = strtrim(strcompress(y, /remove_all), 2)
         y = ' 2) '+strmid(y, 0, strlen(y)-1)+ $
             '  0  band   # position y     [pixel]'
         printf, filew, y

         mag = string(tab.(43+comp*3*7), $
                format = '('+string(nband)+'(A,","))')
         mag = strtrim(strcompress(mag, /remove_all), 2)
         mag = ' 3) '+strmid(mag, 0, strlen(mag)-1)+ $
             '   0   band    # total magnitude'
         printf, filew, mag

         re = string(tab.(46+comp*3*7), $
                format = '('+string(nband)+'(A,","))')
         re = strtrim(strcompress(re, /remove_all), 2)
         re = ' 4) '+strmid(re, 0, strlen(re)-1)+ $
             '    0   band       #     R_e              [Pixels]'
         printf, filew, re

         n = string(tab.(49+comp*3*7), $
                format = '('+string(nband)+'(A,","))')
         n = strtrim(strcompress(n, /remove_all), 2)
         n = ' 5) '+strmid(n, 0, strlen(n)-1)+ $
             '   0   band       # Sersic exponent (deVauc=4, expdisk=1)'
         printf, filew, n

         q = string(tab.(52+comp*3*7), $
                format = '('+string(nband)+'(A,","))')
         q = strtrim(strcompress(q, /remove_all), 2)
         q = ' 9) '+strmid(q, 0, strlen(q)-1)+ $
             '    0   band       # axis ratio (b/a)'
         printf, filew, q

         pa = string(tab.(55+comp*3*7), $
                format = '('+string(nband)+'(A,","))')
         pa = strtrim(strcompress(pa, /remove_all), 2)
         pa = ' 10) '+strmid(pa, 0, strlen(pa)-1)+ $
             '    0   band       # position angle (PA) [Degrees: Up=0, Left=90]'
         printf, filew, pa

         printf, filew, ' Z) 0                  # output image (see above)'
      ENDFOR
   ENDIF

   close, filer
   close, filew

;maximum allowed positional offset
   pos_offset = ss_mult.RE_GALFIT_BAND[0]

;constraint file
   openw, ut, constr_file+'_bd', /get_lun

   printf, ut, '# Component/    parameter   constraint  Comment'
   printf, ut, '# operation                  values'

   FOR j=2, 2 do begin ;maxcomp+1 DO BEGIN
      printf, ut, '           '+strtrim(j, 2)+' n 0.2 to 8'
      printf, ut, '           '+strtrim(j, 2)+' re 0.3 to 400'
      printf, ut, '           '+strtrim(j, 2)+' q 0.0001 to 1'
      printf, ut, '           '+strtrim(j, 2)+' mag -5 5'
      printf, ut, '           '+strtrim(j, 2)+' mag 0 to 40'
      printf, ut, '           '+strtrim(j, 2)+' pa -360 to 360'
      printf, ut, '           '+strtrim(j, 2)+' x '+ $
              strtrim(-pos_offset*0.5, 2)+ $
              ' '+ strtrim(pos_offset*0.5, 2)
      printf, ut, '           '+strtrim(j, 2)+' y '+ $
              strtrim(-pos_offset*0.5, 2)+ $
              ' '+ strtrim(pos_offset*0.5, 2)
   ENDFOR
   printf, ut
   printf, ut, '           2-3 x 0 0'
   printf, ut, '           2-3 y 0 0'

   printf, ut
   free_lun, ut


;run galfit
   IF NOT keyword_set(nofit) THEN BEGIN
      IF keyword_set(nice) THEN spawn, 'nice '+galfit_exe+' '+obj_file $
      ELSE spawn, galfit_exe+' '+obj_file
   ENDIF

;check output (and rerun)
stop
END
