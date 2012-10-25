PRO bd_fit, obj_fitstab_file, no_fit=no_fit

;   num = '21_17.346'
;   obj_fitstab_file = '/home/barden/Desktop/multi/BD_objects/t'+num+'_gf.fits'

   fit_info = mrdfits(obj_fitstab_file, 'FIT_INFO', /silent)

   tab = mrdfits(obj_fitstab_file, 'FINAL_BAND')

;   fit_info.initfile = '/home/barden/Desktop/multi/BD_objects/t'+num+'_obj'
;   fit_info.constrnt = '/home/barden/Desktop/multi/BD_objects/t'+num+'_constr'
   
   band_info = mrdfits(obj_fitstab_file, 'BAND_INFO', /silent)
   
   obj_file = strtrim(fit_info.initfile,2)
   constr_file = strtrim(fit_info.constrnt,2)

   band_str = strupcase(strtrim(band_info.band,2))
   nband = n_elements(band_str)

   tmp = mrdfits(obj_fitstab_file, 'MODEL_'+band_str[0], model)

;extract info from SS-fit
   forward_function read_sersic_results
   ss_mult = read_sersic_results(obj_fitstab_file, nband)
   
;change path from boris on dator to ppzsb1 on dator or supercomputer
   obj_file = strrep(obj_file, 'boris', 'ppzsb1')
   constr_file = strrep(constr_file, 'boris', 'ppzsb1')
   ;obj_file = strrep(obj_file, '/home/boris', '/work/work1/ppzsb1')
   ;constr_file = strrep(constr_file, '/home/boris', '/work/work1/ppzsb1')

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

   free_lun, filer
   free_lun, filew

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
   IF NOT keyword_set(no_fit) THEN BEGIN
      IF keyword_set(nice) THEN spawn, 'nice '+galfit_exe+' '+obj_file $
      ELSE spawn, galfit_exe+' '+obj_file
   ENDIF
END

PRO run_bd_fit, data_table_file
;data_table = '/eg/path/to/GAMA_9_ffvqqff_gama_only.fits
   data_table = mrdfits(data_table_file, 1)

   FOR i=0l, n_elements(data_table)-1 DO BEGIN
      obj_fitstab_file = strtrim(data_table[i].file_galfit, 2)
      ;change path from boris on dator to ppzsb1 on dator or supercomputer
      obj_fitstab_file = strrep(obj_fitstab_file, 'boris', 'ppzsb1')
      ;obj_fitstab_file = strrep(obj_fitstab_file, '/home/boris', '/work/work1/ppzsb1')

      bd_fit, obj_fitstab_file, /no_fit
   ENDFOR
END

PRO create_batches, n_cores, data_table_file, galexe_str, outdir, outfile
   data_table = mrdfits(data_table_file, 1)

   batch = 0
   FOR i=0l, n_elements(data_table)-1 DO BEGIN
      IF i MOD n_cores EQ 0 THEN BEGIN
         IF batch GT 0 THEN BEGIN
            printf, lun, 'echo "Finished job now"'
            free_lun, lun
         ENDIF
         batch++
         openw, lun, outdir+'/'+outfile+strtrim(batch, 2), /get_lun

         printf, lun, '#!/bin/bash'
         printf, lun, '# This is a submit script for B/D fitting.'
         printf, lun, '# OPTIONS FOR GRID ENGINE =============================================================='
         printf, lun, '#$ -l h_rt=10:00:00'
         printf, lun, '# This specifies the job should run for no longer than 10 hour'
         printf, lun, '#$ -cwd'
         printf, lun, '# This sends output into the directory from which you submitted'
         printf, lun, '#$ -j y'
         printf, lun, '# This joins up the error and output into one file rather that making two files'
         printf, lun, '#$ -o '+outfile+'.out'
         printf, lun, '# This send your output to the file "'+outfile+'.out" rather than a standard grid engine output filename'
         printf, lun, '# OPTIONS FOR GRID ENGINE================================================================='
         printf, lun, '# Here we just use Unix command to run our program'
         printf, lun, 'echo "Running on `hostname`"'
         printf, lun, 'cd /work/work1/ppzsb1/'
         printf, lun, '# edit above line to the correct working directory for your job'
         printf, lun, '# do something here'
      ENDIF

      obj_file = strtrim(data_table[i].initfile, 2)+'_bd'
      cmd_str = galexe_str+' '+obj_file

      IF obj_file NE '_bd' THEN printf, lun, cmd_str
   ENDFOR
   printf, lun, 'echo "Finished job now"'

   free_lun, lun
END

; PRO extract_bd_info, data_table_file, band_str, out_fits_table, version_num
; ;bands in band_str have to be in the proper order!

;    setup = {version:0}
;    setup.version = version_num

;    nband = n_elements(band_str)

;    data_table = mrdfits(data_table_file, 1)

;    FOR i=0l, n_elements(data_table) DO BEGIN
;       obj_file = strtrim(data_table[i].initfile, 2)+'_bd'
;       IF obj_file EQ '_bd' THEN CONTINUE

;       forward_function read_sersic_results
;       bd_table = read_sersic_results(obj_file, nband, /bd)

;       str = strtrim(data_table[i].initfile, 2)
;       sky_file = strmid(str, 0, strpos(str, '_obj')+'_'+['', band_str]

;       update_table, bd_table, i, out_fits_table, obj_file, sky_file, nband, setup, /final, /bd
;    ENDFOR

; END
