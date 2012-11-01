; The procedures in this file are standalone scripts for running
; GALFIT bulge-disk fits on the University of Nottingham supercomputer
; following a single-sersic run of GALAPAGOS-2.
;
; These procedures use routines from galapagos, so that must be
; compiled, e.g.
; .r galapgaos
; .r gala_bd_multi
;
; First of all the GALFIT feedme files must be generated, using
; 'run_bd_fit'. This also outputs two additional files:
; 'bd*_batch_file', which lists all the feedme files created, and
; 'bd*_rsync_includes', which is an rsync includes file (used later).
; 
; run_bd_fit is given the filename of the GALAPAGOS catalogue to be used.
;
; run_bd_fit is also given a label, in order to allow multiple versions of
; B/D fitting scripts to coexist.
; 
; A selection may be applied in run_bd_fit, e.g., to select only
; objects in a particular magnitude range, but this is currently
; hardcoded.
;
; The details of the initial conditions (i.e., how they are computed
; from the results of the single-sersic fits) and the fitting
; parameters (i.e., which parameters are allowed to vary, wavelengths,
; band labels, etc.) are all hardcoded in run_bd_fit.
;
; This is an example of running run_bd_fit:
; run_bd_fit, 'gama/galapagos/galapagos_2.0.3_galfit_0.1.2.1_GAMA_9/GAMA_9_ffvqqff_gama_only.fits', '1'
;
; Once the files have been created, they, and the corresponding data,
; need to be transferred to the supercomputer.  For this we use rsync,
; supplied with the corresponding rsync includes file.
; run_bd_fit prints an appropriate command upon completion.
; For example:
; rsync -av --prune-empty-dirs --include-from=bd_rsync_includes
;       gama/galapagos/galapagos_2.0.3_galfit_0.1.2.1_GAMA_9/
;       jupiter:/work/work1/ppzsb1/megamorph/gama/galapagos/galapagos_2.0.3_galfit_0.1.2.1_GAMA_9/
; The --prune-empty-dirs is not strictly necessary, and may slow down
; the transfer.
;
; Next the qsub submission scripts must be created.  This is done
; using the 'create_batches' procedure, which is given the maximum
; number of cores to utlise, the filename of the batch file output by
; run_bd_fit, the name of the galfit executable to use (on jupiterthe
; supercomputer), the path to put the submission scripts at, and the
; stub for naming them.
; For example:
; create_batches, 64, '/home/ppzsb1/bd_batch_file', 'galfitm-0.1.2.1', 'gama/galapagos/galapagos_2.0.3_galfit_0.1.2.1_GAMA_9/batches', 'gala_gama_bd1_'
;
; These batch files need to be transferred to the supercomputer:
; rsync -av gama/galapagos/galapagos_2.0.3_galfit_0.1.2.1_GAMA_9/batches
;        jupiter /work/work1/ppzsb1/megamorph/gama/galapagos/galapagos_2.0.3_galfit_0.1.2.1_GAMA_9/
;
; Finally, one can ssh into a supercomputer head node, navigate to the
; batches path, and submit the jobs:
; ls gala_gama_bd1* | xargs -L1 qsub


PRO bd_fit, obj_fitstab_file, label, no_fit=no_fit

;   num = '21_17.346'
;   obj_fitstab_file = '/home/barden/Desktop/multi/BD_objects/t'+num+'_gf.fits'


   fit_info = mrdfits(obj_fitstab_file, 'FIT_INFO', /silent)

   tab = mrdfits(obj_fitstab_file, 'FINAL_BAND', /silent)

;   fit_info.initfile = '/home/barden/Desktop/multi/BD_objects/t'+num+'_obj'
;   fit_info.constrnt = '/home/barden/Desktop/multi/BD_objects/t'+num+'_constr'
   
   band_info = mrdfits(obj_fitstab_file, 'BAND_INFO', /silent)
   
   obj_file = strtrim(fit_info.initfile,2)
   constr_file = strtrim(fit_info.constrnt,2)

   band_str = strupcase(strtrim(band_info.band,2))
   nband = n_elements(band_str)

   tmp = mrdfits(obj_fitstab_file, 'MODEL_'+band_str[0], model, /silent)

   ;extract info from SS-fit
   forward_function read_sersic_results
   ss_mult = read_sersic_results(obj_fitstab_file, nband)

   ;print, obj_fitstab_file, ss_mult.MAG_GALFIT_BAND[0]
   
   obj_file = strrep(obj_file, '/home/boris/', '')
   openw, filew, obj_file+'_bd'+label, /get_lun
   openr, filer, obj_file, /get_lun
   line = ''
   REPEAT BEGIN
      readf, filer, line

      IF strpos(strtrim(line, 2), 'B) ') EQ 0 THEN BEGIN
         line = 'B) '+strrep(obj_fitstab_file, '.fits', $
                             '_bd'+label+'.fits    # output file name')
      ENDIF
      IF strpos(strtrim(line, 2), 'G) ') EQ 0 THEN BEGIN
         line = 'G) '+constr_file+'_bd'+label
     ENDIF
     ;change path from boris on dator to general location
     line = strrep(line, '/home/boris/', '')
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
        '    1   band       #     R_e              [Pixels]'
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
        '    1   band       #     R_e              [Pixels]'
   printf, filew, re
   n = string((ss_mult.N_GALFIT_BAND >1.5), $
                format = '('+string(nband)+'(A,","))')
   n = strtrim(strcompress(n, /remove_all), 2)
   n = ' 5) '+strmid(n, 0, strlen(n)-1)+ $
        '   1   band       # Sersic exponent (deVauc=4, expdisk=1)'
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
   constr_file = strrep(constr_file, '/home/boris/', '')
   openw, ut, constr_file+'_bd'+label, /get_lun

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
      IF keyword_set(nice) THEN spawn, 'nice '+galfit_exe+' '+obj_file+'_bd'+label $
      ELSE spawn, galfit_exe+' '+obj_file+'_bd'+label
   ENDIF
END

PRO run_bd_fit, data_table_file, label
   ;run_bd_fit, 'gama/galapagos/galapagos_2.0.3_galfit_0.1.2.1_GAMA_9/GAMA_9_ffvqqff_gama_only.fits', '1'
   ;this must be run from the root of the gama/galapagos/... tree
   ;in order to get the paths right
   ;data_table = 'relative/path/to/GAMA_9_ffvqqff_gama_only.fits
   data_table = mrdfits(data_table_file, 1)

   openw, batch_file, 'bd'+label+'_batch_file', /get_lun
   rsync_filename = 'bd'+label+'_rsync_includes'
   openw, rsync_file, rsync_filename, /get_lun

   printf, rsync_file, '+ **/'
   FOR i=0l, n_elements(data_table)-1 DO BEGIN
       ;only do B/D for bright objects
       IF ((data_table[i].MAG_GALFIT_BAND[0] GE 17.0) && (data_table[i].MAG_GALFIT_BAND[0] LT 18.0)) THEN BEGIN
           obj_fitstab_file = strtrim(data_table[i].file_galfit, 2)
           ;change path from boris on dator to general location
           obj_fitstab_file = strrep(obj_fitstab_file, '/home/boris/', '')
           IF file_test(obj_fitstab_file) THEN BEGIN
               bd_fit, obj_fitstab_file, label, /no_fit
               printf, batch_file, strrep(obj_fitstab_file, '_gf.fits', '_obj_bd'+label)
               obj_id = STRSPLIT(obj_fitstab_file, '/', /EXTRACT)
               obj_id = strrep(obj_id(N_ELEMENTS(obj_id)-1), '_gf.fits', '')
               printf, rsync_file, '+ *'+obj_id+'*'
           ENDIF
       ENDIF
  ENDFOR
  printf, rsync_file, '- *' 
  print, "Necessary files can now be transferred using the command:"
  print, "rsync -av --prune-empty-dirs --include-from="+rsync_filename+" ./ jupiter:/path/to/gama/galapagos/galapagos_run/"
  free_lun, batch_file
  free_lun, rsync_file
END

PRO create_batches, n_cores, bd_batch_file, galexe_str, outdir, outfile
   ;create_batches, 64, '/home/ppzsb1/bd_batch_file', 'galfitm-0.1.2.1', 'gama/galapagos/galapagos_2.0.3_galfit_0.1.2.1_GAMA_9/batches', 'gala_gama_bd1_'
   READCOL,bd_batch_file,F='A', bd_files
   nfiles = n_elements(bd_files)
   n_per_batch = ceil(nfiles / float(n_cores))
   batch = 0
   FOR i=0l, nfiles-1 DO BEGIN
      IF i MOD n_per_batch EQ 0 THEN BEGIN
         IF batch GT 0 THEN BEGIN
            printf, lun, 'echo "Finished job now"'
            free_lun, lun
         ENDIF
         batch++
         outfilenum = outfile+string(strtrim(batch, 2), format='(I3.3)')
         openw, lun, outdir+'/'+outfilenum, /get_lun

         printf, lun, '#!/bin/bash'
         printf, lun, '# This is a submit script for B/D fitting.'
         printf, lun, '# OPTIONS FOR GRID ENGINE ==================================================='
         printf, lun, '# Job should run for no longer than 4 hours'
         printf, lun, '#$ -l h_rt=4:00:00'
         printf, lun, '# Standard output and standard error will both be saved to'
         printf, lun, '# "'+outfilenum+'.out" in the directory qsub was run from'
         printf, lun, '#$ -cwd'
         printf, lun, '#$ -j y'
         printf, lun, '#$ -o '+outfilenum+'.out'
         printf, lun, '# OPTIONS FOR GRID ENGINE===================================================='
         printf, lun, 'echo "Running on `hostname`"'
         printf, lun, 'cd /work/work1/ppzsb1/megamorph'
         printf, lun, 'GALFIT="./'+galexe_str+'"'
     ENDIF
     
     obj_fitstab_file = strrep(bd_files[i], '_obj_bd', '_gf_bd')+'.fits'
     cmd_str = '[ ! -f '+obj_fitstab_file+' ] && $GALFIT '+bd_files[i]
     printf, lun, cmd_str
   ENDFOR
   printf, lun, 'echo "Finished job now"'

   free_lun, lun
END

; PRO extract_bd_info, data_table_file, band_str, out_fits_table, version_num, label
; ;bands in band_str have to be in the proper order!

;    setup = {version:0}
;    setup.version = version_num

;    nband = n_elements(band_str)

;    data_table = mrdfits(data_table_file, 1)
;    forward_function read_sersic_results

;    FOR i=0l, n_elements(data_table) DO BEGIN
;       obj_file = strtrim(data_table[i].initfile, 2)+'_bd'+label
;       obj_fitstab_file = strrep(obj_file, '_obj_bd', '_gf_bd')+'.fits'
;       IF file_test(obj_fitstab_file) THEN BEGIN
;          bd_table = read_sersic_results(obj_file, nband, /bd)

;          str = strtrim(data_table[i].initfile, 2)
;          sky_file = strmid(str, 0, strpos(str, '_obj')+'_'+['', band_str]

;          update_table, bd_table, i, out_fits_table, obj_file,
;          sky_file, nband, setup, /final, /bd
;       ENDIF
;    ENDFOR

; END
