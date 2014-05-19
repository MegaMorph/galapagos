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


;PRO bd_fit, out_file, out_file_bd, setup, obj_file, obj_file_bd,
;   constr_file, constr_file_bd, no_fit=no_fit
PRO gala_bd_bridge, filein
restore, filein
;obj_fitstab_file = out_file_bd+'.fits'
in_file = out_file+'.fits'

;   num = '21_17.346'
;   obj_fitstab_file = '/home/barden/Desktop/multi/BD_objects/t'+num+'_gf.fits'

   tab = mrdfits(in_file, 'FINAL_BAND', /silent)

   band_info = mrdfits(in_file, 'BAND_INFO', /silent)
   
   band_str = strupcase(strtrim(band_info.band,2))
   nband = n_elements(band_str)
   if nband eq 1 then bandstr = ' '
   if nband gt 1 then bandstr = 'band'

   tmp = mrdfits(in_file, 'MODEL_'+band_str[0], model, /silent)

   ;extract info from SS-fit
   forward_function read_sersic_results
   ss_mult = read_sersic_results(in_file, nband)

   ;print, obj_fitstab_file, ss_mult.MAG_GALFIT_BAND[0]
   
;   obj_file = strrep(obj_file, '/home/boris/', '')
   openw, filew, obj_file_bd, /get_lun
   openr, filer, obj_file, /get_lun
   line = ''
   REPEAT BEGIN
      readf, filer, line

      IF strpos(strtrim(line, 2), 'B) ') EQ 0 THEN BEGIN
          line = 'B) '+out_file_bd+'.fits'
      ENDIF
      IF strpos(strtrim(line, 2), 'G) ') EQ 0 THEN BEGIN
          line = 'G) '+constr_file_bd
     ENDIF
     IF (setup.bd_hpc AND strpos(strtrim(line, 2), 'D) ') EQ 0) THEN line = strrep(line, setup.bd_psf_corr[0], setup.bd_psf_corr[1])
;change path from boris on dator to general location
     IF (setup.bd_hpc AND strpos(strtrim(line, 2), 'D) ') NE 0) THEN line = strrep(line, setup.outdir, setup.bd_hpc_path)
     printf, filew, line
   ENDREP UNTIL strpos(strtrim(line, 2), '# Sersic function') EQ 0

; DISK PARAMETERS
   printf, filew
   printf, filew, ' 0) sersic             # Object type --- DISC'
   x = string(ss_mult.X_GALFIT_BAND, $
                format = '('+string(nband)+'(A,","))')
   x = strtrim(strcompress(x, /remove_all), 2)
   x = ' 1) '+strmid(x, 0, strlen(x)-1)+ $
        '    '+strtrim(setup.cheb_d[0]+1,2)+' '+bandstr+'   # position x     [pixel]'
   printf, filew, x
   y = string(ss_mult.Y_GALFIT_BAND, $
                format = '('+string(nband)+'(A,","))')
   y = strtrim(strcompress(y, /remove_all), 2)
   y = ' 2) '+strmid(y, 0, strlen(y)-1)+ $
     '    '+strtrim(setup.cheb_d[1]+1,2)+'  '+bandstr+'   # position y     [pixel]'
   printf, filew, y
   mag = string(ss_mult.MAG_GALFIT_BAND+2.5*alog10(2), $
                format = '('+string(nband)+'(A,","))')
   mag = strtrim(strcompress(mag, /remove_all), 2)
   mag = ' 3) '+strmid(mag, 0, strlen(mag)-1)+ $
        '    '+strtrim(setup.cheb_d[2]+1,2)+'  '+bandstr+'    # total magnitude'
   printf, filew, mag
;correct re to be the right shape if cheb_d eq 0!!
; re started at constant value in any case!
; up to single_bd1 and multi_bd5
;   if nband eq 1 then re_d = string(ss_mult.RE_GALFIT_BAND, format = '(A)')
;   if nband gt 1 then re_d = string(strarr(nband)+median(ss_mult.RE_GALFIT_BAND), $
;                                  format = '('+string(nband)+'(A,","))'
; new setup after single_bd2 and multi_bd6
   if nband eq 1 then re_d = string(ss_mult.RE_GALFIT_BAND*1.2 >1., format = '(A)')
; original!   if nband eq 1 then re_d = string(ss_mult.RE_GALFIT_BAND >1., format = '(A)')
   if nband gt 1 then re_d = string(strarr(nband)+(median(ss_mult.RE_GALFIT_BAND)*1.2 >1.), $
                                  format = '('+string(nband)+'(A,","))')
   re_d = strtrim(strcompress(re_d, /remove_all), 2)
   re_d = ' 4) '+strmid(re_d, 0, strlen(re_d)-1)+ $
     '    '+strtrim(setup.cheb_d[3]+1,2)+'   '+bandstr+'       #     R_e              [Pixels]'

   printf, filew, re_d
;correct n to be the right shape if cheb_d eq 0!!
; sersic index of disk always started at 1
   if setup.cheb_d[4] eq -1 then n = string((strarr(nband)+1.), $
                                            format = '('+string(nband)+'(A,","))')
   if setup.cheb_d[4] ne -1 then begin
       if nband eq 1 then n = string((ss_mult.N_GALFIT_BAND <1.5), format = '(A)')
       if nband gt 1 then n = string(((strarr(nband)+median(ss_mult.N_GALFIT_BAND) <1.5)), $
                                            format = '('+string(nband)+'(A,","))')
   endif
   n = strtrim(strcompress(n, /remove_all), 2)
   n = ' 5) '+strmid(n, 0, strlen(n)-1)+ $
     '   '+strtrim(setup.cheb_d[4]+1,2)+'   '+bandstr+'       # Sersic exponent (deVauc=4, expdisk=1)'
   printf, filew, n
   q = string(ss_mult.Q_GALFIT_BAND, $
                format = '('+string(nband)+'(A,","))')
   q = strtrim(strcompress(q, /remove_all), 2)
   q = ' 9) '+strmid(q, 0, strlen(q)-1)+ $
        '    '+strtrim(setup.cheb_d[5]+1,2)+'   '+bandstr+'       # axis ratio (b/a)'
   printf, filew, q
   pa = string(ss_mult.PA_GALFIT_BAND, $
                format = '('+string(nband)+'(A,","))')
   pa = strtrim(strcompress(pa, /remove_all), 2)
   pa = ' 10) '+strmid(pa, 0, strlen(pa)-1)+ $
        '    '+strtrim(setup.cheb_d[6]+1,2)+ $
        '  '+bandstr+'       # position angle (PA) [Degrees: Up=0, Left=90]'
   printf, filew, pa

   printf, filew, ' Z) 0                  # output image (see above)'

; BULGE PARAMETERS
   printf, filew
   printf, filew
   printf, filew, '# Sersic function'
   printf, filew
   printf, filew, ' 0) sersic             # Object type --- BULGE'
   printf, filew, x
   printf, filew, y
   printf, filew, mag
;correct re to be the right shape if cheb_b eq 0!!
; re started at constant value in any case!

;; old setup up to single_bd and multi_bd5
;   if nband eq 1 then re_b = string(ss_mult.RE_GALFIT_BAND, format = '(A)')
;   if nband gt 1 then re_b = string(strarr(nband)+median(ss_mult.RE_GALFIT_BAND), $
;                                  format = '('+string(nband)+'(A,","))')
; new setup after single_bd2 and multi_bd6
   if nband eq 1 then re_b = string(ss_mult.RE_GALFIT_BAND*0.3 > 0.5, format = '(A)')
   if nband gt 1 then re_b = string(strarr(nband)+(median(ss_mult.RE_GALFIT_BAND)*0.3 > 0.5), $
                                  format = '('+string(nband)+'(A,","))')

   re_b = strtrim(strcompress(re_b, /remove_all), 2)
   re_b = ' 4) '+strmid(re_b, 0, strlen(re_b)-1)+ $
        '    '+strtrim(setup.cheb_b[3]+1,2)+'   '+bandstr+'       #     R_e              [Pixels]'
   printf, filew, re_b
;correct n to be the right shape if cheb_b eq 0!!
   if setup.cheb_b[4] eq -1 then n = string((strarr(nband)+4.), $
                                            format = '('+string(nband)+'(A,","))')
   if setup.cheb_b[4] ne -1 then begin
       if nband eq 1 then n = string((ss_mult.N_GALFIT_BAND >1.5), format = '(A)')
       if nband gt 1 then n = string(((strarr(nband)+median(ss_mult.N_GALFIT_BAND) >1.5)), $
                                            format = '('+string(nband)+'(A,","))')
;; try starting at 1 instead, Marina claims it's more stable (bd8)
;       if nband gt 1 then n = string(((strarr(nband)+1.)), format = '('+string(nband)+'(A,","))')
   endif
   n = strtrim(strcompress(n, /remove_all), 2)
   n = ' 5) '+strmid(n, 0, strlen(n)-1)+ $
        '   '+strtrim(setup.cheb_b[4]+1,2)+'   '+bandstr+'       # Sersic exponent (deVauc=4, expdisk=1)'
   printf, filew, n
   q = string(ss_mult.Q_GALFIT_BAND >0.6, $
                format = '('+string(nband)+'(A,","))')
   q = strtrim(strcompress(q, /remove_all), 2)
   q = ' 9) '+strmid(q, 0, strlen(q)-1)+ $
        '    '+strtrim(setup.cheb_b[5]+1,2)+'   '+bandstr+'       # axis ratio (b/a)'
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
             '  0  '+bandstr+'   # position x     [pixel]'
         printf, filew, x

         y = string(tab.(40+comp*3*7), $
                format = '('+string(nband)+'(A,","))')
         y = strtrim(strcompress(y, /remove_all), 2)
         y = ' 2) '+strmid(y, 0, strlen(y)-1)+ $
             '  0  '+bandstr+'   # position y     [pixel]'
         printf, filew, y

         mag = string(tab.(43+comp*3*7), $
                format = '('+string(nband)+'(A,","))')
         mag = strtrim(strcompress(mag, /remove_all), 2)
         mag = ' 3) '+strmid(mag, 0, strlen(mag)-1)+ $
             '   0   '+bandstr+'    # total magnitude'
         printf, filew, mag

         re = string(tab.(46+comp*3*7), $
                format = '('+string(nband)+'(A,","))')
         re = strtrim(strcompress(re, /remove_all), 2)
         re = ' 4) '+strmid(re, 0, strlen(re)-1)+ $
             '    0   '+bandstr+'       #     R_e              [Pixels]'
         printf, filew, re

         n = string(tab.(49+comp*3*7), $
                format = '('+string(nband)+'(A,","))')
         n = strtrim(strcompress(n, /remove_all), 2)
         n = ' 5) '+strmid(n, 0, strlen(n)-1)+ $
             '   0   '+bandstr+'       # Sersic exponent (deVauc=4, expdisk=1)'
         printf, filew, n

         q = string(tab.(52+comp*3*7), $
                format = '('+string(nband)+'(A,","))')
         q = strtrim(strcompress(q, /remove_all), 2)
         q = ' 9) '+strmid(q, 0, strlen(q)-1)+ $
             '    0   '+bandstr+'       # axis ratio (b/a)'
         printf, filew, q

         pa = string(tab.(55+comp*3*7), $
                format = '('+string(nband)+'(A,","))')
         pa = strtrim(strcompress(pa, /remove_all), 2)
         pa = ' 10) '+strmid(pa, 0, strlen(pa)-1)+ $
             '    0   '+bandstr+'       # position angle (PA) [Degrees: Up=0, Left=90]'
         printf, filew, pa

         printf, filew, ' Z) 0                  # output image (see above)'
      ENDFOR
   ENDIF

   free_lun, filer
   free_lun, filew

   ;maximum allowed positional offset
   pos_offset = ss_mult.RE_GALFIT_BAND[0]

   ;constraint file
;   constr_file = strrep(constr_file, '/home/boris/', '')
   openw, ut, constr_file_bd, /get_lun

   printf, ut, '# Component/    parameter   constraint  Comment'
   printf, ut, '# operation                  values'

   FOR j=2, maxcomp+1 DO BEGIN
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
   printf, ut, '           2_3 x offset'
   printf, ut, '           2_3 y offset'

   printf, ut
   free_lun, ut

   ;run galfit
   cd, galfit_path
   IF NOT setup.bd_hpc THEN BEGIN
       IF file_test(out_file_bd+'.fits') eq 0 then begin
           IF setup.nice THEN spawn, 'nice '+setup.galexe+' '+obj_file_bd $
           ELSE spawn, setup.galexe+' '+obj_file_bd
           wait, 1
       ENDIF
   ENDIF
   spawn, 'rm '+galfit_path+'/galfit.[0123456789]*'
   spawn, 'rm ~/galfit.[0123456789]*'
   file_delete, filein

END

