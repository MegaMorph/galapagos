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


PRO gala_bd_bridge, filein
   restore, filein
   
;obj_fitstab_file = out_file_bd+'.fits'
   in_file = out_file+'.fits'
   
   tab = mrdfits(in_file, 'FINAL_BAND', /silent)
   
   band_info = mrdfits(in_file, 'BAND_INFO', /silent)
   
   band_str = strupcase(strtrim(band_info.band,2))
   nband = n_elements(band_str)
   if nband eq 1 then bandstr = ' '
   if nband gt 1 then bandstr = 'band'
   
;extract info from SS-fit
   forward_function read_sersic_results
   ss_mult = read_sersic_results(in_file, nband)
   
;   obj_file = strrep(obj_file, '/home/boris/', '')
   openw, filew, obj_file_bd, /get_lun
   openr, filer, obj_file, /get_lun
   line = ''
   maxdeg = nband

   REPEAT BEGIN
       readf, filer, line
       
       IF strpos(strtrim(line, 2), 'A) ') EQ 0 THEN BEGIN
          workline=line
          If setup.do_restrict then begin
; get image names from seric fits header
             input_files=strtrim(band_info.datain,2)
             mask_files=strtrim(band_info.mask,2)

; read in primary ellips file
             deg_prim = readfits(strtrim(mask_file_primary,2)+'.fits',1,/silent)
; restrict maximal DOF if images are empty
             FOR b=1,nband DO BEGIN
                
; check how many postage stamps contain (a useful amount of) data and
; restrict maximum number of degrees in polynomial (b-1 as b starts counting at 1, instead of 0)
                  deg_im = readfits(strtrim(band_info[b-1].datain,2),1, /silent)
                  deg_wht = readfits(strtrim(band_info[b-1].mask,2),1,/silent)
                  deg_npix_prim = float(n_elements(where(deg_prim EQ 1)))
; masked pixels have value 1!
                  hlpin = where(deg_prim EQ 1 AND (deg_wht EQ 1 OR deg_im EQ 0), deg_npix_prim_mask)
                  delvarx, hlpin

; correct if more than x% of pixels are masked or have value 0 within
; the primary elliipse
                  IF deg_npix_prim_mask/deg_npix_prim GT setup.restrict_frac_primary/100. THEN $
                     maxdeg = maxdeg-1
               ENDFOR
           ENDIF
       ENDIF
       
; adapt a few lines for B/D use
       IF strpos(strtrim(line, 2), 'B) ') EQ 0 THEN BEGIN
           line = 'B) '+out_file_bd+'.fits'
       ENDIF
       IF strpos(strtrim(line, 2), 'G) ') EQ 0 THEN BEGIN
           line = 'G) '+constr_file_bd
       ENDIF
       IF strpos(strtrim(line, 2), 'W) ') EQ 0 THEN BEGIN
          IF setup.version GE 4.1 THEN line = 'W) '+setup.gal_output_bd+' # GALFIT output file format'
       ENDIF


       IF (setup.bd_hpc AND strpos(strtrim(line, 2), 'D) ') EQ 0) THEN line = strrep(line, setup.bd_psf_corr[0], setup.bd_psf_corr[1])
;change path from boris on dator to general location
       IF (setup.bd_hpc AND strpos(strtrim(line, 2), 'D) ') NE 0) THEN line = strrep(line, setup.outdir, setup.bd_hpc_path)
       printf, filew, line
   ENDREP UNTIL strpos(strtrim(line, 2), '# Sersic function') EQ 0

; DISK PARAMETERS
   printf, filew
   printf, filew, ' 0) sersic             # Object type --- DISC'

; X DISK
   if (setup.cheb_d[0]+1)<maxdeg eq 1 then begin
       x_d = string(strarr(nband)+median([ss_mult.X_GALFIT_BAND]), $
                    format = '('+string(nband)+'(A,","))')
   endif else x_d = string(ss_mult.X_GALFIT_BAND, $
                           format = '('+string(nband)+'(A,","))')
   x_d = strtrim(strcompress(x_d, /remove_all), 2)
   x_d = ' 1) '+strmid(x_d, 0, strlen(x_d)-1)+ $
     '    '+strtrim((setup.cheb_d[0]+1)<maxdeg,2)+' '+bandstr+'   # position x     [pixel]'
   printf, filew, x_d
   
; Y DISK
   if (setup.cheb_d[1]+1)<maxdeg eq 1 then begin
       y_d = string(strarr(nband)+median([ss_mult.Y_GALFIT_BAND]), $
                    format = '('+string(nband)+'(A,","))')
   endif else y_d = string(ss_mult.Y_GALFIT_BAND, $
                           format = '('+string(nband)+'(A,","))')
   y_d = strtrim(strcompress(y_d, /remove_all), 2)
   y_d = ' 2) '+strmid(y_d, 0, strlen(y_d)-1)+ $
     '    '+strtrim((setup.cheb_d[1]+1)<maxdeg,2)+'  '+bandstr+'   # position y     [pixel]'
   printf, filew, y_d
   
; MAG DISK
   if (setup.cheb_d[2]+1)<maxdeg eq 1 then begin
       mag_d = string(strarr(nband)+median([ss_mult.MAG_GALFIT_BAND])+2.5*alog10(2), $
                      format = '('+string(nband)+'(A,","))')
   endif else mag_d = string(ss_mult.MAG_GALFIT_BAND+2.5*alog10(2), $
                             format = '('+string(nband)+'(A,","))')
   mag_d = strtrim(strcompress(mag_d, /remove_all), 2)
   mag_d = ' 3) '+strmid(mag_d, 0, strlen(mag_d)-1)+ $
     '    '+strtrim((setup.cheb_d[2]+1)<maxdeg,2)+'  '+bandstr+'    # total magnitude'
   printf, filew, mag_d

; RE DISK
; re always started at constant value!
; new setup after single_bd2 and multi_bd6
;   if nband eq 1 then re_d = string(ss_mult.RE_GALFIT_BAND*1.2 >1., format = '(A)')
;   if nband gt 1 then re_d = string(strarr(nband)+(median(ss_mult.RE_GALFIT_BAND)*1.2 >1.), $
;                                    format = '('+string(nband)+'(A,","))')
   re_d = string(strarr(nband)+(median([ss_mult.RE_GALFIT_BAND])*1.2 >1.), $
                 format = '('+string(nband)+'(A,","))')
   re_d = strtrim(strcompress(re_d, /remove_all), 2)
   re_d = ' 4) '+strmid(re_d, 0, strlen(re_d)-1)+ $
     '    '+strtrim((setup.cheb_d[3]+1)<maxdeg,2)+'   '+bandstr+'       #     R_e              [Pixels]'
   printf, filew, re_d

; SERSIC INDEX DISK
; n always started at constant value!
   if setup.cheb_d[4] eq -1 then n_d = string((strarr(nband)+1.), $
                                              format = '('+string(nband)+'(A,","))')
   if setup.cheb_d[4] ne -1 then begin
       n_d = string(strarr(nband)+(median([ss_mult.N_GALFIT_BAND]) <1.5), $
                                       format = '('+string(nband)+'(A,","))')
   endif
   n_d = strtrim(strcompress(n_d, /remove_all), 2)
   n_d = ' 5) '+strmid(n_d, 0, strlen(n_d)-1)+ $
     '   '+strtrim((setup.cheb_d[4]+1)<maxdeg,2)+'   '+bandstr+'       # Sersic exponent (deVauc=4, expdisk=1)'
   printf, filew, n_d

; AR DISK
   if (setup.cheb_d[5]+1)<maxdeg eq 1 then begin
       q_d = string(strarr(nband)+median([ss_mult.Q_GALFIT_BAND]), $
                    format = '('+string(nband)+'(A,","))')
   endif else q_d = string(ss_mult.Q_GALFIT_BAND, $
                           format = '('+string(nband)+'(A,","))')
   q_d = strtrim(strcompress(q_d, /remove_all), 2)
   q_d = ' 9) '+strmid(q_d, 0, strlen(q_d)-1)+ $
     '    '+strtrim((setup.cheb_d[5]+1)<maxdeg,2)+'   '+bandstr+'       # axis ratio (b/a)'
   printf, filew, q_d

; PA DISK
   if (setup.cheb_d[6]+1)<maxdeg eq 1 then begin
       pa_d = string(strarr(nband)+median([ss_mult.PA_GALFIT_BAND]), $
                     format = '('+string(nband)+'(A,","))')
   endif else pa_d = string(ss_mult.PA_GALFIT_BAND, $
                            format = '('+string(nband)+'(A,","))')
   pa_d = strtrim(strcompress(pa_d, /remove_all), 2)
   pa_d = ' 10) '+strmid(pa_d, 0, strlen(pa_d)-1)+ $
     '    '+strtrim((setup.cheb_d[6]+1)<maxdeg,2)+ $
     '  '+bandstr+'       # position angle (PA) [Degrees: Up=0, Left=90]'
   printf, filew, pa_d
   
   printf, filew, ' Z) 0                  # output image (see above)'
   
; BULGE PARAMETERS
   printf, filew
   printf, filew
   printf, filew, '# Sersic function'
   printf, filew
   printf, filew, ' 0) sersic             # Object type --- BULGE'

; X BULGE
   if (setup.cheb_b[0]+1)<maxdeg eq 1 then begin
       x_b = string(strarr(nband)+median([ss_mult.X_GALFIT_BAND]), $
                    format = '('+string(nband)+'(A,","))')
   endif else x_b = string(ss_mult.X_GALFIT_BAND, $
                           format = '('+string(nband)+'(A,","))')
   x_b = strtrim(strcompress(x_b, /remove_all), 2)
   x_b = ' 1) '+strmid(x_b, 0, strlen(x_b)-1)+ $
     '    '+strtrim((setup.cheb_b[0]+1)<maxdeg,2)+' '+bandstr+'   # position x     [pixel]'
   printf, filew, x_b

; Y BULGE
   if (setup.cheb_b[1]+1)<maxdeg eq 1 then begin
       y_b = string(strarr(nband)+median([ss_mult.Y_GALFIT_BAND]), $
                    format = '('+string(nband)+'(A,","))')
   endif else y_b = string(ss_mult.Y_GALFIT_BAND, $
                           format = '('+string(nband)+'(A,","))')
   y_b = strtrim(strcompress(y_b, /remove_all), 2)
   y_b = ' 2) '+strmid(y_b, 0, strlen(y_b)-1)+ $
     '    '+strtrim((setup.cheb_b[1]+1)<maxdeg,2)+'  '+bandstr+'   # position y     [pixel]'
   printf, filew, y_b

; MAG BULGE
   if (setup.cheb_b[2]+1)<maxdeg eq 1 then begin
       mag_b = string(strarr(nband)+median([ss_mult.MAG_GALFIT_BAND])+2.5*alog10(2), $
         format = '('+string(nband)+'(A,","))')
   endif else mag_b = string(ss_mult.MAG_GALFIT_BAND+2.5*alog10(2), $
                             format = '('+string(nband)+'(A,","))')
   mag_b = strtrim(strcompress(mag_b, /remove_all), 2)
   mag_b = ' 3) '+strmid(mag_b, 0, strlen(mag_b)-1)+ $
     '    '+strtrim((setup.cheb_b[2]+1)<maxdeg,2)+'  '+bandstr+'    # total magnitude'
   printf, filew, mag_b

; RE BULGE
; re always started at constant value!
; new setup after single_bd2 and multi_bd6
;   if nband eq 1 then re_b = string(ss_mult.RE_GALFIT_BAND*0.3 > 0.5, format = '(A)')
;   if nband gt 1 then re_b = string(strarr(nband)+(median(ss_mult.RE_GALFIT_BAND)*0.3 > 0.5), $
;                                    format = '('+string(nband)+'(A,","))')
   re_b = string(strarr(nband)+(median([ss_mult.RE_GALFIT_BAND])*0.3 > 0.5), $
                                    format = '('+string(nband)+'(A,","))')
   re_b = strtrim(strcompress(re_b, /remove_all), 2)
   re_b = ' 4) '+strmid(re_b, 0, strlen(re_b)-1)+ $
     '    '+strtrim((setup.cheb_b[3]+1)<maxdeg,2)+'   '+bandstr+'       #     R_e              [Pixels]'
   printf, filew, re_b

; SERSIC INDEX BULGE
; n always started at constant value!
   if setup.cheb_b[4] eq -1 then n_b = string((strarr(nband)+4.), $
                                              format = '('+string(nband)+'(A,","))')
   if setup.cheb_b[4] ne -1 then begin
;       if nband eq 1 then n_b = string((ss_mult.N_GALFIT_BAND >1.5), format = '(A)')
;       if nband gt 1 then n_b = string(((strarr(nband)+median(ss_mult.N_GALFIT_BAND) >1.5)), $
;                                     format = '('+string(nband)+'(A,","))')
       n_b = string(strarr(nband)+(median([ss_mult.N_GALFIT_BAND]) >1.5), $
                                     format = '('+string(nband)+'(A,","))')
;; try starting at 1 instead, Marina claims it's more stable (bd8)
;       if nband gt 1 then n = string(((strarr(nband)+1.)), format = '('+string(nband)+'(A,","))')
   endif
   n_b = strtrim(strcompress(n_b, /remove_all), 2)
   n_b = ' 5) '+strmid(n_b, 0, strlen(n_b)-1)+ $
     '   '+strtrim((setup.cheb_b[4]+1)<maxdeg,2)+'   '+bandstr+'       # Sersic exponent (deVauc=4, expdisk=1)'
   printf, filew, n_b

; AR BULGE
   if (setup.cheb_b[5]+1)<maxdeg eq 1 then begin
       q_b = string(strarr(nband)+(median([ss_mult.Q_GALFIT_BAND])>0.6), $
                                   format = '('+string(nband)+'(A,","))')
   endif else q_b = string(ss_mult.Q_GALFIT_BAND, $
                           format = '('+string(nband)+'(A,","))')
   q_b = strtrim(strcompress(q_b, /remove_all), 2)
   q_b = ' 9) '+strmid(q_b, 0, strlen(q_b)-1)+ $
     '    '+strtrim((setup.cheb_b[5]+1)<maxdeg,2)+'   '+bandstr+'       # axis ratio (b/a)'
   printf, filew, q_b

; PA BULGE
   if (setup.cheb_b[6]+1)<maxdeg eq 1 then begin
       pa_b = string(strarr(nband)+median([ss_mult.PA_GALFIT_BAND]), $
                     format = '('+string(nband)+'(A,","))')
   endif else pa_b = string(ss_mult.PA_GALFIT_BAND, $
                            format = '('+string(nband)+'(A,","))')
   pa_b = strtrim(strcompress(pa_b, /remove_all), 2)
   pa_b = ' 10) '+strmid(pa_b, 0, strlen(pa_b)-1)+ $
     '    '+strtrim((setup.cheb_b[6]+1)<maxdeg,2)+ $
     '  '+bandstr+'       # position angle (PA) [Degrees: Up=0, Left=90]'
   printf, filew, pa_b
 
   printf, filew, ' Z) 0                  # output image (see above)'
   
; new routine checks ss_mult (from fits table) instead
   maxcomp = ss_mult.neigh_galfit+2

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
;       printf, ut, '           '+strtrim(j, 2)+' pa -360 to 360'
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

   outputpost = ''
   IF setup.galfitoutput THEN outputpost = ' &> '+obj_file_bd+'.out'
   
   IF NOT setup.bd_hpc THEN BEGIN
      IF file_test(out_file_bd+'.fits') eq 0 then begin
         IF setup.nice THEN BEGIN
            IF setup.gal_kill_time EQ 0 THEN spawn, 'nice '+setup.galexe+' '+obj_file_bd+outputpost
            IF setup.gal_kill_time NE 0 THEN spawn, 'perl -e "alarm '+strtrim(60*setup.gal_kill_time,2)+'; exec @ARGV" "nice '+setup.galexe+' '+obj_file_bd+'"'+outputpost
         ENDIF
         
         IF NOT setup.nice THEN BEGIN
            IF setup.gal_kill_time EQ 0 THEN spawn, setup.galexe+' '+obj_file_bd+outputpost
            IF setup.gal_kill_time NE 0 THEN spawn, 'perl -e "alarm '+strtrim(60*setup.gal_kill_time,2)+'; exec @ARGV" "'+setup.galexe+' '+obj_file_bd+'"'+outputpost
         ENDIF
         wait, 1
      ENDIF
;       spawn, 'rm '+galfit_path+'/galfit.[0123456789]*'
      spawn, 'rm ~/galfit.[0123456789]*'
   ENDIF
   file_delete, filein
  
END

