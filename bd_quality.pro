FUNCTION bd_quality, obj_fitstab_file, setup_file, r_kron, plot=plot
;given the GALFIT output files for a single component sersic or a two
;component Bulge-disc fit plus the corresponding mask image (all variables
;strings with filenames), determine the ratio of residuals. Values exceeding
;1.25 imply that the B/D fit is preferred over the Sersic-Fit.
   nsig = 10
   fact = 1.                   ;factor by which Kron-radius is enlarged
   r_core = 1.

;==============================================================================
; Using the Kron-radius is sub-optimal as SExtractor might have over-deblended
; the object.
; Instead use some average of the combination of the half-light radii of ss
; and bd components
;
; OR use half-light radius only when half-light radius and Kron radius disagree
;==============================================================================

;segmentation maps do not work: depth is not sufficient ->
;mask becomes too round -> too much noise gets included in estimator ->
;result gets washed out


   r_kron = 46.2245 ;(a*kron)
   num = '21_17.346'
   obj_fitstab_file = '/home/barden/Desktop/multi/BD_objects/t'+num+'_gf.fits'
   setup_file = '/home/barden/Desktop/multi/BD_objects/gala.gama_9_complete'


   fit_info = mrdfits(obj_fitstab_file, 'FIT_INFO', /silent)

fit_info.initfile = '/home/barden/Desktop/multi/BD_objects/t'+num+'_obj'
   
   band_info = mrdfits(obj_fitstab_file, 'BAND_INFO', /silent)
   
   obj_file = strtrim(fit_info.initfile,2)

   band_str = strtrim(band_info.band,2)
   nband = n_elements(band_str)

   read_setup, setup_file, setup
   mask = strmid(obj_file, 0, strpos(obj_file, '_obj', /reverse_search))
   mask = mask+'_'+band_str[0]+'_'+setup.mask+'.fits'

   openr, 1, obj_file
   line = ''
   REPEAT BEGIN
      readf, 1, line
   ENDREP UNTIL strpos(strtrim(line, 2), '0) sersic') EQ 0
   readf, 1, line
   line = strtrim(line, 2)
   pos = min([strpos(line, ','), strpos(line, ' ', 3)])
   x = double(strmid(line, 3, pos-3))
   readf, 1, line
   line = strtrim(line, 2)
   pos = min([strpos(line, ','), strpos(line, ' ', 3)])
   y = double(strmid(line, 3, pos-3))
   FOR i=0, 3 DO readf, 1, line
   line = strtrim(line, 2)
   pos = min([strpos(line, ','), strpos(line, ' ', 3)])
   q = double(strmid(line, 3, pos-3))
   readf, 1, line
   line = strtrim(line, 2)
   pos = min([strpos(line, ','), strpos(line, ' ', 4)])
   pa = double(strmid(line, 4, pos-3))
   close, 1


   quality = 0d
   FOR band=0, nband-1 DO BEGIN
      fits_read, mask, m
      sr = mrdfits(obj_fitstab_file, 'RESIDUAL_'+band_str[band], /silent)
      sr *= (1-m)
      bd = mrdfits(strrep(obj_fitstab_file, '.fits', '_bd.fits'), $
                   'RESIDUAL_'+band_str[band], /silent)
      bd *= (1-m)

      sz = (size(sr))[1:2]
      dist_ellipse, rad, sz, x, y, 1/q, pa
;      rad*=seg

      rad_sr = where(rad LE (r_kron*fact) AND sr NE 0 AND rad GT r_core, $
                     n_rad_sr)
      IF n_rad_sr EQ 0 THEN message, 'no pixels found'

      tot_sr = total(abs(sr[rad_sr]))

      resistant_mean, sr[rad_sr]*(rad[rad_sr]), 3, m, sig, n
      sig *= sqrt(n_rad_sr-n-1)
;      plot, rad[rad_sr], sr[rad_sr]*(rad[rad_sr]), psym=1, chars=2, $
;            xtitle='r_kron', ytitle='flux x r_kron'
;      hor, [-1, 0, 1]*sig*nsig+m, col=250

;remove outliers sersic: sig*nsig 
      rad_sr = where(rad LE r_kron*fact AND sr NE 0 AND rad GT r_core AND $
                     sr*rad GT -nsig*sig AND sr*rad LT nsig*sig, n_rad_sr)
      IF n_rad_sr EQ 0 THEN message, 'no pixels found'
      
      running_resistant_mean, sqrt(rad[rad_sr]), sr[rad_sr]*(rad[rad_sr]), $
                              max(sqrt(rad[rad_sr]))/100., $
                              ox_sr, oy_sr, od_sr, osig_sr

      rad_bd = where(rad LE r_kron*fact AND bd NE 0 AND rad GT r_core, n_rad_bd)
      IF n_rad_bd EQ 0 THEN message, 'no pixels found'

      tot_bd = total(abs(bd[rad_bd]))

      resistant_mean, bd[rad_bd]*(rad[rad_bd]), 3, m, sig, n
      sig *= sqrt(n_rad_bd-n-1)
;remove outliers b/d: sig*nsig 
      rad_bd = where(rad LE r_kron*fact AND bd NE 0 AND rad GT r_core AND $
                     bd*rad GT -nsig*sig AND bd*rad LT nsig*sig, n_rad_bd)
      IF n_rad_bd EQ 0 THEN message, 'no pixels found'

      running_resistant_mean, sqrt(rad[rad_bd]), bd[rad_bd]*(rad[rad_bd]), $
                              max(sqrt(rad[rad_bd]))/100., $
                              ox_bd, oy_bd, od_bd, osig_bd

      IF keyword_set(plot) THEN BEGIN
         plot, [ox_bd, ox_bd], [oy_bd+osig_bd*2, oy_bd-osig_bd*2], psym=6, $
               /nodata, chars=2, xtitle='sqrt(r_kron)', $
               ytitle='flux x r_kron';, yr=[-1, 1]*sig*nsig
         oplot, ox_bd, abs(oy_bd), psym=6
         oplot, ox_bd, osig_bd*2, psym=10
         oplot, ox_bd, -osig_bd*2, psym=10
         oplot, ox_sr, abs(oy_sr), psym=6, col=150
         oplot, ox_sr, osig_sr*2, psym=10, col=150
         oplot, ox_sr, -osig_sr*2, psym=10, col=150
         hor, 0, col=250
         hor, [-1, 1]*sig*nsig, col=250
      ENDIF

      delvarx, m, sr, bd

      tmp=where(osig_bd LT osig_sr, ct_sig)
      tmp=where(abs(oy_bd) LT abs(oy_sr), ct_mn)

      IF n_elements(osig_bd) EQ 0 THEN quality = [quality, -1] $
      ELSE quality = [quality, float(ct_sig+ct_mn)/n_elements(osig_bd)-1]

;total fluxes might help identifying pathetic cases
;print, tot_sr-tot_bd, tot_sr/tot_bd-1

;q=''
;read, q, prompt=band_str[band]+': '+strtrim(quality[band+1], 2)+ $
;      ' r_f: '+strtrim(tot_sr/tot_bd-1, 2)
   ENDFOR

   quality = quality[1:*]
   return, quality
END
