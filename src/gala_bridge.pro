PRO gala_bridge, filein
;variables provided in filein:
;cur, orgwht, idx, orgpath, orgpre, setup, psf, chosen_psf_file, sky_file, 
;stamp_param_file, mask_file, im_file,obj_file, 
;constr_file, out_file, table, nband, outpath_file
;orgpath_band, orgpath_pre, orgpath_galfit, orgpath_file,
;orgpath_file_no_band, seed
  restore, filein
  cur = save_cur
  table = save_table

  print, ' '
  print, ' '
  print, 'starting object '+obj_file
  print, ' '
  print, ' '
  
  
  FOR b=1,nband DO BEGIN
      print, 'getting sky in '+setup.stamp_pre[b]+'-band'
;read in image and weight (takes few sec)
     im=readfits(strtrim(table[cur].frame[b],2), hd,/silent)
     wht=readfits(orgwht[idx,b], whd,/silent)
;read segmentation map (needed FOR excluding neighbouring sources)
     seg = readfits(orgpath_file[idx,0]+setup.outseg, seghd,/silent)
;read the skymap
     map = readfits(orgpath_file_no_band[idx,b]+setup.stamp_pre[b]+'.'+setup.skymap+'.fits', maphd,/silent)
;image size
     sz_im = (size(im))[1:2]
     
; create arrays used by getsky_loop to identify sky pixels
; only needs to be done once as all images FOR different bands have to
; be micro-resgistered and have the same size. Saves time!
     IF b EQ 1 THEN BEGIN
        xarr = (lindgen(sz_im[0], sz_im[1]) MOD sz_im[0])+1
        yarr = (transpose(lindgen(sz_im[1], sz_im[0]) MOD sz_im[1]))+1
     ENDIF
     
;rad is the minimum starting radius FOR the sky calculation (outside
;the aperture of the object)
     rad = table.a_image*table.kron_radius*setup.skyscl+setup.skyoff
     
;get first guess for global sky
     skypix = where(map EQ 0 AND finite(im) EQ 1,ct)
; new scheme takes around 2 second,s the next, old system, about 6
; moment() would be 3 times faster, but returns a MEAN value!
;; current version
; DAMN! HAVE TO CATCH THE CASE WHEN NO PIXELS ARE SKYPIXELS!!!! Was
; that what backup=backup was supposed to do in getsky_loop?
     IF ct GT 0 THEN BEGIN
        global_sky=median(im[skypix])
        global_sigsky=stddev(im[skypix])
        IF finite(global_sky) ne 1 or finite(global_sigsky) NE 1 THEN BEGIN
;; old version
;   resistant_mean, im[skypix], 3, sky, sigsky, nrej
;   sigsky *= sqrt(ct-1-nrej)
           global_sky=median(im[skypix],/double)
           global_sigsky=stddev(im[skypix],/double)
        ENDIF
;; also belonging to old version
;   resistant_mean, im[skypix], 3, sky, sigsky, nrej
;   sigsky *= sqrt(ct-1-nrej)
;   par = [1, sky, sigsky, sigsky]
;; second step in old scheme is a curvefit to the histogram
;   plothist, im[skypix], x, y, xr = [-1, 1]*5*sigsky+sky, /peak, /noplot
;   IF n_elements(par) LT n_elements(x) THEN $
;    yfit = curvefit(x, y, noweight, par, sigma, $
;                    FUNCTION_name = 'curvefit_gauss2side', $
;                    /noderivative, status = status, iter = iter)
;   global_sky = par[1]
;   global_sigsky = par[2]
        print, 'sky estimated as median(skypixels) as '+strtrim(global_sky,2)+' +/- '+strtrim(global_sigsky,2)
     ENDIF
     IF ct EQ 0 THEN BEGIN
        global_sky=median(im)
        global_sigsky=stddev(im)
        print, 'sky estimated as median(image) as '+strtrim(global_sky,2)+' +/- '+strtrim(global_sigsky,2)
     ENDIF
     
     delvarx, skypix
     
;make sure all positions are relative to the current frame
;(neighbouring frames may have negative positions)
     adxy, hd, table.alpha_j2000, table.delta_j2000, x, y
     
     table.x_image = x+1
     table.y_image = y+1
     
; read in psf to be passed onto procedure
     psf = readfits(chosen_psf_file[b], psfhead,/silent)
     
; fix contrib_targets
; see IF current source has contributors
; this is only done in REFERENCE band, FOR the other bands, num and
; frames are input by gala_bridge/galapagos, to which they have been returned in
; the run on the reference band.
     IF b EQ 1 THEN BEGIN
        contrib_targets, setup.expt[b], setup.zp[b], setup.neiscl, setup.skyoff, setup.power, table, $
                         cur, setup.cut, nums, frames, setup.nobj_max 
        IF nums[0] GE 0 THEN print, strtrim(n_elements(nums),2)+' contributing targets found'
     ENDIF 
     
; get sky values, while using the contributing sources
     print, 'starting getsky_loop at '+systime(0)  
     getsky_loop, setup, cur, table, rad, im, hd, map, setup.expt, $
                  setup.zp, setup.neiscl, setup.skyoff, setup.power, $
                  setup.cut, setup.files, psf, setup.dstep, $
                  setup.wstep, setup.gap, setup.nslope, sky_file[b], $
                  setup.galfit_out, setup.outcat, setup.outparam, $
                  setup.stampfile, global_sky, global_sigsky, $
                  setup.convbox, nums, frames, b, $
                  orgpath_pre, outpath_file, outpath_file_no_band, nband, $
                  xarr, yarr, seed
     
; create mask images
     print, 'creating mask image for '+setup.stamp_pre[b]+'-band'
     create_mask, table, wht, seg, stamp_param_file, mask_file[b], mask_file_primary, $
                  im_file[b], table[cur].frame[b], cur, $
                  setup.neiscl, setup.skyoff, nums, frames, $
                  setup.maglim_gal, setup.maglim_star, $
                  setup.stel_slope, setup.stel_zp, objects, corner, $
                  b 
     IF b EQ 1 THEN BEGIN
        delvarx, save_objects, save_corner
        save_objects = objects
        save_corner = corner
     ENDIF       
     
  ENDFOR
  
  print, 'preparing GALFIT start file'
  prepare_galfit, setup, save_objects, setup.files, save_corner, table, obj_file, $
                  im_file, sigma_file, constr_file, mask_file, mask_file_primary, chosen_psf_file, $
                  out_file, sky_file, setup.convbox, setup.zp, $
                  setup.platescl, nums, frames, cur, $
                  setup.outcat, setup.outparam, setup.stampfile, $
                  setup.conmaxre, setup.conminm, setup.conmaxm, $
                  setup.version, nband, orgpre, maxdeg ;, n_constrained = n_constrained
  
  IF maxdeg LT setup.mindeg THEN BEGIN
     print, 'maximal degree of freedom is '+strtrim(maxdeg,2)+' and hence less than the necessary '+strtrim(setup.mindeg,2)+', skipping objects'
; write out file that indicates that object has been skipped
     openw, 1, obj_file+'_not_started', width=100
     printf, 1, 'object not started'
     close, 1
  ENDIF
  
;spawn the script
  cd, outpath_galfit[idx]
  
  outputpost = ''
  IF setup.galfitoutput THEN outputpost = ' &> '+obj_file+'.out'

;; perl version
  IF maxdeg GE setup.mindeg THEN BEGIN
     print, 'degrees of freedom is '+strtrim(maxdeg,2)+' and everything else also seems fine.'
     print, 'starting the fit at '+systime(0)
     IF setup.nice THEN BEGIN
        IF setup.gal_kill_time EQ 0 THEN spawn, 'nice '+setup.galexe+' '+obj_file+outputpost
        IF setup.gal_kill_time NE 0 THEN spawn, 'perl -e "alarm '+strtrim(60*setup.gal_kill_time,2)+'; exec @ARGV" "nice '+setup.galexe+' '+obj_file+'"'+outputpost
     ENDIF
     IF NOT setup.nice THEN BEGIN
        IF setup.gal_kill_time EQ 0 THEN spawn, setup.galexe+' '+obj_file+outputpost
        IF setup.gal_kill_time NE 0 THEN spawn, 'perl -e "alarm '+strtrim(60*setup.gal_kill_time,2)+'; exec @ARGV" "'+setup.galexe+' '+obj_file+'"'+outputpost
     ENDIF
     
  ENDIF

; calculate Chi^ for primary object only right here, for speed up of main code
; only creates the file, does NOT read them in!
  print, 'now deriving Chi^2 value for primary object if possible and applicable'
  IF file_test(strtrim(out_file+'.fits',2)) THEN BEGIN
     fit_info = mrdfits(strtrim(out_file+'.fits',2), 'FIT_INFO',/silent)
     derive_primary_chi2, strtrim(fit_info.logfile,2),setup.galexe
  ENDIF

  print, 'all done for this object at '+systime(0)  
  print, 'deleting *sav file and returning to main queue' 
  file_delete, filein
  wait, 1
END
