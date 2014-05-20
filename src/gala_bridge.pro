PRO gala_bridge, filein, bd_fit = bd_fit
;variables provided in filein:
;cur, orgwht, idx, orgpath, orgpre, setup, psf, chosen_psf_file, sky_file, 
;stamp_param_file, mask_file, im_file, obj_file, 
;constr_file, out_file, table, nband, outpath_file
;orgpath_band, orgpath_pre, orgpath_galfit, orgpath_file,
;orgpath_file_no_band, seed

restore, filein
cur = save_cur
table = save_table

goodband = 0
to_be_redone = -1
for b=1,nband do begin
;read in image and weight (takes few sec)
    im=readfits(strtrim(table[cur].frame[b],2), hd,/silent)
    wht=readfits(orgwht[idx,b], whd,/silent)
;read segmentation map (needed for excluding neighbouring sources)
    seg = readfits(orgpath_file[idx,0]+setup.outseg, seghd,/silent)
;read the skymap
    map = readfits(orgpath_file_no_band[idx,b]+setup.stamp_pre[b]+'.'+setup.skymap+'.fits', maphd,/silent)
;image size
    sz_im = (size(im))[1:2]
    
; create arrays used by getsky_loop to identify sky pixels
; only needs to be done once as all images for different bands have to
; be micro-resgistered and have the same size. Saves time!
    if b eq 1 then begin
        xarr = (lindgen(sz_im[0], sz_im[1]) MOD sz_im[0])+1
        yarr = (transpose(lindgen(sz_im[1], sz_im[0]) MOD sz_im[1]))+1
    ENDIF

;rad is the minimum starting radius for the sky calculation (outside
;the aperture of the object)
   rad = table.a_image*table.kron_radius*setup.skyscl+setup.skyoff

;get first guess for global sky
   skypix = where(map EQ 0 and finite(im) eq 1,ct)
; new scheme takes around 2 second,s the next, old system, about 6
; moment() would be 3 times faster, but returns a MEAN value!
;; current version
; DAMN! HAVE TO CATCH THE CASE WHEN NO PIXELS ARE SKYPIXELS!!!! Was
; that what backup=backup was supposed to do in getsky_loop?
   if ct gt 0 then begin
       global_sky=median(im[skypix])
       global_sigsky=stddev(im[skypix])
       if finite(global_sky) ne 1 or finite(global_sigsky) ne 1 then begin
;; old version
;   resistant_mean, im[skypix], 3, sky, sigsky, nrej
;   sigsky *= sqrt(ct-1-nrej)
           global_sky=median(im[skypix],/double)
           global_sigsky=stddev(im[skypix],/double)
       endif
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
    ENDIF
    IF ct eq 0 then begin
       global_sky=median(im)
       global_sigsky=stddev(im)
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
; see if current source has contributors
; this is only done in REFERENCE band, for the other bands, num and
; frames are input by gala_bridge/galapagos, to which they have been returned in
; the run on the reference band.
; !!!!!!! DON'T USE FIRST BAND, USE FIRST BAND WITH DATA!
   if b eq 1 then BEGIN
       contrib_targets, setup.expt[b], setup.zp[b], setup.neiscl, setup.skyoff, setup.power, table, $
         cur, setup.cut, nums, frames
    ENDIF 
  
; get sky values, while using the contributing sources
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
    create_mask, table, wht, seg, stamp_param_file, mask_file[b], $
                 im_file[b], table[cur].frame[b], cur, $
                 setup.neiscl, setup.skyoff, nums, frames, $
                 setup.maglim_gal, setup.maglim_star, $
                 setup.stel_slope, setup.stel_zp, objects, corner, $
                 b 
    if b eq 1 then begin
        delvarx, save_objects, save_corner
        save_objects = objects
        save_corner = corner
    ENDIF       

endfor

;if not keyword_set(bd_fit) then $
prepare_galfit, setup, save_objects, setup.files, save_corner, table, obj_file, $
                im_file, sigma_file, constr_file, mask_file, chosen_psf_file, $
                out_file, sky_file, setup.convbox, setup.zp, $
                setup.platescl, nums, frames, cur, $
                setup.outcat, setup.outparam, setup.stampfile, $
                setup.conmaxre, setup.conminm, setup.conmaxm, $
                setup.version, nband, orgpre ;, n_constrained = n_constrained

;if keyword_set(bd_fit) then $
;  prepare_galfit, setup, save_objects, setup.files, save_corner, table, obj_file, $
;  im_file, constr_file, mask_file, chosen_psf_file, $
;  out_file, sky_file, setup.convbox, setup.zp, $
;  setup.platescl, nums, frames, cur, $
;  setup.outcat, setup.outparam, setup.stampfile, $
;  setup.conmaxre, setup.conminm, setup.conmaxm, $
;  setup.version, nband, orgpre, bd_fit = bd_fit
;spawn, 'touch '+filein+'.preparegalfit';§§§§§§§§§§§§§§§§§§§§§§

;spawn the script
   cd, outpath_galfit[idx]
   IF setup.nice THEN spawn, 'nice '+setup.galexe+' '+obj_file $
   ELSE spawn, setup.galexe+' '+obj_file
   spawn, 'rm '+outpath_galfit[idx]+'galfit.[0123456789]*'
   spawn, 'rm ~/galfit.[0123456789]*'

   file_delete, filein
   wait, 1
END
