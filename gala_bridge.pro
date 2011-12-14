PRO gala_bridge, filein
;variables provided in filein:
;cur, orgwht, idx, orgpath, orgpre, setup, psf, chosen_psf_file, sky_file, 
;stamp_param_file, mask_file, im_file, obj_file, 
;constr_file, out_file, fittab, nband, outpath_file
;orgpath_band, orgpath_pre, orgpath_galfit, orgpath_file,
;orgpath_file_no_band, seed
restore, filein
table = mrdfits(setup.outdir+setup.sexcomb+'.ttmp', 1)

for b=1,nband do begin
;read in image and weight (takes few sec)
    im=readfits(strtrim(table[cur].frame[b],2), hd,/silent)
    wht=readfits(orgwht[idx,b], whd,/silent)
    
;image size
    sz_im = (size(im))[1:2]
    
; create arrays used by getsky_loop to identify sky pixels
; only needs to be done once as all images for different bands have to
; be micro-resgistered and have the same size. Saves time!
    if b eq 1 then begin
        xarr = (lindgen(sz_im[0], sz_im[1]) MOD sz_im[0])+1
        yarr = (transpose(lindgen(sz_im[1], sz_im[0]) MOD sz_im[1]))+1
    ENDIF
    
;read segmentation map (needed for excluding neighbouring sources)
    seg = readfits(orgpath_file[idx,0]+setup.outseg, seghd,/silent)
    
;read the skymap
    map = readfits(orgpath_file_no_band[idx,b]+setup.stamp_pre[b]+'.'+setup.skymap+'.fits', maphd,/silent)
    
;rad is the minimum starting radius for the sky calculation (outside
;the aperture of the object)
    rad = table.a_image*table.kron_radius*setup.skyscl+setup.skyoff
    
;get first guess for global sky
    skypix = where(map EQ 0 and finite(im) eq 1,ct)
; new scheme takes around 2 seconds the next, old system, about 6
; moment() would be 3 times faster, but returns a MEAN value!
;; current version
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
;; second step is a curvefit to the histogram
;   plothist, im[skypix], x, y, xr = [-1, 1]*5*sigsky+sky, /peak, /noplot
;   IF n_elements(par) LT n_elements(x) THEN $
;    yfit = curvefit(x, y, noweight, par, sigma, $
;                    FUNCTION_name = 'curvefit_gauss2side', $
;                    /noderivative, status = status, iter = iter)
;   global_sky = par[1]
;   global_sigsky = par[2]

    delvarx, skypix
    
;make sure all positions are relative to the current frame
;(neighbouring frames may have negative positions)
    adxy, hd, table.alpha_j2000, table.delta_j2000, x, y
    
    table.x_image = x+1
    table.y_image = y+1
    
; read in psf to be passed onto procedure
    psf = readfits(chosen_psf_file[b], psfhead,/silent)
    
; fix contrib_targets
    getsky_loop, setup, cur, table, rad, im, hd, map, setup.expt, $
      setup.zp, setup.neiscl, setup.skyoff, setup.power, $
      setup.cut, setup.files, psf, setup.dstep, $
      setup.wstep, setup.gap, setup.nslope, sky_file[b], $
      setup.galfit_out, setup.outcat, setup.outparam, $
      setup.stampfile, global_sky, global_sigsky, $
      setup.convbox, nums, frames, setup.galexe, fittab, b, $
      orgpath_pre, outpath_file, outpath_file_no_band, nband, $
      xarr, yarr, seed
    if b eq 1 then begin
        delvarx, save_nums,save_frames
        save_nums = nums
        save_frames = frames
    ENDIF

;spawn, 'touch '+filein+'.skyloop';§§§§§§§§§§§§§§§§§§§§§§
    create_mask, table, wht, seg, stamp_param_file, mask_file[b], $
      im_file[b], table[cur].frame[b], cur, $
      setup.neiscl, setup.skyoff, save_nums, save_frames, $
      setup.maglim_gal, setup.maglim_star, $
      setup.stel_slope, setup.stel_zp, objects, corner, $
      b 
    if b eq 1 then begin
        delvarx, save_objects, save_corner
        save_objects = objects
        save_corner = corner
    ENDIF       
;spawn, 'touch '+filein+'.mask';§§§§§§§§§§§§§§§§tile10_5/t10_5.3416_obj§§§§§§
endfor

prepare_galfit, setup, save_objects, setup.files, save_corner, table, obj_file, $
  im_file, constr_file, mask_file, chosen_psf_file, $
  out_file, sky_file, setup.convbox, setup.zp, $
  setup.platescl, save_nums, save_frames, cur, $
  setup.outcat, setup.outparam, setup.stampfile, $
  setup.conmaxre, setup.conminm, setup.conmaxm, $
  fittab, setup.version, nband, orgpre;, n_constrained = n_constrained
;spawn, 'touch '+filein+'.preparegalfit';§§§§§§§§§§§§§§§§§§§§§§

;spawn the script
cd, outpath_galfit[idx]
IF setup.nice THEN spawn, 'nice '+setup.galexe+' '+obj_file $
ELSE spawn, setup.galexe+' '+obj_file
spawn, 'rm '+outpath_galfit[idx]+'galfit.[0123456789]*'

;   wait, randomu(systime(/seconds))*8+2
;   spawn, 'touch '+out_file

;file_delete, filein
wait, 1
END

