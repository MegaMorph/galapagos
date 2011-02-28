PRO gala_bridge, filein
;variables provided in filein:
;cur, orgwht, idx, orgpath, orgpre, setup, psf, chosen_psf_file, sky_file, 
;stamp_param_file, mask_file, im_file, obj_file, 
;constr_file, out_file, fittab, nband, outpath_file
;orgpath_band, orgpath_pre, orgpath_file, orgpath_file_no_band
stop
   restore, filein

   table = mrdfits(setup.outdir+setup.sexcomb+'.ttmp', 1)

for b=1,nband do begin
;read in image and weight (takes 15sec)
    fits_read, table[cur].frame[b], im, hd
;    fits_read, orgim[idx,b], im, hd
    fits_read, orgwht[idx,b], wht, whd

;image size
   sz_im = (size(im))[1:2]

;read segmentation map (needed for excluding neighbouring sources)
   fits_read, orgpath_file[idx,0]+setup.outseg, seg

;read the skymap
   fits_read, orgpath_file_no_band[idx,b]+setup.skymap+'.fits', map
;         print, systime(), ' done'

;rad is the minimum starting radius for the sky calculation (outside
;the aperture of the object)
   rad = table.a_image*table.kron_radius*setup.skyscl+setup.skyoff

;get first guess for global sky
;takes 35sec
;         print, systime(), ' estimating global sky...'
   skypix = where(map EQ 0, ct)
   resistant_mean, im[skypix], 3, sky, sigsky, nrej
   sigsky *= sqrt(ct-1-nrej)
   par = [1, sky, sigsky, sigsky]
   plothist, im[skypix], x, y, xr = [-1, 1]*5*sigsky+sky, /peak, /noplot
   IF n_elements(par) LT n_elements(x) THEN $
    yfit = curvefit(x, y, noweight, par, sigma, $
                    FUNCTION_name = 'curvefit_gauss2side', $
                    /noderivative, status = status, iter = iter)
   delvarx, skypix
   global_sky = par[1]
   global_sigsky = par[2]
;spawn, 'touch '+filein+'.sky';§§§§§§§§§§§§§§§§§§§§§§

;         print, systime(), ' done'
;         print, global_sky, global_sigsky

;make sure all positions are relative to the current frame
;(neighbouring frames may have negative positions)
   adxy, hd, table.alpha_j2000, table.delta_j2000, x, y

   table.x_image = x+1
   table.y_image = y+1
;++++++++++++++++++++++++++++++++++
   fits_read, chosen_psf_file[b], psf, psfhead
; fittab used??? YES
; fix getsky_loopgala_bridge,'/data/gama/galapagos_multi_wl/tile10_5/t10_5.1558_gf.sav'

; fix contrib_targets
; put b to be passed along!
   getsky_loop, setup, cur, table, rad, im, hd, map, setup.expt, $
     setup.zp, setup.neiscl, setup.skyoff, setup.power, $
     setup.cut, setup.files, psf, setup.dstep, $
     setup.wstep, setup.gap, setup.nslope, sky_file[b], $
     setup.galfit_out, setup.outcat, setup.outparam, $
     setup.stampfile, global_sky, global_sigsky, $
     setup.convbox, nums, frames, setup.galexe, fittab, b, $
     orgpath_pre, outpath_file, outpath_file_no_band
;spawn, 'touch '+filein+'.skyloop';§§§§§§§§§§§§§§§§§§§§§§
;   print,table[cur].number
   create_mask, table, wht, seg, stamp_param_file, mask_file[b], $
     im_file[b], table[cur].frame[b], cur, $
     setup.neiscl, setup.skyoff, nums, frames, $
     setup.maglim_gal, setup.maglim_star, $
     setup.stel_slope, setup.stel_zp, objects, corner, $
     b
;spawn, 'touch '+filein+'.mask';§§§§§§§§§§§§§§§§§§§§§§
endfor
stop
save, /all, filename='/home/boris/IDL/bridge_save.sav'
restore, '/home/boris/IDL/bridge_save.sav'
  prepare_galfit, setup, objects, setup.files, corner, table, obj_file, $
                   im_file, constr_file, mask_file, chosen_psf_file, $
                   out_file, sky_file, setup.convbox, setup.zp, $
                   setup.platescl, nums, frames, cur, $
                   setup.outcat, setup.outparam, setup.stampfile, $
                   setup.conmaxre, setup.conminm, setup.conmaxm, $
                   fittab, setup.version, nband, orgpre;, n_constrained = n_constrained
stop
;spawn, 'touch '+filein+'.preparegalfit';§§§§§§§§§§§§§§§§§§§§§§

;spawn the script

   cd, orgpath[idx]
   IF setup.nice THEN spawn, 'nice '+setup.galexe+' '+obj_file $
   ELSE spawn, setup.galexe+' '+obj_file
   spawn, 'rm '+orgpath[idx]+'galfit.[0123456789]*'

;   wait, randomu(systime(/seconds))*8+2
;   spawn, 'touch '+out_file

;   file_delete, filein

   wait, 1
END

