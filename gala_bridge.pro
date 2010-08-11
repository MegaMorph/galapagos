PRO gala_bridge, filein
;variables provided in filein:
;cur, orgwht, idx, orgpath, orgpre, setup, psf, chosen_psf_file, sky_file, 
;nums, frames, stamp_param_file, mask_file, im_file, obj_file, 
;constr_file, mask_file, out_file, fittab

   restore, filein
;spawn, 'touch '+filein+'.restored';§§§§§§§§§§§§§§§§§§§§§§

   table = mrdfits(setup.outdir+setup.sexcomb+'.ttmp', 1)

;read in image and weight (takes 15sec)
;   print, systime(), ' reading images...'
   fits_read, table[cur].frame, im, hd
   fits_read, orgwht[idx], wht, whd

;image size
   sz_im = (size(im))[1:2]

;read segmentation map (needed for excluding neighbouring sources)
   fits_read, orgpath[idx]+orgpre[idx]+setup.outseg, seg

;read the skymap
   fits_read, orgpath[idx]+orgpre[idx]+setup.skymap+'.fits', map
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
   getsky_loop, cur, table, rad, im, hd, map, setup.expt, $
                setup.zp, setup.neiscl, setup.skyoff, setup.power, $
                setup.cut, setup.files, psf, setup.dstep, $
                setup.wstep, setup.gap, setup.nslope, sky_file, $
                setup.galfit_out, setup.outcat, setup.outparam, $
                setup.stampfile, global_sky, global_sigsky, $
                setup.convbox, nums, frames, setup.galexe, fittab
;spawn, 'touch '+filein+'.skyloop';§§§§§§§§§§§§§§§§§§§§§§

   print,table[cur].number
   create_mask, table, wht, seg, stamp_param_file, mask_file, $
                im_file, table[cur].frame, cur, $
                setup.neiscl, setup.skyoff, nums, frames, $
                setup.maglim_gal, setup.maglim_star, $
                setup.stel_slope, setup.stel_zp, objects, corner
;spawn, 'touch '+filein+'.mask';§§§§§§§§§§§§§§§§§§§§§§

   prepare_galfit, objects, setup.files, corner, table, obj_file, $
                   im_file, constr_file, mask_file, chosen_psf_file, $
                   out_file, sky_file, setup.convbox, setup.zp, $
                   setup.platescl, nums, frames, cur, $
                   setup.outcat, setup.outparam, setup.stampfile, $
                   setup.conmaxre, setup.conminm, setup.conmaxm, $
                   fittab, setup.version;, n_constrained = n_constrained
;spawn, 'touch '+filein+'.preparegalfit';§§§§§§§§§§§§§§§§§§§§§§

;spawn the script

   cd, orgpath[idx]
   IF setup.nice THEN spawn, 'nice '+setup.galexe+' '+obj_file $
   ELSE spawn, setup.galexe+' '+obj_file
   spawn, 'rm '+orgpath[idx]+'galfit.[0123456789]*'

;   wait, randomu(systime(/seconds))*8+2
;   spawn, 'touch '+out_file

   file_delete, filein

;file_delete, filein+'.restored';§§§§§§§§§§§§§§§§§§§§§§
;file_delete, filein+'.sky';§§§§§§§§§§§§§§§§§§§§§§
;file_delete, filein+'.skyloop';§§§§§§§§§§§§§§§§§§§§§§
;file_delete, filein+'.mask';§§§§§§§§§§§§§§§§§§§§§§
;file_delete, filein+'.preparegalfit';§§§§§§§§§§§§§§§§§§§§§§

   wait, 1
END

