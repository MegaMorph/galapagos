PRO stamp_file_bridge, filein
; variables provided in filein:
; i, images, outpath_file, setup, outpath_file_no_band, outpath_band, outpre
  restore, filein
  
  create_stamp_file, images[i,0], $
                     outpath_file[i,0]+setup.outcat, $
                     outpath_file[i,0]+setup.outparam, $
                     outpath_file_no_band[i,0]+setup.stampfile, $
                     setup.stampsize, setup

; set up for cut_list to only cut required subset of postage stamps (to save disk space)
  cat = read_sex_table(outpath_file[i,0]+setup.outcat, outpath_file[i,0]+setup.outparam)
 
  cut_list = intarr(n_elements(cat.number)) 
  IF (setup.srclist EQ '' OR setup.srclistrad LE 0) THEN BEGIN
      cut_list[*] = 1
  ENDIF ELSE BEGIN
      if file_test(setup.srclist) eq 0 then print, 'supplied object file does not exist'
      if file_test(setup.srclist) eq 0 then stop
      readcol, setup.srclist, cut_ra, cut_dec, format='F,F', comment='#', /SILENT
; correlate to sextractor list of this tile

      srccor, cat.alpha_j2000/15., cat.delta_j2000, cut_ra/15., cut_dec, $
        setup.srclistrad, cat_i, cut_i, OPTION=0, /SPHERICAL, /SILENT
      if cat_i[0] ne -1 then cut_list[cat_i] = 1
  ENDELSE
  
  FOR b=1,nband do begin 
      print, 'cutting band '+strtrim(b,2)
      cut_stamps, images[i,b], $
        outpath_file_no_band[i,0]+setup.stampfile, $
        outpath_band[i,b], $
        outpre[i,b], '_'+setup.stamp_pre[b], $
        cut_list
      IF setup.sigflags[b] EQ 1 THEN BEGIN
          print, 'cutting matching sigma maps'
          cut_stamps, setup.sigmaps[i,b], $
            outpath_file_no_band[i,0]+setup.stampfile, $
            outpath_band[i,b], $
            outpre[i,b], '_'+setup.stamp_pre[b]+'_sigma', $
            cut_list
      ENDIF
  ENDFOR
  
  file_delete, filein
  wait, 1
END

