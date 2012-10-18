PRO skymap_bridge, filein
; variables provided in filein:
; i, weights, outpath_file, setup, outpath_file_no_band
restore, filein

FOR b=1,nband do begin
    create_skymap, weights[i,b], $
      outpath_file[i,0]+setup.outseg, $
      outpath_file[i,0]+setup.outcat, $
      outpath_file[i,0]+setup.outparam, $
      outpath_file_no_band[i,b]+setup.stamp_pre[b]+'.'+setup.skymap, $
      setup.skyscl, setup.skyoff
ENDFOR

file_delete, filein
wait, 1
END
