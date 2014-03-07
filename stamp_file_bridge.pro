PRO stamp_file_bridge, filein
; variables provided in filein:
; i, images, outpath_file, setup, outpath_file_no_band, outpath_band, outpre
  restore, filein
  
  create_stamp_file, images[i,0], $
                     outpath_file[i,0]+setup.outcat, $
                     outpath_file[i,0]+setup.outparam, $
                     outpath_file_no_band[i,0]+setup.stampfile, $
                     setup.stampsize, setup

  FOR b=1,nband do begin 
      print, 'cutting band '+strtrim(b,2)
      cut_stamps, images[i,b], $
        outpath_file_no_band[i,0]+setup.stampfile, $
        outpath_band[i,b], $
        outpre[i,b], '_'+setup.stamp_pre[b]
      IF setup.sigflags[b] EQ 1 THEN BEGIN
          print, 'cutting matching sigma maps'
          cut_stamps, setup.sigmaps[i,b], $
            outpath_file_no_band[i,0]+setup.stampfile, $
            outpath_band[i,b], $
            outpre[i,b], '_'+setup.stamp_pre[b]+'_sigma'
      ENDIF
  ENDFOR
  
  file_delete, filein
  wait, 1
END

