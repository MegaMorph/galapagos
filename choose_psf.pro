pro choose_psf, obj_ra, obj_dec, psf_struct, tile, chosen_psf_file, nband
  chosen_psf_file = strarr(nband+1)
  FOR q=0, nband DO chosen_psf_file[q]=' '

  FOR q=1, nband DO BEGIN
; if psf was given as a single file
      if strtrim(psf_struct[0].type[q],2) eq 'single' then begin
          chosen_psf_file[q] = strtrim(psf_struct[0].psffile[q],2)
      endif
      
; if psf given tile-wise
      if strtrim(psf_struct[0].type[q],2) eq 'tile' then begin
          valid = where(strtrim(psf_struct.psffile[q],2) ne '')
          wh=where(strtrim(psf_struct[valid].tile[q],2) eq strtrim(tile,2))
          chosen_psf_file[q] = strtrim(psf_struct[valid[wh]].psffile[q],2)
      endif  
      
; if psf is given in ra,dec
      if strtrim(psf_struct[0].type[q],2) eq 'closest' then begin     
          valid = where(strtrim(psf_struct.psffile[q],2) ne '')
          gcirc, 1, psf_struct[valid].ra/15., psf_struct[valid].dec, obj_ra/15., obj_dec, dist
          ord = sort(dist)
          chosen_psf_file[q] = strtrim(psf_struct[valid[ord[0]]].psffile[q],2)
      endif
      
; if psf is given in boxes
      if strtrim(psf_struct[0].type[q],2) eq 'box' then begin
          valid = where(strtrim(psf_struct.psffile[q],2) ne '')
          wh = where(psf_struct[valid].ra_min[q] le obj_ra and psf_struct[valid].ra_max[q] gt obj_ra and $
                     psf_struct[valid].dec_min[q] lt obj_dec and psf_struct[valid].dec_max[q] gt obj_dec ,cnt)
          if cnt gt 1 then begin
              box_mid_ra = (psf_struct[valid].ra_min[q]+psf_struct[valid].ra_max[q])/2.
              box_mid_dec = (psf_struct[valid].dec_min[q]+psf_struct[valid].dec_max[q])/2.
              gcirc, 1, box_mid_ra[valid[wh]]/15., box_mid_dec[valid[wh]], obj_ra/15., obj_dec, dist
              ord = sort(dist)
              chosen_psf_file = strtrim(psf_struct[valid[wh[ord[0]]]].psffile[q],2)
          endif 
          if cnt eq 1 then chosen_psf_file[q] = psf_struct[valid[wh]].psffile[q]
          if cnt eq 0 then begin
              box_mid_ra = (psf_struct[valid].ra_min[q]+psf_struct[valid].ra_max[q])/2.
              box_mid_dec = (psf_struct[valid].dec_min[q]+psf_struct[valid].dec_max[q])/2.
              gcirc, 1, box_mid_ra/15., box_mid_dec, obj_ra/15., obj_dec, dist
              ord = sort(dist)
              chosen_psf_file[q] = strtrim(psf_struct[valid[ord[0]]].psffile[q],2)
          endif 
      endif  
  ENDFOR

end
