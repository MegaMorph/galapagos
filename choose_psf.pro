pro choose_psf, obj_ra, obj_dec, psf_struct, tile, chosen_psf_file
  chosen_psf_file = ' '

; if psf was given as a simngle file
  if strtrim(psf_struct[0].type,2) eq 'single' then begin
     chosen_psf_file = strtrim(psf_struct[0].psffile,2)
  endif
  
; if psf given tile-wise
  if strtrim(psf_struct[0].type,2) eq 'tile' then begin
     wh=where(strtrim(psf_struct.tile,2) eq strtrim(tile,2))
     chosen_psf_file = strtrim(psf_struct[wh].psffile,2)
  endif  
  
; if psf is given in ra,dec
  if strtrim(psf_struct[0].type,2) eq 'closest' then begin     
     gcirc, 1, psf_struct.ra/15., psf_struct.dec, obj_ra/15., obj_dec, dist
     ord = sort(dist)
     chosen_psf_file = strtrim(psf_struct[ord[0]].psffile,2)
  endif
  
; if psf is given in boxes
  if strtrim(psf_struct[0].type,2) eq 'box' then begin
     wh = where(psf_struct.ra_min le obj_ra and psf_struct.ra_max gt obj_ra and $
                psf_struct.dec_min lt obj_dec and psf_struct.dec_max gt obj_dec ,cnt)
     if cnt gt 1 then begin
        box_mid_ra = (psf_struct.ra_min+psf_struct.ra_max)/2.
        box_mid_dec = (psf_struct.dec_min+psf_struct.dec_max)/2.
        gcirc, 1, box_mid_ra[wh]/15., box_mid_dec[wh], obj_ra/15., obj_dec, dist
        ord = sort(dist)
        chosen_psf_file = strtrim(psf_struct[wh[ord[0]]].psffile,2)
     endif 
     if cnt eq 1 then chosen_psf_file = psf_struct[wh].psffile
     if cnt eq 0 then begin
        box_mid_ra = (psf_struct.ra_min+psf_struct.ra_max)/2.
        box_mid_dec = (psf_struct.dec_min+psf_struct.dec_max)/2.
        gcirc, 1, box_mid_ra/15., box_mid_dec, obj_ra/15., obj_dec, dist
        ord = sort(dist)
        chosen_psf_file = strtrim(psf_struct[ord[0]].psffile,2)
     endif 
  endif  
end
