pro choose_psf, obj_ra, obj_dec, psf_struct, tile, chosen_psf_file, nband
  chosen_psf_file = strarr(nband+1)
  FOR q=0, nband DO chosen_psf_file[q]=' '

  FOR q=1, nband DO BEGIN
; if psf was given as a single file
      if strtrim(psf_struct.type[q],2) eq 'single' then begin
          chosen_psf_file[q] = strtrim(psf_struct.psffile[q,0],2)
      endif
      
; if psf given tile-wise
      if strtrim(psf_struct.type[q],2) eq 'tile' then begin
          valid = where(strtrim(psf_struct.psffile[q,*],2) ne '')
          wh=where(strtrim(psf_struct.tile[q,valid],2) eq strtrim(tile,2))
          chosen_psf_file[q] = strtrim(psf_struct.psffile[q,valid[wh]],2)
      endif  
      
; if psf is given in ra,dec
      if strtrim(psf_struct.type[q],2) eq 'closest' then begin     
          ord=0
          valid = where(strtrim(psf_struct.psffile[q,*],2) ne '')
          gcirc, 1, psf_struct.ra[q,valid]/15., psf_struct.dec[q,valid], obj_ra/15., obj_dec, dist
          ord = sort(dist)
          chosen_psf_file[q] = strtrim(psf_struct.psffile[q,valid[ord[0]]],2)
;          chosen_psf_file[q] = strtrim(psf_struct.psffile[q,0],2)

;          valid = where(strtrim(psf_struct.psffile[q,*],2) ne '')
;          srccor, obj_ra/15., obj_dec,  psf_struct.ra[q,valid]/15., psf_struct.dec[q,valid],1, ob_i, psf_i, OPTION=2, /SPHERICAL, /SILENT
;          chosen_psf_file[q] = strtrim(psf_struct.psffile[q,valid[psf_i]],2)
      endif
      
  ENDFOR

end
