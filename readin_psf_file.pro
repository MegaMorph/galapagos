pro readin_psf_file, file, sex_ra, sex_dec, images, psf_struct
; 4 possibilities (for now)
; fits name: single PSF file
; 2columns: tile: one psf per tile, input list has tilename and psf_file
; 3 columns: closest: file with RA, DEC, psf_file
; 5 columns: box: defined boxes in which to use a psf, file with re_min, ra_max, dec_min, dec_max, pwf_file

  file=strtrim(file ,2)
; in case the psf given is an actual psf image
  if strmid(file, 4,/reverse) eq '.fits' then begin
     print, 'PSF: One single PSF was given, this will be used for the entire survey'
     create_struct, psf_struct, 'single', ['type','psffile'],'A,A',dimen=1
     psf_struct.type = 'single'
     psf_struct.psffile = file
  endif else begin

; check how many input columns
     openr, 1, file
     ncol=0
     line=''
; look for first valid line and count number of columns
try_again:
     readf, 1, line        
;get rid of leading and trailing blanks
     line = strtrim(line, 2)
;comment or empty line encountered?
     IF strmid(line, 0, 1) EQ '#' OR strlen(line) EQ 0 THEN goto, try_again
;comment at end of line?
     pos = strpos(line, '#')
     IF pos EQ -1 THEN pos = strlen(line)
; get columns and number of columns, separated by ' '      
     columns = strsplit(line, ' ', COUNT=ncol)      
     close,1

; if 2 columns, choose PSF tile-wise
     if ncol eq 2 then begin
        print, 'You have chosen to use one PSF per input tile, now checking whether all PSFs are defined'
        readcol, file, tile, psf, format='A,A', comment='#',/silent
        create_struct, psf_struct, 'tile', ['type','tile', 'psffile'],'A,A,A',dimen=n_elements(psf)
        psf_struct.type = 'tile'
        psf_struct.tile = tile
        psf_struct.psffile = psf
; security check for tile that all tiles have a PSF
; use images[i] to check whether each tile has a PSF
        check=0
        for i=0,n_elements(images)-1 do begin
           wh = where(strtrim(psf_struct.tile,2) eq strtrim(images[i],2), cnt)
           if cnt eq 2 then begin
              print, strtrim(images[i],2)+' has '+strtrim(cnt,2)+' PSFs assigned'
              stop
           endif
           if cnt eq 0 then check = 1
        endfor
        if check eq 1 then begin
           print, 'Not all tiles have an assigned PSF file'
           stop
        endif
        if check eq 0 then print, 'All tiles have an assigned PSF file'
     endif
     
; if 3 columns, choose closest PSF
     if ncol eq 3 then begin
        print, 'You have chosen to use a series of PSFs and the closest PSF to an object is chosen'
        readcol, file, ra, dec, psf, comment='#', format='D,D,A',/silent
        create_struct, psf_struct, 'closest', ['type','ra', 'dec', 'psffile'],'A,D,D,A',dimen=n_elements(psf)
        psf_struct.type = 'closest'
        psf_struct.ra = ra
        psf_struct.dec = dec
        psf_struct.psffile = psf        
     endif
     
; if 4 columns, choose PSF box-wise
     if ncol eq 5 then begin
        print, 'You have chosen to use a PSF per defined box. Now checking whether all areas are covered'
        readcol, file, ra_min, ra_max, dec_min, dec_max, psf, format='D,D,D,D,A', comment='#',/silent
        create_struct, psf_struct, 'box', ['type','ra_min', 'ra_max', 'dec_min', 'dec_max', 'psffile'],'A,D,D,D,D,A',dimen=n_elements(psf)
        psf_struct.type = 'box'
        psf_struct.ra_min = ra_min
        psf_struct.ra_max = ra_max
        psf_struct.dec_min = dec_min
        psf_struct.dec_max = dec_max
        psf_struct.psffile = psf
; security check for box that all areas are covered
; use sexcat to check whether all objects lie within one of these boxes
        check = intarr(n_elements(sex_ra))
        for i = 0, n_elements(psf_struct.ra_min)-1 do begin
           wh = where(sex_ra ge psf_struct[i].ra_min and sex_ra lt psf_struct[i].ra_max and $
                      sex_dec ge psf_struct[i].dec_min and sex_dec lt psf_struct[i].dec_max,cnt)
           if cnt ne 0 then check[wh] = check[wh]+1
        endfor 
        checkhelp = where(check ge 2, checkn)
        if checkn ge 1 then begin
           print, 'some objects have 2 PSFs defined, for these, the closer one will be chosen'
        endif
        checkhelp = where(check ne 1, checkn)
        if checkn eq 0 then print, 'All objects lie within the defined PSF boxes'
        checkhelp = where(check eq 0, checkn)
        if checkn ne 0 then begin
           print, 'WARNING, not all objects lie within the defined boxes, for the ones outside, the closest psf (defined by box-center) will be chosen'
        endif
 ;       print,check 
     endif
  endelse

end
