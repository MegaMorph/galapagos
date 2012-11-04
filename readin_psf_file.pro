pro readin_psf_file, files, sex_ra, sex_dec, images, psf_struct, nband
; for each band individually:
; 4 possibilities (for now)
; fits name: sinfilif file[0] eq ' ' then file=file[1:n_elements(file)-1]
; 2 columns: tile: one psf per tile, input list has tilename and psf_file
; 3 columns: closest: file with RA, DEC, psf_file
; 5 columns: box: defined boxes in which to use a psf, file with re_min, ra_max, dec_min, dec_max, pwf_file

; split up input into individual files
; then run procedure for each band, putting everything in ONE structure
files=strtrim(files ,2)
file=' '
REPEAT BEGIN
    if strpos(files, ',') ne -1 then file=[file,strmid(files,0,strpos(files, ','))]
    files=strmid(files,strpos(files, ',')+1)
ENDREP UNTIL strpos(files,',') eq -1
file=[file,files]
;if file[0] eq ' ' then file=file[1:n_elements(file)-1]

if n_elements(file) ne nband+1 then print, 'wrong number of PSFs given in setup file'
if n_elements(file) ne nband+1 then stop

;create array, then go through individual bands and fill the structure
; GET MAXIUMUM NUMBERS OF ENTRIES FIRST!!!
maxdimen=0
FOR b=1,nband DO BEGIN
    ncol=0
    if strmid(file[b], 4,/reverse) eq '.fits' and maxdimen lt 1 then BEGIN
        maxdimen=1
    ENDIF ELSE BEGIN
; check how many input columns
        openr, 1, strtrim(file[b],2)
        ncol=0
        line=''
; look for first valid line and count number of columns
try_again1:
        readf, 1, line     
;get rid of leading and trailing blanks
        line = strtrim(line, 2)
;comment or empty line encountered?
        IF strmid(line, 0, 1) EQ '#' OR strlen(line) EQ 0 THEN goto, try_again1
;comment at end of line?
        pos = strpos(line, '#')
        IF pos EQ -1 THEN pos = strlen(line)
; get columns and number of columns, separated by ' '      
        columns = strsplit(line, ' ', COUNT=ncol)      
        close,1
        
        if ncol eq 2 then readcol, strtrim(file[b],2), tile, psf, format='A,A', comment='#',/silent
        if ncol eq 3 then readcol, strtrim(file[b],2), ra, dec, psf, comment='#', format='D,D,A',/silent
        if ncol eq 5 then readcol, strtrim(file[b],2), ra_min, ra_max, dec_min, dec_max, psf, format='D,D,D,D,A', comment='#',/silent   
        if n_elements(psf) gt maxdimen then maxdimen=n_elements(psf)
    ENDELSE
ENDFOR
create_struct, psf_struct, 'psfstruct', $
  ['type','tile','ra','dec','ra_min','ra_max','dec_min','dec_max','psffile'], $
  'A('+strtrim(nband+1,2)+'),A('+strtrim(nband+1,2)+'),D('+strtrim(nband+1,2)+'),' + $
  'D('+strtrim(nband+1,2)+'),D('+strtrim(nband+1,2)+'),D('+strtrim(nband+1,2)+'),' + $
  'D('+strtrim(nband+1,2)+'),D('+strtrim(nband+1,2)+'),A('+strtrim(nband+1,2)+')',dimen=maxdimen

; now loop over all bands and read_in
for b=1,nband do begin
; in case the psf given is an actual psf image
    if strmid(file[b], 4,/reverse) eq '.fits' then begin
        print, 'PSF: One single PSF was given, this will be used for the entire survey in this band'
;        create_struct, psf_struct, 'single', ['type','psffile'],'A,A',dimen=1
        psf_struct[0].type[b] = 'single'
        psf_struct[0].psffile[b] = strtrim(file[b],2)
    endif else begin

; check how many input columns
     openr, 1, file[b]
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
         readcol, strtrim(file[b],2), tile, psf, format='A,A', comment='#',/silent
;        create_struct, psf_struct, 'tile', ['type','tile', 'psffile'],'A,A,A',dimen=n_elements(psf)
         psf_struct[0:n_elements(tile)-1].type[b] = 'tile'
         psf_struct[0:n_elements(tile)-1].tile[b] = tile
         psf_struct[0:n_elements(tile)-1].psffile[b] = psf
; security check for tile that all tiles have a PSF
; use images[i] to check whether each tile has a PSF
         check=0
         for i=0,n_elements(images[*,b])-1 do begin
             wh = where(strtrim(psf_struct.tile,2) eq strtrim(images[i,b],2), cnt)
             if cnt eq 2 then begin
                 print, strtrim(images[i,b],2)+' has '+strtrim(cnt,2)+' PSFs assigned'
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
         readcol, strtrim(file[b],2), ra, dec, psf, comment='#', format='D,D,A',/silent
;        create_struct, psf_struct, 'closest', ['type','ra', 'dec', 'psffile'],'A,D,D,A',dimen=n_elements(psf)
         psf_struct[0:n_elements(ra)-1].type[b] = 'closest'
         psf_struct[0:n_elements(ra)-1].ra[b] = ra
         psf_struct[0:n_elements(ra)-1].dec[b] = dec
         psf_struct[0:n_elements(ra)-1].psffile[b] = psf        
;         print, n_elements(ra)
     endif
     
; if 4 columns, choose PSF box-wise
     if ncol eq 5 then begin
         print, 'You have chosen to use a PSF per defined box. Now checking whether all areas are covered'
         readcol, strtrim(file[b],2), ra_min, ra_max, dec_min, dec_max, psf, format='D,D,D,D,A', comment='#',/silent
;        create_struct, psf_struct, 'box', ['type','ra_min', 'ra_max', 'dec_min', 'dec_max', 'psffile'],'A,D,D,D,D,A',dimen=n_elements(psf)
         psf_struct[0:n_elements(ra)-1].type = 'box'
         psf_struct[0:n_elements(ra)-1].ra_min[b] = ra_min
         psf_struct[0:n_elements(ra)-1].ra_max[b] = ra_max
         psf_struct[0:n_elements(ra)-1].dec_min[b] = dec_min
         psf_struct[0:n_elements(ra)-1].dec_max[b] = dec_max
         psf_struct[0:n_elements(ra)-1].psffile[b] = psf
; security check for box that all areas are covered
; use sexcat to check whether all objects lie within one of these boxes
         check = intarr(n_elements(sex_ra))
         for i = 0, n_elements(psf_struct.ra_min[b])-1 do begin
             wh = where(sex_ra ge psf_struct[i].ra_min[b] and sex_ra lt psf_struct[i].ra_max[b] and $
                        sex_dec ge psf_struct[i].dec_min[b] and sex_dec lt psf_struct[i].dec_max[b],cnt)
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
     endif
 endelse
endfor 
end
