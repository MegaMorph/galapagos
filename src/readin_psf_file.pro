pro readin_psf_file, files, images, psf_struct, nband, save_folder
; for each band individually:
; 4 possibilities (for now)
; fits name: sinfilif file[0] eq ' ' then file=file[1:n_elements(file)-1]
; 2 columns: tile: one psf per tile, input list has tilename and psf_file
; 3 columns: closest: file with RA, DEC, psf_file

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

; check whether all files exist
FOR b=1,nband DO BEGIN
    if file_test(file[b]) eq 0 then begin
        print, 'PSF input file '+file[b]+' not found'
        stop
    ENDIF
ENDFOR

;create array, then go through individual bands and fill the structure
; GET MAXIUMUM NUMBERS OF ENTRIES FIRST!!!
maxdimen=0
FOR b=1,nband DO BEGIN
    ncol=0
    if strmid(strtrim(file[b],2), 4,/reverse) eq '.fits' and maxdimen le 1 then BEGIN
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
        
        if NOT (ncol eq 2 or ncol eq 3) then begin
            print, 'something wrong in your input lists. I can not count 2 or 3 columns'
            print, 'are you sure the spaces i the list are spaces and not e.g. TABS?'
            stop
        endif
        
        if ncol eq 2 then readcol, strtrim(file[b],2), tile, psf, format='A,A', comment='#',/silent
        if ncol eq 3 then readcol, strtrim(file[b],2), ra, dec, psf, comment='#', format='D,D,A',/silent
        if n_elements(psf) gt maxdimen then maxdimen=n_elements(psf)
    ENDELSE
ENDFOR
psf_struct = create_struct('type', strarr(nband+1),'tile', strarr(nband+1,maxdimen),'ra',dblarr(nband+1,maxdimen), $
                           'dec',dblarr(nband+1,maxdimen), 'psffile', strarr(nband+1,maxdimen))

; now loop over all bands and read_in
for b=1,nband do begin
; in case the psf given is an actual psf image
    for f = 1, n_elements(file)-1 do spawn, 'cp '+file[f]+' '+save_folder
    if strmid(file[b], 4,/reverse) eq '.fits' then begin
        print, 'PSF: One single PSF was given, this will be used for the entire survey in this band'
        psf_struct.type[b] = 'single'
        psf_struct.psffile[b,0] = strtrim(file[b],2)
        if file_test(strtrim(file[b],2)) ne 1 then print, 'file '+strtrim(file[b],2)+' does not exist'
        if file_test(strtrim(file[b],2)) ne 1 then stop
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
            psf_struct.type[b] = 'tile'
            psf_struct.tile[b,0:n_elements(tile)-1] = tile
            psf_struct.psffile[b,0:n_elements(tile)-1] = psf
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
            for p=0,n_elements(psf)-1 do begin
                if file_test(strtrim(psf[p],2)) ne 1 then print, 'file '+strtrim(psf[p],2)+' (and maybe others) does not exist, is your path in the PSF files set correctly?'
                if file_test(strtrim(psf[p],2)) ne 1 then stop
            endfor
        endif
        
; if 3 columns, choose closest PSF
        if ncol eq 3 then begin
            print, 'You have chosen to use a series of PSFs and the closest PSF to an object is chosen'
            readcol, strtrim(file[b],2), ra, dec, psf, comment='#', format='D,D,A',/silent
            psf_struct.type[b] = 'closest'
            psf_struct.ra[b,0:n_elements(ra)-1] = ra
            psf_struct.dec[b,0:n_elements(ra)-1] = dec
            psf_struct.psffile[b,0:n_elements(ra)-1] = psf        
;         print, n_elements(ra)
            for p=0,n_elements(psf)-1 do begin
                if file_test(strtrim(psf[p],2)) ne 1 then print, 'file '+strtrim(psf[p],2)+' (and maybe others) does not exist, is your path in the PSF files set correctly?'
                if file_test(strtrim(psf[p],2)) ne 1 then stop
            endfor
        endif
        
    endelse

endfor 
print, 'ALL PSFs SUCCESFULLY READ IN (and checked to exist)'
end
