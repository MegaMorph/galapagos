pro create_output_from_fits, fits, layers, gal_exe, namepost=namepost, overwrite=overwrite,adapt=adapt
; .run create_output_from_fits.pro
; create_output_from_fits, '/Users/haeussler/Documents/Dropbox/Arianna_Fits/414.556/414.556_gf.fits','blank,input,model,residual,psf','~/megamorph/galfit/exec/galfitm-1.2.0-osx'

; WARNING! Always uses LAST *galfit.??.band file, so make sure that
; this is what you meant. This is of course also the file matching the
; fits file itself.
  
; THIS CODE
; use *galfit.01 obj file
; run galfit with all parameters fixed!
; galfit -o option?

; BETTER WOULD BE
; read results from fits output file itself instead of fake obj file
; run galfit with all parameters fixed!
; THIS would also be independent of this *galfit.??.band file existing and would only depend on the fits file itself
; much more complicated to make that file, though, as the header is
; complicated and can contain multiple objects.
  if not keyword_set(namepost) then namepost = 'changed_content'
  
  spawn, 'pwd', infolder
  IF strpos(fits,'/') NE -1 THEN BEGIN
     workfolder = strmid(fits,0,strpos(fits,'/',/REVERSE_SEARCH))
     CD, workfolder
     fits = strmid(fits,strpos(fits,'/',/REVERSE_SEARCH)+1)
  ENDIF

; find all matching galfit.??.band files and select newest
  spawn, 'ls '+strmid(fits,0,strpos(fits,'.fits'))+'.galfit.*', list

; throw away all the files ending in 'band' or 'output'
  list = list[where(strmid(list,4,/reverse_offset) NE '.band')]
  list = list[where(strmid(list,6,/reverse_offset) NE '_output')]
  IF keyword_set(adapt) THEN list = list[where(strmid(list,5,/reverse_offset) EQ '_adapt')]

  list2 = list
; isolate counting number
  FOR i=0,n_elements(list)-1 DO BEGIN
     list2[i] = strmid(list[i],strpos(list[i],'.',/reverse_search)+1)
  ENDFOR

  list2 = fix(list2)
; select latest file (file with highest number)
  wh = where(list2 EQ max(list2))
  obj = list[wh]

  print, 'using file '+strtrim(obj,2)+' to start from'
  
; go through setup file one line at a time
; change output file format and set all DOG == 0!!
  openr, filer, obj, /get_lun
  line = ''

  newfile = obj+'_output'
  openw, filew, newfile, /get_lun

; go through setup file line by line
  WHILE ~ EOF(filer) DO BEGIN
; Read a line of text:
     readf, filer, line
; split up into parts
     content_numbers = ' '
     content_descriptor = ' '
     start = strtrim(strmid(line,0,strpos(line,')')+2),2)
     content = strtrim(strmid(line,strpos(line,')')+2, strpos(line,'#')-strpos(line,')')-2),2)
     comment = strtrim(strmid(line,strpos(line,'#')),2)
     

; if line is normal setup line
     IF strpos(content,',') EQ -1 AND strtrim(line,2) NE '# INITIAL FITTING PARAMETERS' THEN BEGIN
; change output file name if requested
        IF strpos(strtrim(line, 2), 'B) ') EQ 0 THEN BEGIN
           IF keyword_set(overwrite) THEN outfile_name = content
           IF NOT keyword_set(overwrite) THEN outfile_name = strmid(content,0,strpos(content,'.fits'))+'_'+namepost+'.fits'
           printf, filew, start+' '+outfile_name+'            '+comment
        ENDIF ELSE IF strpos(strtrim(line, 2), 'W) ') EQ 0 THEN BEGIN
           printf, filew, start+' '+layers+'       '+comment
        ENDIF ELSE printf, filew, line
     ENDIF
     
; if line is mwl setup line
     IF strpos(content,',') NE -1 AND strpos(content,'cheb') EQ -1  THEN BEGIN
        content_elements = strsplit(content,',',/extract)
        printf, filew, line
     ENDIF

; if line is mwl parameter line
     IF strpos(content,',') NE -1 AND strpos(content,'cheb') NE -1  THEN BEGIN
        content_numbers = strtrim(strmid(content,0,strpos(content,' ')),2)
        content_dof = strtrim(strmid(content,strpos(content,' ')),2)
; change DOG
        printf, filew, start+' '+content_numbers+' 0 cheb '+comment
     ENDIF

  ENDWHILE
  close, filer
  close, filew
  FREE_LUN, filer
  FREE_LUN, filew

; run fit with free parameters
  spawn, gal_exe+' '+newfile
;  spawn, 'rm fit.log '+newfile

  CD, infolder
 
end
