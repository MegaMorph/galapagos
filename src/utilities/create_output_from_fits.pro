pro create_output_from_fits, fits, layers, gal_exe, comp_req=comp_req, namepost=namepost, overwrite=overwrite,adapt=adapt
; .run create_output_from_fits.pro
; create_output_from_fits, '/Users/haeussler/Documents/Dropbox/Arianna_Fits/414.556/414.556_gf.fits','blank,input,model,residual,psf','~/megamorph/galfit/exec/galfitm-1.2.0-osx'
; create_output_from_fits, '/lustre/home/bhaeussl/GAMA/gala/G09/tile16_7/galfit/t16_7.7006_gf.fits','blank,input,model,residual,psf','/lustre/home/bhaeussl/megamorph/galfit/exec/galfitm-1.2.1-linux-x86_64'

; WARNING! Always uses LAST *galfit.??.band file, so make sure that
; this is what you meant. This is of course also the file matching the
; fits file itself.
  
; THIS CODE
; use *galfit.01 obj file (or *galfit.01_adapt if keyword /adapt is used) 
; run galfit with all parameters fixed!

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
  spawn, 'ls '+strmid(fits,0,strpos(fits,'.fits'))+'.galfit.??', list

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

  print_flag = 1
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

; check whether this object is a component not to be printed.
; check for 'object number' line
     IF strpos(strtrim(line, 2), '# Component number') EQ 0 AND keyword_set(comp_req) THEN BEGIN
; extract actual number
        comp_number = fix(strtrim(strmid(line, 19),2))
; set print flag on
        print_flag = 1
; check whether component is requested
; switch print_flag off if object not required, until next line
        IF where(comp_req eq comp_number) EQ -1 THEN BEGIN
           print_flag=0
        ENDIF
     ENDIF
     
     IF print_flag EQ 1 THEN BEGIN
; if line is normal setup line
        IF strpos(content,',') EQ -1 AND strtrim(line,2) NE '# INITIAL FITTING PARAMETERS' THEN BEGIN
; change output file name if requested
           IF strpos(strtrim(line, 2), 'B) ') EQ 0 THEN BEGIN
              IF keyword_set(overwrite) THEN outfile_name = content
              IF NOT keyword_set(overwrite) THEN outfile_name = strmid(content,0,strpos(content,'.fits'))+'_'+namepost+'.fits'
              printf, filew, start+' '+outfile_name+'            '+comment
           ENDIF ELSE IF strpos(strtrim(line, 2), 'G) ') EQ 0 THEN BEGIN
              printf, filew, start+' none           '+comment
           ENDIF ELSE IF strpos(strtrim(line, 2), 'W) ') EQ 0 THEN BEGIN
              printf, filew, start+' '+layers+'       '+comment
           ENDIF ELSE printf, filew, line
        ENDIF
        
; if line is mwl setup line
        IF strpos(content,',') NE -1 AND strpos(content,'cheb') EQ -1  THEN BEGIN
           content_elements = strsplit(content,',',/extract)
           IF strpos(strtrim(line, 2), 'W) ') EQ 0 THEN BEGIN
              printf, filew, start+' '+layers+'       '+comment
           ENDIF ELSE  printf, filew, line
        ENDIF
        
; if line is mwl parameter line
        IF strpos(content,',') NE -1 AND strpos(content,'cheb') NE -1  THEN BEGIN
           content_numbers = strtrim(strmid(content,0,strpos(content,' ')),2)
           content_dof = strtrim(strmid(content,strpos(content,' ')),2)
; change DOG
           printf, filew, start+' '+content_numbers+' 0 cheb '+comment
        ENDIF

     ENDIF  ; end print_flag
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

PRO display_all_fits_from_folder,folder
; .run create_output_from_fits.pro
; display_all_fits_from_folder, '/home/bhaeussl/GAMA/gala/G12/tile10_5/galfit'
;display_all_fits_from_folder, '/home/bhaeussl/GAMA/gala/G12/tile10_5/galfit_bd1'
;display_all_fits_from_folder, '/home/bhaeussl/GAMA/gala/G12/tile10_5/galfit_bd4'

;; SciGarn
;  ds9_exe = '/lustre/opsw/software/ds9-7.7.5/ds9'
;  galfit_exe='/lustre/home/bhaeussl/megamorph/galfit/exec/galfitm-1.2.1-linux-x86_64'

; Chapman
  ds9_exe = ' /usr/local/bin/ds9'
  galfit_exe='/home/bhaeussl/megamorph/galfit/exec/galfitm-1.2.1-linux-x86_64'

  spawn, 'pwd', startfolder  

; get all objects. Get them in order of file age to diplay newest fits first!
  CD, folder
  spawn, 'ls -t *gf.fits', galfit_out_files
  
  openw, sf, 'display_files', /get_lun
; get new output names
  galfit_new_out_files = galfit_out_files
  FOR j=0, n_elements(galfit_new_out_files)-1 DO $
     galfit_new_out_files[j] = strmid(galfit_new_out_files[j],0,strpos(galfit_new_out_files[j],'.fits'))+'_changed_content.fits'

; loop over all objects to ...
  FOR i=0,n_elements(galfit_out_files)-1 DO BEGIN
     
; ... create new output file
     create_output_from_fits, galfit_out_files[i],'input,model,residual',galfit_exe

; ... display the new output
     printf, sf, ds9_exe +' -multiframe -log '+galfit_new_out_files[i]
  ENDFOR
  close, sf
  free_lun, sf
  
  CD, startfolder  
END

PRO change_output_for_entire_catalogue, catalogue, layers, gal_exe, bd =bd, comp_req = comp_req, namepost=namepost, overwrite=overwrite
; pro create_output_from_fits, fits, layers, gal_exe, namepost=namepost, overwrite=overwrite,adapt=adapt
; .run create_output_from_fits.pro
; change_output_for_entire_catalogue, '/home/kalina/old_home_link/galaOut/abell370_updatedImages_7band/abell370_updatedImages_7band_gala_dVBulge.fits','blank,input,model,residual,psf','~/megamorph/galfit/exec/galfitm-1.2.1-linux-x86_64'
; change_output_for_entire_catalogue, '/home/kalina/old_home_link/galaOut/abell370_updatedImages_7band/abell370_updatedImages_7band_gala_dVBulge.fits','input,model,residual,component','~/megamorph/galfit/exec/galfitm-1.2.1-linux-x86_64',comp_req=[2,3],namepost='new_content'

  cat =  mrdfits(catalogue,1)

  FOR i=0,n_elements(cat.number)-1 DO BEGIN
     IF NOT keyword_set(bd) THEN BEGIN
        IF strtrim(cat[i].file_galfit,2) NE '' THEN BEGIN
           print, 'creating new output for file '+cat[i].file_galfit
           create_output_from_fits, cat[i].file_galfit, layers, gal_exe, $
                                    comp_req=comp_req, namepost=namepost, overwrite=overwrite
        ENDIF
     ENDIF
     IF keyword_set(bd) THEN BEGIN
        IF strtrim(cat[i].file_galfit_bd,2) NE '' THEN BEGIN
           print, 'creating new output for file '+cat[i].file_galfit
           create_output_from_fits, cat[i].file_galfit_bd, layers, gal_exe, $
                                    comp_req=comp_req, namepost=namepost, overwrite=overwrite
        ENDIF
     ENDIF
     
  ENDFOR
END

;pro find_fits_limit
;  arr = intarr(99,99)
;  arr[49,49]=1
;
;  mwrfits,arr,'~/limit.fits',/create
;  head = headfits('~/limit.fits',exten=1)
;  for i=1,500 do begin
;     print, 'testing layer '+strtrim(i,2)
;     mwrfits,arr,'~/limit.fits',/silent
;  endfor
;
;
;end
