PRO derive_primary_chi2, obj_file, gal_exe
; .run derive_primary_chi2.pro
; derive_primary_chi2, '~/primary_package/t11_4.1793/t11_4.1793_gf.galfit.01_adapt','/home/bhaeussl/megamorph/galfit/exec/galfitm-1.2.1-linux-x86_64'
  obj=obj_file
  
; change work directory if necessary
  spawn, 'pwd', infolder
  IF strpos(obj,'/') NE -1 THEN BEGIN
     workfolder = strmid(obj,0,strpos(obj,'/',/REVERSE_SEARCH))
     CD, workfolder
     obj = strmid(obj,strpos(obj,'/',/REVERSE_SEARCH)+1)
  ENDIF

; count number of bands first and get band names
  openr, filer, obj, /get_lun
  line = ''
  REPEAT BEGIN
     readf, filer, line
  ENDREP UNTIL strpos(strtrim(line, 2), 'A) ') EQ 0
; jump to line A1)
  readf, filer, line
  content = strtrim(strmid(line,strpos(line,')')+2, strpos(line,'#')-strpos(line,')')-2),2)
  bandnames = strsplit(content,',',/extract)
  nband = n_elements(bandnames)
  close, filer
  free_lun, filer

;start reading input file line by line
  openr, filer, obj, /get_lun
  line = ''
  
  newfile = obj+'_with_primary_mask'
  openw, filew, newfile, /get_lun

  WHILE ~ EOF(filer) DO BEGIN
; Read a line of text:
     readf, filer, line
; split up into parts
     content_numbers = ' '
     content_descriptor = ' '
     start = strtrim(strmid(line,0,strpos(line,')')+2),2)
; if comment behind the content

     IF strpos(line,'#') NE -1 THEN BEGIN
        content = strtrim(strmid(line,strpos(line,')')+2, strpos(line,'#')-strpos(line,')')-2),2)
        comment = strtrim(strmid(line,strpos(line,'#')),2)
     ENDIF ELSE BEGIN
; if no comment is there
        content = strtrim(strmid(line,strpos(line,')')+2),2)
        comment = ' '
     ENDELSE

; change output file name
     IF strpos(strtrim(line, 2), 'B) ') EQ 0 THEN BEGIN
        new_output_name = strmid(content,0,strpos(content,'.fits'))+'_primary_only.fits'
        printf, filew, 'B) '+new_output_name
; change masks used and create new masks that combine primary and normal mask!
     ENDIF ELSE IF strpos(strtrim(line, 2), 'F) ') EQ 0 THEN BEGIN

; create new masks
; find primary mask first
        content_elements = strsplit(content,',',/extract)
        file_folder = strmid(content_elements[0],0,strpos(content_elements[0],'/',/reverse_search)+1) 
        file_name = strmid(content_elements[0],strlen(file_folder),strlen(content_elements[0]))
        file_base = strmid(file_name,0,strpos(file_name,'_mask.fits'))
        file_base = strmid(file_base,0,strpos(file_base,'_',/reverse_search))
        pmask_file = file_folder+file_base+'_mask_primary.fits'
        pmask = mrdfits(pmask_file,0,/silent)

        new_masks_all = ''
; now create new mask per band
        FOR b=0,nband-1 DO BEGIN
           file_folder = strmid(content_elements[b],0,strpos(content_elements[b],'/',/reverse_search)+1) 
           file_name = strmid(content_elements[b],strlen(file_folder),strlen(content_elements[b]))
           file_base = strmid(file_name,0,strpos(file_name,'_mask.fits'))
           file_base = strmid(file_base,0,strpos(file_base,'_',/reverse_search))
           new_mask_file = file_folder+'primary_'+file_name

           mask = mrdfits(content_elements[b],0,maskhd,/silent)
           new_mask = mask-mask+1
; do NOT mask primary!
           new_mask[where(pmask EQ 1)] = 0
; mask everything that is masked in original mask!
           omasked=where(mask NE 0, cntom)
           IF cntom GE 1 then new_mask[omasked] = 1
           writefits, new_mask_file, new_mask, maskhd,/silent           
           new_masks_all = new_masks_all+new_mask_file+','
        ENDFOR
; put new masks in galfit file
; cut trailing ','
        new_masks_all = strmid(new_masks_all,0,strlen(new_masks_all)-1)
        printf, filew, 'F) '+new_masks_all
; change content of ouput file to ALWAYS only contain tables, no images
     ENDIF ELSE IF strpos(strtrim(line, 2), 'W) ') EQ 0 THEN BEGIN
        printf, filew, start+'  '+comment
; for this to work, option p) has to be set ==2, command line option -o2 does not work 
     ENDIF ELSE IF strpos(strtrim(line, 2), 'P) ') EQ 0 THEN BEGIN
        printf, filew, start+' 0 '+comment
        
; if any other line, simply print out
     ENDIF ELSE printf, filew, line
  
  ENDWHILE
  close, filer
  close, filew
  free_lun, filer
  free_lun, filew

; run fits in option galfit -o3
  spawn, gal_exe+' -o2 '+newfile +' >null'

; read out fit_info of new fit, and write wanted value into a little ascii file 
  fit_info_prime = mrdfits(new_output_name,'FIT_INFO',/silent)

  openw, filew_prime, obj_file+'_primary_fit_info', /get_lun
  printf, filew_prime, $
          strtrim(fit_info_prime.NDOF,2), ' ',$
          strtrim(fit_info_prime.CHISQ,2), ' ',$
          strtrim(fit_info_prime.CHI2NU,2)
  close, filew_prime
  free_lun, filew_prime

; delete all mask files and new start file to save disk space, only keep output file for readout of Chi^2
  IF file_test('null') THEN spawn, 'rm null ';*_with_primary_mask'
  new_mask_names = strsplit(new_masks_all,',',/extract)
  FOR d=0,n_elements(new_mask_names)-1 DO spawn, 'rm '+strtrim(new_mask_names[d],2)
  spawn, 'rm '+new_output_name

; back to the start
  CD, infolder
END
