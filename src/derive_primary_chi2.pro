PRO derive_primary_chi2, obj_file, gal_exe
; .run derive_primary_chi2.pro
; derive_primary_chi2, '~/primary_package/t11_4.1793/t11_4.1793_gf.galfit.01_adapt'
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
        content_elements = strsplit(content,',',/extract)
        file_base = strmid(content_elements[0],0,strpos(content_elements[0],'_',strpos(content_elements[0],'_')+1))
        pmask_file = file_base+'_mask_primary.fits'
        pmask = mrdfits(pmask_file,0,/silent)
        new_mask_files = 'primary_'+content_elements
        new_masks_all = ''
        FOR b=0,nband-1 DO BEGIN
           mask = mrdfits(content_elements[b],0,maskhd,/silent)
           new_mask = mask-mask+1
; do NOT mask primary!
           new_mask[where(pmask EQ 1)] = 0
; mask everything that is masked in original mask!
           new_mask[where(mask NE 0)] = 1
           writefits, new_mask_files[b], new_mask, maskhd,/silent           
           new_masks_all = new_masks_all+new_mask_files[b]+','
        ENDFOR
; put new masks in galfit file
; cut trailing ','
        new_masks_all = strmid(new_masks_all,0,strlen(new_masks_all)-1)
        printf, filew, 'F) '+new_masks_all
; change content of ouput file to ALWAYS only contain tables, no images
     ENDIF ELSE IF strpos(strtrim(line, 2), 'W) ') EQ 0 THEN BEGIN
        printf, filew, start+'   '+comment
        
; if any other line, simply print out
     ENDIF ELSE printf, filew, line
  
  
  ENDWHILE
  close, filer
  close, filew
  FREE_LUN, filer
  FREE_LUN, filew

; run fits in option galfit -o3
  spawn, gal_exe+' -o2 '+newfile +' >null'

; delete all mask files and new start file to save disk space, only keep output file for readout of Chi^2
;  spawn, 'rm fit.log null '+newfile
;  FOR b=0,nband-1 DO spawn, 'rm '+new_mask_files[b]
  
  
; back to the start
  CD, infolder
END
