PRO package_objects, object_list, outfolder, notar=notar
; .run package_objects.pro
; package_objects, ['~/GAMA/galapagos/galapagos_2.0.3_galfit_0.1.2.1_GAMA_9/tile41_26/galfit/t41_26.321_obj','~/GAMA/galapagos/galapagos_2.0.3_galfit_0.1.2.1_GAMA_9/tile41_26/galfit/t41_26.322_obj'], '~/test_package'

; cut trailing / in folder name
  IF strmid(outfolder,strlen(outfolder)-1) EQ '/' THEN outfolder = strmid(outfolder,0,strpos(outfolder,'/',/reverse_search))
; create output folder
  spawn, 'mkdir -p '+outfolder
  psffolder = outfolder+'/PSF/'  
  spawn, 'mkdir -p '+psffolder
  
     print, ' '
  FOR o = 0,n_elements(object_list)-1 DO BEGIN
     statusline, 'packaging object '+strtrim(o+1,2)+' of '+strtrim(n_elements(object_list),2)
     obj_new = strmid(object_list[o],strpos(object_list[o],'/',/reverse_search)+1)
     obj_new = strmid(obj_new,0,strpos(obj_new,'_obj',/reverse_search))
     outfolder_new = outfolder+'/'+obj_new
     IF strpos(strtrim(outfolder_new, 2), 'bd') NE -1 THEN outfolder_new = strmid(outfolder_new, 0, strpos(outfolder_new, 'bd')-1)
     package_single_object, object_list[o], outfolder_new, psffolder, /notar
  ENDFOR
     print, ' '
  IF NOT keyword_set(notar) THEN BEGIN
; now pack that folder into a tar file
     print, 'now packaging into tar file'
     outfolder_base = strmid(outfolder,0,strpos(outfolder,'/',/reverse_search))
     outfolder_new = strmid(outfolder,strpos(outfolder,'/',/reverse_search)+1)
     CD, outfolder_base
     
     spawn, 'tar -cf '+outfolder_new+'.tar '+outfolder_new
     spawn, 'rm -r '+outfolder_new
  ENDIF
  
END

PRO package_objects_by_ra_dec, input_cat, ra_dec_cat, radius, outfolder, notar=notar, bd=bd, all=all
; package_objects_by_ra_dec,'/home/bhaeussl/CANDELS/galapagos/egs_30mas_f160w_detect_lin/CANDELS_EGS_30mas_f160w_detect_lin.fits','/home/bhaeussl/test_package/ra_list',1.0,'/home/bhaeussl/test_package/'

; read in catalogue
  print, 'reading catalogue'
  cat = mrdfits(input_cat, 1,/silent)
; read in list of required objects
  readcol, ra_dec_cat, ra, dec, format='F,F', comment='#', /SILENT
; select objects by RA & DEC
  print, 'correlating sources'
  srccor, cat.alpha_j2000/15., cat.delta_j2000, ra/15., dec, $
          radius, cat_i, ra_i, OPTION=0, /SPHERICAL, /SILENT

  IF cat_i[0] NE -1 THEN BEGIN
  ; select the correct object IDs (single-sersic or B/D)
    targets_ss = cat[cat_i].initfile
    targets = targets_ss
    IF keyword_set(bd) or keyword_set(all) THEN BEGIN
       targets_bd = cat[cat_i].initfile_bd
       targets = targets_bd
    ENDIF
    IF keyword_set(all) THEN targets = [targets_ss, targets_bd]
    package_objects, targets, outfolder, notar=notar
  ENDIF
END


PRO package_single_object, obj, outfolder, psffolder, notar=notar
; .run package_objects.pro
; package_single_object, '~/GAMA/galapagos/galapagos_2.0.3_galfit_0.1.2.1_GAMA_9/tile41_26/galfit/t41_26.321_obj', '~/test_package'
  obj = strtrim(obj,2)

;IF n_elements(obj) GT 1 
;outfolder

; create output folder
  spawn, 'mkdir -p '+outfolder

; copy (and adapt object file)
;      obj_new = 
  openr, filer, obj, /get_lun
  line = ''

; get all files needed from the object file itself
  files_to_copy = [obj]
  obj_new = strmid(obj,strpos(obj,'/',/reverse_search)+1)
  files_to_copy_new = [obj_new]

  WHILE ~ EOF(filer) DO BEGIN
; Read a line of text:
      readf, filer, line
; split up into parts
      content_numbers = ' '
      content_descriptor = ' '
      start = strtrim(strmid(line,0,strpos(line,')')+2),2)
      IF strpos(line,'#') NE -1 THEN BEGIN
         content = strtrim(strmid(line,strpos(line,')')+2, strpos(line,'#')-strpos(line,')')-2),2)
         comment = strtrim(strmid(line,strpos(line,'#')),2)
      ENDIF ELSE BEGIN
; if no comment is there
         content = strtrim(strmid(line,strpos(line,')')+2),2)
         comment = ' '
      ENDELSE

; images
     IF strpos(strtrim(line, 2), 'A) ') EQ 0 THEN BEGIN
          content_elements = strsplit(content,',',/extract)
          content_elements_new = content_elements
          FOR el=0,n_elements(content_elements)-1 DO BEGIN
; cut off initial path
              content_elements_new[el] = strmid(content_elements_new[el],strpos(content_elements_new[el],'/',/reverse_search)+1)
          ENDFOR
          files_to_copy = [files_to_copy,content_elements]
          files_to_copy_new = [files_to_copy_new,content_elements_new]
      ENDIF

; get bandnames
      IF strpos(strtrim(line, 2), 'A1) ') EQ 0 THEN BEGIN
          content_elements = strsplit(content,',',/extract)
          bandnames = content_elements
      ENDIF
      
; output image if there
      IF strpos(strtrim(line, 2), 'B) ') EQ 0 THEN BEGIN
          IF file_test(content) THEN BEGIN
              files_to_copy = [files_to_copy,content]
              content_new = strmid(content,strpos(content,'/',/reverse_search)+1)
              files_to_copy_new = [files_to_copy_new,content_new]
 
; if this file exists, there also has to be a galfit.xx file for
; restart purposes. This file is needed, too!

; isolate path first where that file would exist
              workfolder = strmid(content,0,strpos(content,'/',/REVERSE_SEARCH))

; find all matching galfit.??.band files and select newest
              spawn, 'ls '+workfolder+'/'+strmid(content_new,0,strpos(content_new,'.fits'))+'.galfit.*', list

; throw away all the files ending in 'band' or 'output'
              list = list[where(strmid(list,4,/reverse_offset) NE '.band')]
              list = list[where(strmid(list,6,/reverse_offset) NE '_output')]
  
              list2 = list
; isolate counting number
              FOR i=0,n_elements(list)-1 DO list2[i] = strmid(list,strpos(list,'.',/reverse_search)+1)
              list2 = fix(list2)
; select latest file (file with highest number)
              wh = where(list2 EQ max(list2))
              galfit_restart_file = strtrim(list[wh],2)
              files_to_copy = [files_to_copy,galfit_restart_file]
              galfit_restart_file_new = strmid(galfit_restart_file,strpos(galfit_restart_file,'/',/reverse_search)+1)
              files_to_copy_new = [files_to_copy_new,galfit_restart_file_new]
         ENDIF
      ENDIF

; sigma image
      IF strpos(strtrim(line, 2), 'C) ') EQ 0 THEN BEGIN
          content_elements = strsplit(content,',',/extract)
          content_elements_new = content_elements
          FOR el=0,n_elements(content_elements)-1 DO BEGIN
; cut off initial path
              content_elements_new[el] = strmid(content_elements_new[el],strpos(content_elements_new[el],'/',/reverse_search)+1)
          ENDFOR
          files_to_copy = [files_to_copy,content_elements]
          files_to_copy_new = [files_to_copy_new,content_elements_new]
      ENDIF

; psf images
      IF strpos(strtrim(line, 2), 'D) ') EQ 0 THEN BEGIN
          content_elements = strsplit(content,',',/extract)
; remove 'kernel' at the end
         IF strpos(content_elements[-1],'kernel') NE -1 THEN $
            content_elements[-1] = strmid(content_elements[-1], 0, strpos(content_elements[-1],'kernel'))

          content_elements_new = content_elements
; cut off initial path
          FOR el=0,n_elements(content_elements)-1 DO $
            content_elements_new[el] = strmid(content_elements_new[el],strpos(content_elements_new[el],'/',/reverse_search)+1)
; correct naming for PSFs
          content_elements_new = 'PSF_'+bandnames+'_'+content_elements_new
          
          FOR p=0,n_elements(content_elements)-1 DO BEGIN
             spawn, 'cp '+content_elements[p]+' '+psffolder+'/'+content_elements_new[p]
          ENDFOR
;          files_to_copy = [files_to_copy,content_elements]
;          files_to_copy_new = [files_to_copy_new,content_elements_new]
          
      ENDIF
; mask images
      IF strpos(strtrim(line, 2), 'F) ') EQ 0 THEN BEGIN
          IF content EQ '' THEN content = strtrim(strmid(line,3),2)
          content_elements = strsplit(content,',',/extract)
          content_elements_new = content_elements
          FOR el=0,n_elements(content_elements)-1 DO BEGIN
; cut off initial path
              content_elements_new[el] = strmid(content_elements_new[el],strpos(content_elements_new[el],'/',/reverse_search)+1)
          ENDFOR
          files_to_copy = [files_to_copy,content_elements]
          files_to_copy_new = [files_to_copy_new,content_elements_new]
      ENDIF

; constraint file
      IF strpos(strtrim(line, 2), 'G) ') EQ 0 THEN BEGIN
          IF content EQ '' THEN content = strtrim(strmid(line,3),2)
          content_elements = strsplit(content,',',/extract)
          content_elements_new = content_elements
          FOR el=0,n_elements(content_elements)-1 DO BEGIN
; cut off initial path
              content_elements_new[el] = strmid(content_elements_new[el],strpos(content_elements_new[el],'/',/reverse_search)+1)
          ENDFOR
          files_to_copy = [files_to_copy,content_elements]
          files_to_copy_new = [files_to_copy_new,content_elements_new]
      ENDIF
      
  ENDWHILE

; copy all files into output folder
  FOR i=0,n_elements(files_to_copy)-1 DO BEGIN
      IF files_to_copy[i] EQ 'none' THEN CONTINUE
      spawn, 'cp '+files_to_copy[i]+' '+outfolder+'/'+files_to_copy_new[i]
  ENDFOR

  close, filer
  free_lun, filer
  
; now go and change the paths in the objects file
  change_paths_in_obj_file, obj, outfolder
  change_paths_in_obj_file, galfit_restart_file,outfolder
  
  IF NOT keyword_set(notar) THEN BEGIN
; now pack that folder into a tar file
      outfolder_base = strmid(outfolder,0,strpos(outfolder,'/',/reverse_search))
      outfolder_new = strmid(outfolder,strpos(outfolder,'/',/reverse_search)+1)
      CD, outfolder_base
      
      spawn, 'tar -cf '+outfolder_new+'.tar '+outfolder_new
      spawn, 'rm -r '+outfolder_new
  ENDIF
  
END

PRO change_paths_in_obj_file, obj, outfolder
; .run package_objects.pro
; change_paths_in_obj_file, '~/GAMA/galapagos/galapagos_2.0.3_galfit_0.1.2.1_GAMA_9/tile41_26/galfit/t41_26.321_obj', '~/test_package'

; create output folder
  spawn, 'mkdir -p '+outfolder
  objname = strmid(obj,strpos(obj,'/',/reverse_search)+1)
  newfile = outfolder+'/'+objname+'_adapt'


; go through setup file one line at a time and, where needed, cut down
; to one band value
  openr, filer, obj, /get_lun
  line = ''

  openw, filew, newfile, /get_lun

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
     IF strpos(content,',') EQ -1 AND strtrim(line,2) NE '# INITIAL FITTING PARAMETERS' AND strpos(strtrim(line, 2), 'F) ') NE 0 THEN BEGIN
; find output file name and change it, cut off initial path
         IF content EQ '' THEN BEGIN
             content = strtrim(strmid(line,3),2)
             comment = ' '
         ENDIF
         content = strmid(content,strpos(content,'/',/reverse_search)+1)
         IF strpos(strtrim(line, 2), 'B) ') EQ 0 or strpos(strtrim(line, 2), 'G) ') EQ 0 THEN BEGIN
             printf, filew, start+' '+content+'            '+comment
         ENDIF ELSE printf, filew, line

     ENDIF
; if line is mwl setup line
     IF (strpos(content,',') NE -1 AND strpos(content,'band') EQ -1) OR strpos(strtrim(line, 2), 'F) ') EQ 0 THEN BEGIN
         IF strpos(strtrim(line, 2), 'F) ') EQ 0 THEN BEGIN
             content = strtrim(strmid(line,3),2)
             comment = ' '
         ENDIF
         content_elements = strsplit(content,',',/extract)
         po=''
         FOR el=0,n_elements(content_elements)-1 DO BEGIN
; cut off initial path
             content_elements[el] = strmid(content_elements[el],strpos(content_elements[el],'/',/reverse_search)+1)
         ENDFOR

         IF strpos(strtrim(line, 2), 'D) ') EQ 0 THEN BEGIN 
; remove 'kernel' at the end
             IF strpos(content_elements[-1],'kernel') NE -1 THEN $
               content_elements[-1] = strmid(content_elements[-1], 0, strpos(content_elements[-1],'kernel'))
; correct naming for PSFs
             content_elements = '../PSF/PSF_'+bandnames+'_'+content_elements
         ENDIF
         FOR el=0,n_elements(content_elements)-1 DO BEGIN
             po = po+content_elements[el]
             IF el LT n_elements(content_elements)-1 THEN po = po+','
         ENDFOR
         printf, filew, start+' '+po+'            '+comment
         
; save band names for later use in PSF renaming
         IF strpos(strtrim(line, 2), 'A1) ') EQ 0 THEN BEGIN
             bandnames = content_elements
         ENDIF

     ENDIF
     
; if line is mwl parameter line
     IF strpos(content,',') NE -1 AND strpos(content,'band') NE -1  THEN BEGIN
        printf, filew, line
;        content_numbers = strtrim(strmid(content,0,strpos(content,' ')),2)
;        content_elements = strsplit(content_numbers,',',/extract)
;        content_desc = strtrim(strmid(content,strpos(content,' ')),2)
;        content_desc_fit = strtrim(fix(strmid(content_desc,0, strpos(content_desc,' ')))<1,2)
;        printf, filew, start+' '+content_elements[band_index]+'     '+content_desc_fit+' band      '+comment
;        print, 'parameter line'
     ENDIF

; if line is commented line after initiation block
     IF strtrim(line,2) EQ '# INITIAL FITTING PARAMETERS' THEN BEGIN
        IF keyword_set(np) THEN printf, filew, 'U) 1                                         # Turn on nonparam with default options'
        printf, filew, line
     ENDIF        

;print, ' '
  ENDWHILE
  close, filer
  close, filew
  FREE_LUN, filer
  FREE_LUN, filew
END
