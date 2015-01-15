pro clean_galfit_folder, path, timestamp=timestamp, bd=bd
; .run clean_galfit_folder
; clean_galfit_folder, '/usersVol1/haeussler/vgala'
CD, path

; remove all files of objects that were active when skipped
spawn, 'ls */galfit/*sav', list
FOR n=0,n_elements(list)-1 DO BEGIN
    pre = ''
    pre = strmid(list[n],0,strpos(list[n],'_gf.sav'))
    spawn, 'rm '+pre+'_obj'
    spawn, 'rm '+pre+'_gf.fits'
    spawn, 'rm '+list[n]
ENDFOR

; same for bd folder!
IF keyword_set(bd) THEN BEGIN
   spawn, 'ls */galfit_bd*/*sav', listbd
   FOR n=0,n_elements(listbd)-1 DO BEGIN
      pre = ''
      pre = strmid(listbd[n],0,strpos(listbd[n],'_gf.sav'))
      spawn, 'rm '+pre+'_obj'
      spawn, 'rm '+pre+'_gf.fits'
      spawn, 'rm '+list[n]
   ENDFOR
ENDIF

; remove all files of objects that have obj* files, but no *gf.fits
; files after a certain timestamp

;has to be done folder by folder
IF keyword_set(timestamp) THEN BEGIN
   spawn, 'ls */galfit', folderlist
   
   FOR n=0,n_elements(folderlist)-1 DO BEGIN
      list2=0
      spawn, 'ls */galfit/*obj', list2
      
      FOR n=0,n_elements(list2)-1 DO BEGIN
         pre = ''
         pre = strmid(list2[n],0,strpos(list2[n],'obj'))
         stop
         spawn, 'rm '+pre+'_obj'
         spawn, 'rm '+list[n]
      ENDFOR
   ENDFOR
ENDIF

END

