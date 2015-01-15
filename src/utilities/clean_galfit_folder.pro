pro clean_galfit_folder, path, max_file_age=max_file_age, bd=bd
; timestamp: time in hours backwards from now
; .run clean_galfit_folder
; clean_galfit_folder, '/usersVol1/haeussler/vgala'
; clean_galfit_folder,'/mnt/users/haeussler/CANDELS/galapagos/mwl_cosmos_30mas_all_detect',timestamp=[10,4,1582]
CD, path

; remove all files of objects that were active when skipped
spawn, 'ls */galfit/*sav', list
FOR n=0,n_elements(list)-1 DO BEGIN
    pre = ''
    pre = strmid(list[n],0,strpos(list[n],'_gf.sav'))
    print, 'deleting files '+pre+'_* '
    spawn, 'rm '+pre+'_*'
    spawn, 'rm '+list[n]
ENDFOR

; same for bd folder!
IF keyword_set(bd) THEN BEGIN
   spawn, 'ls */galfit_bd*/*sav', listbd
   FOR n=0,n_elements(listbd)-1 DO BEGIN
      pre = ''
      pre = strmid(listbd[n],0,strpos(listbd[n],'_gf.sav'))
      print, 'deleting files '+pre+'_* '
      spawn, 'rm '+pre+'_*'
      spawn, 'rm '+list[n]
   ENDFOR
ENDIF

; remove all files of objects that have obj* files, but no *gf.fits
; files after a certain timestamp

;has to be done folder by folder because of number of files
IF keyword_set(max_file_age) THEN BEGIN

   spawn, 'ls -d */galfit', folderlist
   FOR f=0,n_elements(folderlist)-1 DO BEGIN
      list2=0
      spawn, 'ls '+folderlist[f]+'/*obj', list2
      FOR n=0,n_elements(list2)-1 DO BEGIN
         
         pre = ''
         pre = strmid(list2[n],0,strpos(list2[n],'_obj'))
         obj_file_test = file_info(list2[n])
         gf_file_test = file_info(pre+'_gf.fits')
         start_file_test = file_info(list2[n]+'_not_started')
         file_age = (systime(1)-obj_file_test.mtime)/3600.

; object file exists exist
         IF obj_file_test.exists EQ 1 THEN BEGIN
; object has NOT not been started on purpose (*not_started file does NOT exist)
            IF start_file_test.exists EQ 0 THEN BEGIN
; output file does not exist, so fit never finished. 
; either crashed or was killed
               IF gf_file_test.exists EQ 0 THEN BEGIN
; check whether file is old enough (last stable point)
                  IF file_age LT float(max_file_age) THEN BEGIN
                     print, 'deleting files '+pre+'_* '
                     spawn, 'rm '+pre+'_* '
                  ENDIF
               ENDIF
            ENDIF
         ENDIF

      ENDFOR
   ENDFOR
ENDIF
END

