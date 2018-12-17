@galapagos.pro
pro clean_galfit_folder, setup_file, max_file_age=max_file_age, bd=bd, clean_all_crashed=clean_all_crashed
; timestamp: time in hours backwards from now
; max_file_age: file age given in HOURS
; clean_crashed: If set, additionally, all files for crashed fits are deleted, so they can be retried
; .run clean_galfit_folder
; clean_galfit_folder,'/home/bhaeussl/megamorph/scripts_boris/gala_setups/G12/GAMA12_galapagos_setup_readout',max_age_file=4,/clean_all_crashed

; get path from setup file, including /bd folder
; read setup file first
  read_setup, setup_file, setup
  
  out_path = setup.outdir
  IF keyword_set(bd) THEN BEGIN
     folder_path = strtrim(strmid(setup.galfit_out_path,0,strlen(setup.galfit_out_path)-1)+'_'+setup.bd_label,2)
  ENDIF ELSE BEGIN
     folder_path = strtrim(strmid(setup.galfit_out_path,0,strlen(setup.galfit_out_path)-1),2)
  ENDELSE
  
  CD, out_path
; remove all files of objects that were active when skipped
  spawn, 'ls */'+folder_path+'/*sav', list
  FOR n=0,n_elements(list)-1 DO BEGIN
     pre = ''
     pre = strmid(list[n],0,strpos(list[n],'_gf.sav'))
     print, 'deleting files '+pre+'_* '
     spawn, 'rm '+pre+'_*'
     IF file_test(list[n]) THEN spawn, 'rm '+list[n]
  ENDFOR

; FILE AGE!
; remove all files of objects that have obj* files, but no *gf.fits
; files after a certain timestamp
  print, '  '
  print, '  '
;has to be done folder by folder because of number of files
  IF keyword_set(max_file_age) THEN BEGIN
     spawn, 'ls -d */'+folder_path, folderlist
     FOR f=0,n_elements(folderlist)-1 DO BEGIN
        list2=0
        spawn, 'ls '+folderlist[f]+'/*obj', list2
        FOR n=0,n_elements(list2)-1 DO BEGIN
           
           pre = ''
           pre = strmid(list2[n],0,strpos(list2[n],'_obj'))
           obj_file_test = file_info(list2[n]) 
           gf_file_test = file_info(pre+'_gf.fits')
           start_file_test = file_info(list2[n]+'_not_started')
           file_age = (systime(1)-obj_file_test.ctime)/3600.
; object file exists exist
           IF obj_file_test.exists EQ 1 THEN BEGIN
; object has NOT not been started on purpose (*not_started file does NOT exist)
              IF start_file_test.exists EQ 0 THEN BEGIN
; output file does not exist, so fit never finished. 
; either crashed or was killed
                 IF gf_file_test.exists EQ 0 THEN BEGIN
; check whether file is old enough (last stable point)
                    IF file_age LT float(max_file_age) THEN BEGIN
                       print, 'deleting files '+pre+'_* , '+strtrim(file_age,2)+' hours old'
                       spawn, 'rm '+pre+'_* '
                    ENDIF ; file older than maximum allowed age
                 ENDIF ; galfit outfile does not exist
              ENDIF ; *not_started file does not exist
           ENDIF ; obj file does exist (by definition of loop should always exist?)
        ENDFOR ; loop over all *obj files in a folder
     ENDFOR ; loop over all folders
  ENDIF ; file_age keyword set

; DELETE CRASHED FITS (same as max_file_age, but without file age limit)
  print, '  '
  print, '  '
;has to be done folder by folder because of number of files
  IF keyword_set(clean_all_crashed) THEN BEGIN
     del=0l
     spawn, 'ls -d */'+folder_path, folderlist
     FOR f=0,n_elements(folderlist)-1 DO BEGIN
        list2=0
        spawn, 'ls '+folderlist[f]+'/*obj', list2
        FOR n=0,n_elements(list2)-1 DO BEGIN
           pre = ''
           pre = strmid(list2[n],0,strpos(list2[n],'_obj'))
           obj_file_test = file_info(list2[n]) 
           gf_file_test = file_info(pre+'_gf.fits')
           start_file_test = file_info(list2[n]+'_not_started')
; object file exists exist
           IF obj_file_test.exists EQ 1 THEN BEGIN
; object has NOT not been started on purpose (*not_started file does NOT exist)
              IF start_file_test.exists EQ 0 THEN BEGIN
; output file does not exist, so fit never finished. 
; either crashed or was killed
                 IF gf_file_test.exists EQ 0 THEN BEGIN
; check whether file is old enough (last stable point)
                    print, 'deleting files '+pre+'_*'
                    del +=1
                    spawn, 'rm '+pre+'_* '
                 ENDIF ; galfit outfile does not exist
              ENDIF ; *not_started file does not exist
           ENDIF ; obj file does exist (by definition of loop should always exist?)
        ENDFOR ; loop over all *obj files in a folder
     ENDFOR ; loop over all folders
     print, 'deleted '+strtrim(del,2)+' crashed objects, ready for restart'
  ENDIF ; clean_crashed keyword 


END
