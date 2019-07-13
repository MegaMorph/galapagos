@galapagos.pro
PRO clean_galfit_restart_files, setup_file
; .run clean_galfit_restart_files
; clean_galfit_restart_files, '/lustre/home/bhaeussl/megamorph/scripts_boris/gala_setups/G15/GAMA15_galapagos_setup_sextractor'
; read setup file first
  starttime=systime(0)
  read_setup, setup_file, setup

  out_path = setup.outdir
  folder_path_all = [strtrim(strmid(setup.galfit_out_path,0,strlen(setup.galfit_out_path)-1),2),strtrim(strmid(setup.galfit_out_path,0,strlen(setup.galfit_out_path)-1)+'_'+setup.bd_label,2)]

  missing_files = ' '
  FOR fp=0,1 DO BEGIN
     folder_path = folder_path_all[fp]
     
     spawn, 'pwd', start_folder

     CD, strtrim(out_path,2)
     
; get list of all folders
     spawn, 'ls -d */'+folder_path, folder_list
     
;folder by folder
     FOR f=0, n_elements(folder_list)-1 DO BEGIN
        CD, out_path+'/'+folder_list[f]
        print, ''
        print, 'checking folder '+strtrim(folder_list[f])
        delvarx, gf_outfile_list
;get list of gf.fits files
        spawn, 'ls *'+setup.galfit_out+'.fits', gf_outfile_list, count=gf_count

        IF gf_count GT 0 THEN BEGIN
; file by file, read out name of restart_file
           FOR gf=0,n_elements(gf_outfile_list)-1 DO BEGIN
              delvarx, gf_restart_file
              fit_info = mrdfits(gf_outfile_list[gf], 'FIT_INFO',/silent)
              gf_restart_file = strtrim(fit_info.logfile,2)
              
; check whether restart file exists
              gf_restart_exist = file_test(strtrim(gf_restart_file,2))
              
; if exists, do nothing
              IF gf_restart_exist EQ 1 THEN BEGIN
                 statusline, gf_restart_file+' exists                     '
;;; if does not exist, delete all files for that object, so it can be restarted
; if does not esist, print out object file, so a restart of the fit can be attempted
              ENDIF ELSE BEGIN
                 print, gf_restart_file+' does not exist                  '
                 missing_files = [missing_files,gf_restart_file]
              ENDELSE 
           ENDFOR               ; end of loop of galfit output files
        ENDIF                   ; if gf_outfile_list is empty
     ENDFOR                     ; end of loop over folders
  ENDFOR                        ; ond of loop for galfit & galfit_bd folders
  
  print, ' '
  print, ' '
  IF n_elements(missing_files) EQ 1 THEN BEGIN
     print, 'congratulations, no missing files found'
  ENDIF ELSE BEGIN
     print, 'missing files (THAT AND MATCHING FILES NEED TO BE DELETED BY HAND!): '
     print, missing_files
  ENDELSE

  CD, start_folder
  print, 'started at: '+starttime  
  print, 'finished at: '+systime(0)
END

PRO clean_galfit_obj_files, setup_file
; .run clean_galfit_restart_files
; clean_galfit_obj_files, '/lustre/home/bhaeussl/megamorph/scripts_boris/gala_setups/G12/GAMA12_galapagos_setup_sextractor'
; read setup file first
  starttime=systime(0)
  read_setup, setup_file, setup

  out_path = setup.outdir
  folder_path_all = [strtrim(strmid(setup.galfit_out_path,0,strlen(setup.galfit_out_path)-1),2),strtrim(strmid(setup.galfit_out_path,0,strlen(setup.galfit_out_path)-1)+'_'+setup.bd_label,2)]

  missing_files = ' '
  FOR fp=0,1 DO BEGIN
     folder_path = folder_path_all[fp]
     
     spawn, 'pwd', start_folder

     CD, strtrim(out_path,2)
     
; get list of all folders
     spawn, 'ls -d */'+folder_path, folder_list
     
;folder by folder
     FOR f=0, n_elements(folder_list)-1 DO BEGIN
        CD, out_path+'/'+folder_list[f]
        print, ''
        print, 'checking folder '+strtrim(folder_list[f])
        delvarx, gf_outfile_list
;get list of gf.fits files
        spawn, 'ls *'+setup.galfit_out+'.fits', gf_outfile_list, count=gf_count

        IF gf_count GT 0 THEN BEGIN
; file by file, read out name of restart_file
           FOR gf=0,n_elements(gf_outfile_list)-1 DO BEGIN
              delvarx, gf_restart_file
              fit_info = mrdfits(gf_outfile_list[gf], 'FIT_INFO',/silent)
              gf_init_file = strtrim(fit_info.initfile,2)
              
; check whether restart file exists
              gf_init_exist = file_test(strtrim(gf_init_file,2))
              
; if exists, do nothing
              IF gf_init_exist EQ 1 THEN BEGIN
                 statusline, gf_init_file+' exists                     '
;;; if does not exist, delete all files for that object, so it can be restarted
; if does not esist, print out object file, so a restart of the fit can be attempted
              ENDIF ELSE BEGIN
                 print, gf_init_file+' does not exist                  '
                 missing_files = [missing_files,gf_init_file]
              ENDELSE 
           ENDFOR               ; end of loop of galfit output files
        ENDIF                   ; if gf_outfile_list is empty
     ENDFOR                     ; end of loop over folders
  ENDFOR                        ; ond of loop for galfit & galfit_bd folders
  
  print, ' '
  print, ' '
  IF n_elements(missing_files) EQ 1 THEN BEGIN
     print, 'congratulations, no missing files found'
  ENDIF ELSE BEGIN
     print, 'missing files (THAT AND MATCHING FILES NEED TO BE DELETED BY HAND!): '
     print, missing_files
  ENDELSE

  CD, start_folder
  print, 'started at: '+starttime  
  print, 'finished at: '+systime(0)
END
