PRO gala_bd_bridge_hpc, data_table, setup
   ;run_bd_fit, 'gama/galapagos/galapagos_2.0.3_galfit_0.1.2.1_GAMA_9/GAMA_9_ffvqqff_gama_only.fits', '1'
   ;this must be run from the root of the gama/galapagos/... tree
   ;in order to get the paths right
   ;data_table = 'relative/path/to/GAMA_9_ffvqqff_gama_only.fits
   ;data_table = mrdfits(data_table_file, 1)

   openw, batch_file, setup.outdir+setup.bd_label+'_batch_file', /get_lun
   rsync_filename = setup.outdir+setup.bd_label+'_rsync_includes'
   openw, rsync_file, rsync_filename, /get_lun

   printf, rsync_file, '+ **/'
   FOR i=0l, n_elements(data_table)-1 DO BEGIN
;only do B/D for bright objects
       IF (data_table[i].do_list_bd EQ 1 and data_table[i].mag_galfit_band[0] le setup.bd_maglim) THEN BEGIN
           obj_fitstab_file = strtrim(data_table[i].file_galfit, 2)
           check_obj_file = obj_fitstab_file
;change path from boris on dator to general location
           obj_fitstab_file = strrep(obj_fitstab_file, setup.outdir, setup.bd_hpc_path)
           obj_fitstab_file = strrep(obj_fitstab_file, setup.galfit_out_path, $
                                     strmid(setup.galfit_out_path, 0, strpos(setup.galfit_out_path, '/'))+'_'+setup.bd_label+'/')
           obj_fitstab_file = strrep(obj_fitstab_file, setup.galfit_out, setup.bd_label+'_'+setup.galfit_out)
           check_obj_file = strrep(check_obj_file, setup.galfit_out_path, $
                                     strmid(setup.galfit_out_path, 0, strpos(setup.galfit_out_path, '/'))+'_'+setup.bd_label+'/')
           check_obj_file = strrep(check_obj_file, setup.galfit_out, setup.bd_label+'_'+setup.galfit_out)
           check_obj_file = strrep(check_obj_file, setup.galfit_out+'.fits', 'obj')
            if setup.bd_hpc_path ne '' then obj_fitstab_file = strrep(obj_fitstab_file, setup.outdir, setup.bd_hpc_path)
           IF file_test(check_obj_file) THEN BEGIN
;              bd_fit, obj_fitstab_file, label, /no_fit
               printf, batch_file, strrep(obj_fitstab_file, setup.galfit_out+'.fits', 'obj')
               obj_id = STRSPLIT(obj_fitstab_file, '/', /EXTRACT)
               obj_id = strrep(obj_id(N_ELEMENTS(obj_id)-1), '_'+setup.bd_label+'_'+setup.galfit_out+'.fits', '')
               printf, rsync_file, '+ *'+obj_id+'*'
           ENDIF
       ENDIF
  ENDFOR
  printf, rsync_file, '- *' 
  print, "Necessary files can now be transferred using the command:"
  print, "rsync -av --include-from="+rsync_filename+" ./ jupiter:/path/to/gama/galapagos/galapagos_run/"
;  print, "rsync -av --prune-empty-dirs --include-from="+rsync_filename+" ./ jupiter:/path/to/gama/galapagos/galapagos_run/"
  free_lun, batch_file
  free_lun, rsync_file
END

