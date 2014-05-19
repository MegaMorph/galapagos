PRO create_batches, n_cores, bd_batch_file, galexe_str, outdir, outfile
   ;create_batches, 64, '/home/ppzsb1/bd_batch_file', 'galfitm-0.1.2.1', 'gama/galapagos/galapagos_2.0.3_galfit_0.1.2.1_GAMA_9/batches', 'gala_gama_bd1_'
   READCOL,bd_batch_file,F='A', bd_files
   nfiles = n_elements(bd_files)
   n_per_batch = ceil(nfiles / float(n_cores))
   batch = 0
   FOR i=0l, nfiles-1 DO BEGIN
      IF i MOD n_per_batch EQ 0 THEN BEGIN
         IF batch GT 0 THEN BEGIN
            printf, lun, 'echo "Finished job now"'
            free_lun, lun
         ENDIF
         batch++
         outfilenum = outfile+string(strtrim(batch, 2), format='(I3.3)')
         openw, lun, outdir+'/'+outfilenum, /get_lun

         printf, lun, '#!/bin/bash'
         printf, lun, '# This is a submit script for B/D fitting.'
         printf, lun, '# OPTIONS FOR GRID ENGINE ==================================================='
         printf, lun, '# Job should run for no longer than 4 hours'
         printf, lun, '#$ -l h_rt=4:00:00'
         printf, lun, '# Standard output and standard error will both be saved to'
         printf, lun, '# "'+outfilenum+'.out" in the directory qsub was run from'
         printf, lun, '#$ -cwd'
         printf, lun, '#$ -j y'
         printf, lun, '#$ -o '+outfilenum+'.out'
         printf, lun, '# OPTIONS FOR GRID ENGINE===================================================='
         printf, lun, 'echo "Running on `hostname`"'
         printf, lun, 'cd /work/work1/ppzsb1/megamorph'
         printf, lun, 'GALFIT="./'+galexe_str+'"'
     ENDIF
     
     obj_fitstab_file = strrep(bd_files[i], '_obj_bd', '_gf_bd')+'.fits'
     cmd_str = '[ ! -f '+obj_fitstab_file+' ] && $GALFIT '+bd_files[i]
     printf, lun, cmd_str
   ENDFOR
   printf, lun, 'echo "Finished job now"'

   free_lun, lun
END

; PRO extract_bd_info, data_table_file, band_str, out_fits_table, version_num, label
; ;bands in band_str have to be in the proper order!

;    setup = {version:0}
;    setup.version = version_num

;    nband = n_elements(band_str)

;    data_table = mrdfits(data_table_file, 1)
;    forward_function read_sersic_results

;    FOR i=0l, n_elements(data_table) DO BEGIN
;       obj_file = strtrim(data_table[i].initfile, 2)+'_bd'+label
;       obj_fitstab_file = strrep(obj_file, '_obj_bd', '_gf_bd')+'.fits'
;       IF file_test(obj_fitstab_file) THEN BEGIN
;          bd_table = read_sersic_results(obj_file, nband, /bd)

;          str = strtrim(data_table[i].initfile, 2)
;          sky_file = strmid(str, 0, strpos(str, '_obj')+'_'+['', band_str]

;          update_table, bd_table, i, out_fits_table, obj_file,
;          sky_file, nband, setup, /final, /bd
;       ENDIF
;    ENDFOR

; END
