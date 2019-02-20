PRO tiling_helper, cat, targets=targets, radius=radius
; .run tiling_helper
; normal run with simple information
; tiling_helper, '/mnt/users/haeussler/CANDELS/galapagos/mwl_cosmos_30mas_all_detect/table_before_start.sav'

; target list in RA&DEC
; tiling_helper, '/mnt/usersVol1/haeussler/vgala/table_before_start.sav', targets='/mnt/usersVol1/haeussler/vgala/setups/setup_2015_Jun_8/targets.primus_vvds_clusters_neigh', radius=2.5

; SAV VERSION DOES NOT WORK. INDEX IS FOR WRONG TABLE (table, not sexcat)
; if target list has already been correlated by Galapagos
; tiling_helper, '/mnt/usersVol1/haeussler/vgala/table_before_start.sav', targets='/mnt/usersVol1/haeussler/vgala/primary_list_targets.primus_vvds_clusters_neigh.sav', radius=2.5

  IF keyword_set(targets) AND NOT keyword_set(radius) THEN BEGIN
     print, 'when using a target list, the matching radius has to be set'
     stop
  ENDIF

  restore, cat
 
  print, 'found a total of '+strtrim(n_elements(sexcat.frame),2)+' objects'
  print, '  '
   
; find UNIQUE file names
  names = sexcat.frame
  names = names[uniq(names,sort(names))]
  
  add_tag, sexcat, 'do_list', 0, cat2
  sexcat = cat2
  delvarx, cat2

  IF NOT keyword_set(targets) THEN sexcat.do_list = 1 ELSE BEGIN

; only do this when the sav file does not exist or is older than the
; sextractor table!
     print, 'correlating SExtractor catalogue to source list. Might take some time'
     
     IF strmid(targets,3,/reverse_offset) EQ '.sav' THEN BEGIN
        restore, targets
        allframes = table.frame[0]
        do_list=intarr(n_elements(allframes))*0
        do_list[tab_i]=1
     ENDIF ELSE BEGIN
        readcol, targets, do_ra, do_dec, format='F,F', comment='#' ;, /SILENT   
        srccor, sexcat.alpha_j2000/15., sexcat.delta_j2000, do_ra/15., do_dec, $
                radius, tab_i, do_i, OPTION=0, /SPHERICAL , /SILENT
        allframes = sexcat.frame
        do_list=intarr(n_elements(allframes))*0
        do_list[tab_i]=1
     ENDELSE

;     sexcat[tab_i].do_list = 1
     delvarx, tab_i

  ENDELSE

; go through all filenames and find number of objects to be fitted.
  total = 0
  todo = 0
  FOR i=0,n_elements(names)-1 DO BEGIN
     hlptodo = where(allframes EQ names[i] AND do_list EQ 1,cntdo)
     det = where(allframes EQ names[i],cntdet)
     total += cntdet
     todo += cntdo
     print, 'image '+names[i]+' : '+strtrim(cntdo,2)+' of '+strtrim(cntdet,2)+' objects (total so far: '+strtrim(todo,2)+')'
  ENDFOR

  print, 'found a total of '+strtrim(todo,2)+ ' objects to be done of a total of '+strtrim(total,2)+' objects'
  
; NOT YET CODED!!!
; might be a nice feature to check the batch files already and giving numbers
; tiling_helper, '/mnt/usersVol1/haeussler/vgala/table_before_start.sav', targets='primary_list_primus_vvds_cluster_targets.sav', radius=2.5, tiling=['/mnt/usersVol1/haeussler/VIDEO/gala_setup/batch1.1','/mnt/usersVol1/haeussler/VIDEO/gala_setup/batch1.2',/mnt/usersVol1/haeussler/VIDEO/gala_setup/batch1.3'','/mnt/usersVol1/haeussler/VIDEO/gala_setup/batch1.4']

;   IF keyword_set(tiling) THEN BEGIN
;      
;   ENDIF

END
