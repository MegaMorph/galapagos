PRO tiling_helper, cat, targets=targets, radius=radius, grid=grid
; .run tiling_helper
; normal run with simple information
; tiling_helper, '/mnt/users/haeussler/CANDELS/galapagos/mwl_cosmos_30mas_all_detect/table_before_start.sav'

; target list in RA&DEC
; tiling_helper, '/mnt/usersVol1/haeussler/vgala/table_before_start.sav', targets='/mnt/usersVol1/haeussler/vgala/setups/setup_2015_Jun_8/targets.primus_vvds_clusters_neigh', radius=2.5

; if target list has already been correlated by Galapagos
; tiling_helper, '/mnt/usersVol1/haeussler/vgala/table_before_start.sav', targets='/mnt/usersVol1/haeussler/vgala/setups/setup_2015_Jun_8/targets.primus_vvds_clusters_neigh', radius=2.5

  restore, cat
 
  print, 'found a total of '+strtrim(n_elements(sexcat.frame),2)+' objects'
  print, '  '
   
; find UNIQUE file names
  names = sexcat.frame
  names = names[uniq(names,sort(names))]
  
; get ranges
  IF keyword_set(grid) THEN BEGIN  
     cutnames = strarr(n_elements(names))
     xrange = intarr(n_elements(names))
     yrange = intarr(n_elements(names))
     FOR i=0,n_elements(names) -1 DO BEGIN
        cutnames[i] = strmid(names[i],(strpos(names[i],'/',/reverse_search)+1))
        cutnames[i] = strmid(cutnames[i],0,(strpos(cutnames[i],'.fits')))
        yrange[i] = fix(strmid(cutnames[i],(strpos(cutnames[i],'_',/reverse_search))+1))
        cutnames[i] = strmid(cutnames[i],0,(strpos(cutnames[i],'_',/reverse_search)))
        xrange[i] = fix(strmid(cutnames[i],(strpos(cutnames[i],'_',/reverse_search))+1))
     ENDFOR
     cntarr = intarr(max(xrange)-min(xrange)+1,max(yrange)-min(yrange)+1)
     namecut = strarr(n_elements(names))
  ENDIF

  IF keyword_set(targets) AND NOT keyword_set(radius) THEN BEGIN
     print, 'when using a target list, the matching radius has to be set'
     stop
  ENDIF

  add_tag, sexcat, 'do_list', 0, cat2
  sexcat = cat2
  delvarx, cat2

  IF NOT keyword_set(targets) THEN sexcat.do_list = 1 ELSE BEGIN

; only do this when the sav file does not exist or is older than the
; sextractor table!2
     print, 'correlating SExtractor catalogue to source list. Might take some time'
     readcol, targets, do_ra, do_dec, format='F,F', comment='#';, /SILENT   
     
     IF strmid(targets,3,/reverse_offset) EQ 'sav' THEN BEGIN
        restore, targets
     ENDIF ELSE BEGIN
        srccor, sexcat.alpha_j2000/15., sexcat.delta_j2000, do_ra/15., do_dec, $
                radius, tab_i, do_i, OPTION=0, /SPHERICAL , /SILENT
     ENDELSE

     sexcat[tab_i].do_list = 1
     delvarx, tab_i

  ENDELSE

; go through all filenames and find number of objects to be fitted.
  total = 0
  todo = 0
  FOR i=0,n_elements(names)-1 DO BEGIN
     hlptodo = where(sexcat.frame EQ names[i] AND sexcat.do_list EQ 1,cntdo)
     det = where(sexcat.frame EQ names[i],cntdet)
     print, 'image '+names[i]+' : '+strtrim(cntdo,2)+' of '+strtrim(cntdet,2)+' objects'
     total += cntdet
     todo += cntdo
     IF keyword_set(grid) THEN BEGIN  
        namecut[i] = strmid(names[i],(strpos(names[i],'/',/reverse_search)+1))
        namecut[i] = strmid(namecut[i],0,(strpos(namecut[i],'.fits')))
        yhere = fix(strmid(namecut[i],(strpos(namecut[i],'_',/reverse_search))+1))
        namecut[i] = strmid(namecut[i],0,(strpos(namecut[i],'_',/reverse_search)))
        xhere = fix(strmid(namecut[i],(strpos(namecut[i],'_',/reverse_search))+1)) 
        cntarr[xhere-1,yhere-1] = cntdo
      ENDIF
  ENDFOR

  print, 'found a total of '+strtrim(todo,2)+ ' objects to be done of a total of '+strtrim(total,2)+' objects'
  
   IF keyword_set(grid) THEN BEGIN  
      print, ' '
      print, 'Totals by column (x):'
      FOR c=0,n_elements(cntarr[1,*])-1 DO print, total(cntarr[c,*])      
      print, ' '
      print, 'Totals by row (y):'
      FOR r=0,n_elements(cntarr[*,1])-1 DO print, total(cntarr[*,r])
      print, ' '
      print, cntarr
   ENDIF


; NOT YET CODED!!!
; might be a nice feature to check the batch files already and giving numbers
; tiling_helper, '/mnt/usersVol1/haeussler/vgala/table_before_start.sav', targets='primary_list_primus_vvds_cluster_targets.sav', radius=2.5, tiling=['/mnt/usersVol1/haeussler/VIDEO/gala_setup/batch1.1','/mnt/usersVol1/haeussler/VIDEO/gala_setup/batch1.2',/mnt/usersVol1/haeussler/VIDEO/gala_setup/batch1.3'','/mnt/usersVol1/haeussler/VIDEO/gala_setup/batch1.4']

;   IF keyword_set(tiling) THEN BEGIN
;      
;   ENDIF

END

