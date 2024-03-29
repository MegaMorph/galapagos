; GALAPAGOS: 
;     Galaxy Analysis over Large Areas: Parameter Assessment by GALFITting Objects from SExtractor
; Multi-Wavelength Version, requires Galfit4 for multi-band fitting.
; Backwards compatible to work with Galfit3 on 1-band data
;==============================================================================
FUNCTION required_entries
  return, ['number', 'x_image', 'y_image', 'cxx_image', 'cyy_image', $
           'cxy_image', 'theta_image', 'theta_world', 'ellipticity', $
           'kron_radius', 'a_image', 'b_image', 'alpha_j2000', $
           'delta_j2000', 'background']
END

FUNCTION one_element_fltarr, var
  IF n_elements(var) EQ 1 THEN BEGIN
     out = fltarr(1)
     out[0] = var
  ENDIF ELSE out = var
  return, out
END 

FUNCTION tmp_file, path
;create a temporary filename with PATH prepended
  file = path+'tmp.'+strcompress(systime(), /remove_all)
  WHILE file_test(strtrim(file,2)) DO file += strtrim(round(randomu(systime(1))*10), 2)
  return, file
END

FUNCTION set_trailing_slash, str
;append a '/' at the end of str in case there is none on input
  out = str
  FOR i=0ul, n_elements(str)-1 DO $
     IF strmid(str[i], strlen(str[i])-1, 1) NE '/' THEN out[i] = out[i]+'/'
  return, out
END

FUNCTION n_lines, file
;identify number of lines in FILE
;lines beginning with '#' are treated as comments and not counted
;empty lines are not counted
  nrow = 0ul
  line = ''
  openr, 1, file
  WHILE NOT eof(1) DO BEGIN
     readf, 1, line
     IF strmid(strtrim(line, 2), 0, 1) NE '#' AND $
        strlen(line) NE 0 THEN nrow++
  ENDWHILE
  close, 1
  
  return, nrow
END

PRO fill_struct, table, file
;fill structure TABLE with values from FILE
;'#' in file is treated as comment
;' ' is used as column separator
  row = 0ul
  line = ''
  openr, 1, file
  WHILE NOT eof(1) DO BEGIN
     readf, 1, line
     IF strmid(line, 0, 1) EQ '#' THEN CONTINUE
     FOR i=0ul, n_tags(table)-1 DO BEGIN
        line = strtrim(line, 2)
        pos = strpos(line, ' ')
        IF pos EQ -1 THEN pos = strlen(line)
        table[row].(i) = strmid(line, 0, pos)
        line = strmid(line, pos, strlen(line))
     ENDFOR
     row++
  ENDWHILE
  close, 1
END

FUNCTION integer2string, num, digits0, array = array
;function to create a string from an integer. Leading postions are
;filled with 0: e.g. 001, 002, 003, ..., 009, 010, 011, ..., 099, 100,
;101, ..., 999 for digits=3
;if array is set digits is the array that contains num. It is used to
;calculate the actual digits. E.g. max(digits)=976 --> digits=3
  IF keyword_set(array) THEN BEGIN
     digits = max(digits0)*10ul
     ct = 0ul
     WHILE (digits /= 10.) GE 1 DO ct++
     digits = ct
  ENDIF ELSE digits = digits0
  
  max = 10^(digits-1)
  str = strtrim(num, 2)
  WHILE num LT max DO BEGIN
     max = max/10
     str = '0'+str
  ENDWHILE
  return, str
END

FUNCTION read_sex_param, file, nrow, no_add = no_add, add_column = add_column
;create a structure with tags based on a SExtractor parameter FILE
;(including path). The length of the structure array is determined by
;NROW. To calculate a set of SExtractor ellipses certain parameter
;entries are required. Remove them with NO_ADD.
;add_column: 2d string array: [[label0, value0],[label1, value1],...]
  
  line = ''
  IF keyword_set(no_add) THEN label = '' $
  ELSE label = required_entries()
  
  openr, 1, file
  WHILE NOT eof(1) DO BEGIN
     readf, 1, line
     IF strlen(line) EQ 0 OR $
        strmid(strtrim(line, 2), 0, 1) EQ '#' THEN CONTINUE
     line = strlowcase(line)
     idx = where(label EQ line, ct)
     IF ct EQ 0 THEN label = [label, line]
  ENDWHILE
  close, 1
  IF label[0] EQ '' THEN label = label[1:*]
  
  value = label
  FOR i=0ul, n_elements(label)-1 DO BEGIN
     CASE label[i] OF
        'number':        value[i] = '0L'
        'flags':         value[i] = '0'
        'imaflags_iso':  value[i] = '0'
        'nimaflags_iso': value[i] = '0'
        ELSE: value[i] = '0.d0'
     ENDCASE
  ENDFOR
  
  IF keyword_set(add_column) THEN BEGIN
     FOR i=0ul, n_elements(add_column[0, *])-1 DO BEGIN
        label = [label, add_column[0, i]]
        value = [value, add_column[1, i]]
     ENDFOR
  ENDIF
  
  return, mrd_struct(label, value, nrow, /no_execute)
END

FUNCTION read_sex_table, file, param, add_column = add_column
;for a given SExtractor table (FILE) use a SExtractor parameter file
;(PARAM) to create a structure array.
;add_column: 2d string array: [[label0, value0],[label1, value1],...]
;tab = read_sex_table(setup.outdir+'outcat', $
;outpath_pre[0]+setup.outparam, add_col = ['FILE', '" "'])
;<-- additional colum of type string with label 'FILE'
  
;identify number of lines in sextractor table
;lines beginning with '#' are treated as comments
  nrow = n_lines(file)
  
;setup structure
  table = read_sex_param(param, nrow, add_column = add_column)
  
;fill structure with values
  fill_struct, table, file
  
  return, table
END

PRO write_sex_table, table, outcat, add_column = add_column
;table: data structure with SExtractor variables
;outcat: filename of the output catalogue
;add_column: string array with the same number of elements as table,
;containing additional columns (OPTIONAL)
  
  label = strlowcase(tag_names(table))
  fmt = strarr(n_tags(table))
  FOR i=0ul, n_elements(label)-1 DO BEGIN
     CASE label[i] OF
        'number':        fmt[i] = 'I'
        'flags':         fmt[i] = 'I'
        'imaflags_iso':  fmt[i] = 'I'
        'nimaflags_iso': fmt[i] = 'I'
        'alpha_sky':     fmt[i] = 'D'
        'delta_sky':     fmt[i] = 'D'
        'alpha_j2000':   fmt[i] = 'D'
        'delta_j2000':   fmt[i] = 'D'
        'alpha_b1950':   fmt[i] = 'D'
        'delta_b1950':   fmt[i] = 'D'
        ELSE: fmt[i] = 'G'
     ENDCASE
  ENDFOR
  
  n = n_elements(table)
  openw, 1, outcat
  FOR i=0ul, n-1 DO BEGIN
     fmt_line = (line = '')
     FOR j=0ul, n_tags(table)-1 DO BEGIN
        str = string(table[i].(j), format = '('+fmt[j]+')')
        IF strpos(str, '.') GE 0 THEN BEGIN
           split = strsplit(str, 'E', /extract)
           WHILE strmid(split[0], strlen(split[0])-2, 2) EQ '00' DO $
              split[0] = strmid(split[0], 0, strlen(split[0])-1)
           IF n_elements(split) GT 1 THEN str = split[0]+'E'+split[1] $
           ELSE str = split[0]
        ENDIF
        line += strtrim(str, 2)+' '
     ENDFOR
     line = strtrim(line, 2)
     IF keyword_set(add_column) THEN line = line+' '+add_column[i]
     printf, 1, line
  ENDFOR
  close, 1
END

PRO run_sextractor, setup, images, weights, outpath_file, tile, exclude
;run SExtractor (SEXEXE path to the executable) using a given
;parameter file (SEXPARAM), a HOT setup file, a COLD setup file and a
;ZEROPOINT on an IMAGE and accompanying WEIGHT. On output one gets a
;hot/cold catalogue (HOTCAT/COLDCAT), segmentation map (COLDSEG/HOTSEG)
;and a combined catalogue and segmentation map (OUTCAT/OUTSEG). A
;checkimage can be created (filename CHECK) of type CHECKTYPE. If
;needed, objects at a set of x,y positions may be removed from the
;catalogues and segmentation maps (filename EXCLUDE). The matching
;radius for this exclusion is given by RAD.
;OUTONLY: if set remove hot/cold cat and seg after combining.
  image = images[tile,0]
  weight = weights[tile,0]
  cold = setup.cold
  coldcat = outpath_file[tile,0]+setup.coldcat
  coldseg = outpath_file[tile,0]+setup.coldseg
  hot = setup.hot
  hotcat = outpath_file[tile,0]+setup.hotcat
  hotseg = outpath_file[tile,0]+setup.hotseg
  outcat = outpath_file[tile,0]+setup.outcat
  outseg = outpath_file[tile,0]+setup.outseg
  outparam = outpath_file[tile,0]+setup.outparam
  check = outpath_file[tile,0]+setup.check
  
;create a temporary parameter file with the minimum set of sextractor
;parameters
  IF file_test(strtrim(hot,2)) THEN multi = 1 ELSE multi = 0
  IF file_test(strtrim(cold,2)) THEN multi += 2
  IF multi EQ 0 THEN $
     message, 'Neither cold nor hot SExtractor setup-file found'
  IF multi EQ 1 THEN BEGIN
     print, 'WARNING!!!!!!!!!! No cold sex setup file found. Using only hot setup (as cold)'
     cold = hot
     coldcat = hotcat
     coldseg = hotseg
     multi = 2
  ENDIF
  IF setup.sex_rms EQ 1 THEN print, 'RMS map used for SExtractor step'
  
  path = strmid(outcat, 0, strpos(outcat, '/', /reverse_search)+1)
  label = required_entries()
  openw, 1, outparam
  FOR i=0ul, n_elements(label)-1 DO printf, 1, strupcase(label[i])
  openr, 2, setup.sexout
  line = ''
  WHILE NOT eof(2) DO BEGIN
     readf, 2, line
     line = strlowcase(line)
     idx = where(label EQ line, ct)
     IF ct EQ 0 THEN printf, 1, strupcase(line)
  ENDWHILE
  close, 1
  close, 2
  
;read image header and calculate sextractor magnitude zeropoint
  header = headfits(image)
;exptime = double(sxpar(header, 'EXPTIME'))
  exptime = setup.expt[0]
  zp_eff = strtrim(setup.zp[0]+2.5*alog10(exptime), 2)
  
  print, '---using exptime: '+strtrim(exptime[0], 2)+'s (zp='+zp_eff[0]+') for: '+ $
         image
  
  IF setup.sex_rms EQ 0 THEN weight_type = 'MAP_WEIGHT'
  IF setup.sex_rms EQ 1 THEN weight_type = 'MAP_RMS'
  
;this will produce a checkimage with all the ellipses
  IF setup.chktype NE 'none' THEN BEGIN
     
;from Arjen, dual-image mode removes a bug from single image
;mode. Better detection & deblending!
;    spawn, sexexe+' '+detimage+','+image+' -c '+cold+ $
;      ' -CATALOG_NAME '+coldcat+' -CATALOG_TYPE ASCII' + $
;      ' -PARAMETERS_NAME '+outparam+ $
;      ' -WEIGHT_IMAGE '+detweight+','+weight+ $
;      ' -WEIGHT_TYPE MAP_RMS,MAP_RMS -MAG_ZEROPOINT '+zp_eff+ $
     
     print, 'starting cold sex check image on image '+image+' '
     print, '   using weight image'+weight+' '
     sexcommand_cc = setup.sexexe+' '+image+','+image+' -c '+cold+ $
                     ' -CATALOG_NAME '+coldcat+' -CATALOG_TYPE ASCII' + $
                     ' -PARAMETERS_NAME '+outparam+ $
                     ' -WEIGHT_IMAGE '+weight+','+weight+ $
                     ' -WEIGHT_TYPE '+weight_type+','+weight_type+' -MAG_ZEROPOINT '+zp_eff[0]+ $
                     ' -CHECKIMAGE_TYPE '+setup.chktype+' -CHECKIMAGE_NAME '+ $
                     file_dirname(check)+'/'+file_basename(check, '.fits')+'.cold.fits'
;     print, sexcommand_cc
     spawn, sexcommand_cc
     IF multi EQ 3 THEN BEGIN
        print, 'starting hot sex check image'
        sexcommand_hc = setup.sexexe+' '+image+','+image+' -c '+hot+ $
                        ' -CATALOG_NAME '+hotcat+' -CATALOG_TYPE ASCII' + $
                        ' -PARAMETERS_NAME '+outparam+ $
                        ' -WEIGHT_IMAGE '+weight+','+weight+ $
                        ' -WEIGHT_TYPE '+weight_type+','+weight_type+' -MAG_ZEROPOINT '+zp_eff[0]+ $
                        ' -CHECKIMAGE_TYPE '+setup.chktype+' -CHECKIMAGE_NAME '+ $
                        file_dirname(check)+'/'+file_basename(check, '.fits')+ $
                        '.hot.fits'
;        print, sexcommand_hc
        spawn, sexcommand_hc
     ENDIF
  ENDIF
  
;now start sextractor to create hotcat and coldcat
  print, 'starting cold sex'
  sexcommand_cs = setup.sexexe+' '+image+','+image+' -c '+cold+ $
                  ' -CATALOG_NAME '+coldcat+' -CATALOG_TYPE ASCII' + $
                  ' -PARAMETERS_NAME '+outparam+ $
                  ' -WEIGHT_IMAGE '+weight+','+weight+ $
                  ' -WEIGHT_TYPE '+weight_type+','+weight_type+' -MAG_ZEROPOINT '+zp_eff[0]+ $
                  ' -CHECKIMAGE_TYPE segmentation -CHECKIMAGE_NAME '+coldseg
;  print, sexcommand_cs
  spawn, sexcommand_cs
; create reg file for cold catalogue
  sex2ds9reg, coldcat, outpath_file[tile,0]+setup.outparam, $
              outpath_file[tile,0]+'cold.reg', 8, color='cyan', tag = 'cold'

  IF multi EQ 3 THEN BEGIN
     print, 'starting hot sex'
     sexcommand_hs = setup.sexexe+' '+image+','+image+' -c '+hot+ $
                     ' -CATALOG_NAME '+hotcat+' -CATALOG_TYPE ASCII' + $
                     ' -PARAMETERS_NAME '+outparam+ $
                     ' -WEIGHT_IMAGE '+weight+','+weight+ $
                     ' -WEIGHT_TYPE '+weight_type+','+weight_type+' -MAG_ZEROPOINT '+zp_eff[0]+ $
                     ' -CHECKIMAGE_TYPE segmentation -CHECKIMAGE_NAME '+hotseg
;     print, sexcommand_hs
     spawn, sexcommand_hs
; create reg file for hot catalogue
     sex2ds9reg, hotcat, outpath_file[tile,0]+setup.outparam, $
                 outpath_file[tile,0]+'hot.reg', 6, color='yellow', tag = 'hot'
  ENDIF
  
;read in hotcat and coldcat
  cold_table = read_sex_table(coldcat, outparam)
  idx = where(cold_table.kron_radius EQ 0, ct)
; if cold_table[0].number eq 0, the cold table is empty! In this case,
; do NOT do all the rest, simply use hotcat as outcat. Copying to new
; files being done below
  IF cold_table[0].number NE 0 THEN BEGIN
     IF ct GT 0 THEN cold_table[idx].kron_radius = median(cold_table.kron_radius)
     
     IF multi EQ 3 THEN BEGIN
        hot_table = read_sex_table(hotcat, outparam)
        idx = where(hot_table.kron_radius EQ 0, ct)
        IF ct GT 0 THEN $
           hot_table[idx].kron_radius = median(hot_table.kron_radius)
     ENDIF
     
;loop over all elements in cold list and find "good" objects in hot
;catalogue, i.e. objects that are not contained in the cold list and 
;calculate ellipse parameters
     print, 'combining hot & cold catalogues'
     IF multi EQ 3 THEN BEGIN
        cold_cxx = cold_table.cxx_image / cold_table.kron_radius^2.
        cold_cyy = cold_table.cyy_image / cold_table.kron_radius^2.
        cold_cxy = cold_table.cxy_image / cold_table.kron_radius^2.
        ncold = n_elements(cold_table)
        
        hot_cxx = hot_table.cxx_image / hot_table.kron_radius^2.
        hot_cyy = hot_table.cyy_image / hot_table.kron_radius^2.
        hot_cxy = hot_table.cxy_image / hot_table.kron_radius^2.
        nhot = n_elements(hot_table)
        
;      fits_read, coldseg, segim, seghd
;      fits_read, hotseg, segim_hot, seghd_hot
        segim = readfits(coldseg, seghd, /silent)
        segim_hot = readfits(hotseg, seghd_hot, /silent)
        
        FOR i=0ul, ncold-1 DO BEGIN
           statusline, 'creating segmentation map for cold objects '+strtrim(i,2)
           idx = where(cold_cxx[i]*(hot_table.x_image- $
                                    cold_table[i].x_image)^2.+ $
                       cold_cyy[i]*(hot_table.y_image- $
                                    cold_table[i].y_image)^2.+ $
                       cold_cxy[i]*(hot_table.x_image-cold_table[i].x_image)* $
                       (hot_table.y_image-cold_table[i].y_image) GT setup.enlarge^2., $
                       ct)
           IF ct GT 0 THEN BEGIN
              hot_table = hot_table[idx]
           ENDIF ELSE BEGIN
              multi = 2
              print, 'all objects contained in cold catalogue'
              i = ulong(ncold)
           ENDELSE
        ENDFOR
     ENDIF
     
;read in the segmentation images and add objects from hot segmentation
;map to cold segmentation map, but only at pixels where no object was
;defined in cold segmentation map. Then write result!
     IF multi EQ 3 THEN BEGIN
        
        print, ''
        nhot = n_elements(hot_table)
        off = max(cold_table.number)+1
        FOR i=0ul, nhot-1 DO BEGIN
           statusline, 'adding hot objects '+strtrim(i,2)
           idx = where(segim_hot EQ hot_table[i].number, ct)
           IF ct GT 0 THEN BEGIN
;prefer cold pixels in output segmentation map
;        FOR j=0ul, ct-1 DO $
;                IF segim[idx[j]] EQ 0 THEN segim[idx[j]] = off+i
;prefer hot pixels in output segmentation map
              segim[idx] = off+i
           ENDIF
           hot_table[i].number = off+i
        ENDFOR
        writefits, outseg, segim, seghd,/silent
     ENDIF ELSE file_copy, coldseg, outseg, /overwrite
     
     IF multi EQ 3 THEN table_all = [cold_table, hot_table] $
     ELSE table_all = cold_table
     
     IF exclude[0, 0] GE 0 THEN BEGIN
        x = reform(exclude[0, *])
        y = reform(exclude[1, *])

        print, ''
        print, 'removing a total of '+strtrim(n_elements(x),2)+' flagged false detections from this tile'
        srccor, x, y, table_all.x_image, table_all.y_image, setup.exclude_rad, l, e, $
                option = 1, /silent
; can invert_index be replaced with a_not_b.pro????  inv2=a_not_b(arr1,arr2)
        hlpind = lindgen(n_elements(table_all.number))
        e = a_not_b(hlpind, e)
;old version
;           e = invert_index(table_all.number, e)
        IF e[0] EQ -1 THEN message, 'No elements left in catalogue!'
        table_all = table_all[e]
        
        ntot = n_elements(table_all)
;      fits_read, outseg, segim_org, seghd
        segim_org = readfits(outseg, seghd, /silent)
        segim = segim_org*0
        
        FOR i=0ul, ntot-1 DO BEGIN
           idx = where(segim_org EQ table_all[i].number, ct)
           IF ct GT 0 THEN segim[idx] = i+1
           table_all[i].number = i+1
        ENDFOR
        
        writefits, outseg, segim, seghd,/silent
     ENDIF
     
     write_sex_table, table_all, outcat
  ENDIF ELSE BEGIN
     print, 'cold catalogue is empty, using hot as combined cat'
     spawn, 'cp '+hotcat+' '+outcat+' '
     spawn, 'cp '+hotseg+' '+outseg+' '
  ENDELSE

; create reg file for combined catalogue
  sex2ds9reg, outcat, outpath_file[tile,0]+setup.outparam, $
              outpath_file[tile,0]+'combined.reg', 12, color='red', tag = 'hot'
  
  IF setup.outonly THEN $
     file_delete, hotcat, coldcat, hotseg, coldseg, /quiet, /allow_nonexistent, /noexpand_path
END

;==============================================================================
FUNCTION calc_dist_to_edge, file, catalogue
;works only on single files and catalogues
;currently efficiency is optimised for 7000*7000 pix images
  
;get image size to calculate central position
;   fits_read, file, im, hd, exten_no=0
  im = readfits(file, hd, exten_no=0, /silent)
  xsz = float(sxpar(hd, 'NAXIS1'))
  ysz = float(sxpar(hd, 'NAXIS2'))
  xcnt = xsz*0.5
  ycnt = ysz*0.5
  
  scale = 5.
  
  nx = fix(xsz/scale)
  ny = fix(ysz/scale)
;find the value of the outer pixels
  med = median(im)
  null = where(im EQ med)
  sm = im
;set the border pixels to an extreme value
  nullval = long(-5*max(abs(im)))
  sm[null] = nullval
;remove inner "border" pixels
  sm = median(sm, 3)
;to increase computation speed reduce size
  sm = congrid(sm, nx, ny)
;add outermost boundary
  sm[0, *] = nullval
  sm[*, 0] = nullval
  sm[nx-1, *] = nullval
  sm[*, ny-1] = nullval
  null = where(sm EQ nullval)
  
  x0 = (findgen(nx) MOD nx)+1
  x = sm*0
  FOR i=0ul, ny-1 DO x[*, i] = x0
  y0 = (findgen(ny) MOD ny)+1
  y = sm*0
  FOR i=0ul, nx-1 DO y[i, *] = y0
  
  radius = catalogue.x_image*0.
  x_null = x[null]
  y_null = y[null]
  sx = catalogue.x_image/scale
  sy = catalogue.y_image/scale
  FOR i=0ul, n_elements(catalogue.number)-1 DO $
     radius[i] = min(sqrt((x_null-sx[i])^2+(y_null-sy[i])^2))*scale
  return, radius
END

PRO create_stamp_file, image, sexcat, sexparam, outparam, sizefac, setup
;routine to create postage stamp parameter files OUTPARAM for an
;IMAGE, using a SEXCAT with the paramater file SEXPARAM
;OUTPARAM has the following format:
;NUMBER X_IMAGE Y_IMAGE left_edge right_edge bottom_edge top_edge
  
;read in sextractor catalogue
  cat = read_sex_table(sexcat, sexparam)
  
  cat.theta_image /= !radeg
  rad = cat.a_image*cat.kron_radius
  
;get image size
  hd = headfits(image)
  nx = sxpar(hd, 'NAXIS1')
  ny = sxpar(hd, 'NAXIS2')
    
  openw, 1, outparam
;loop over objects and calculate postage stamp sizes
  FOR i=0ul, n_elements(cat)-1 DO BEGIN
     xfac = rad[i]*(abs(sin(cat[i].theta_image))+ $
                    (1-cat[i].ellipticity)*abs(cos(cat[i].theta_image)))* $
            sizefac
     yfac = rad[i]*(abs(cos(cat[i].theta_image))+ $
                    (1-cat[i].ellipticity)*abs(sin(cat[i].theta_image)))* $
            sizefac
     major = max([xfac, yfac])
     minor = min([xfac, yfac])
     
     ang = cat[i].theta_image*!radeg
     cirrange, ang
     IF ang GT 180 THEN ang -= 180
     IF ang GT 90 THEN ang -= 180
     IF abs(ang) LT 45 THEN BEGIN
        xfac = major & yfac = minor
     ENDIF ELSE BEGIN
        xfac = minor & yfac = major
     ENDELSE
     
     xlo = round(cat[i].x_image)-round(xfac)
     IF xlo LT 0 THEN xlo = 0
     xhi = round(cat[i].x_image)+round(xfac)
     IF xhi GT nx-1 THEN xhi = nx-1
    
     ylo = round(cat[i].y_image)-round(yfac)
     IF ylo LT 0 THEN ylo = 0
     yhi = round(cat[i].y_image)+round(yfac)
     IF yhi GT ny-1 THEN yhi = ny-1
     
; write out parameters for postages stamps
     printf, 1, cat[i].number, cat[i].x_image, cat[i].y_image, $
             xlo, xhi, ylo, yhi, format = '(I,2(F),4(I))'
  ENDFOR
  close, 1
END

PRO cut_stamps, image, param, outpath, pre, post, cut_list
;cut postage stamps given an IMAGE (string, including path) and a
;PARAM file (string, including path, created by
;create_stamp_file). Output postage stamps are written into the
;OUTPATH. PRE is a string that gets prepended to the individual
;postage stamp file names (e.g. 'v'+'123.fits' --> 'v123.fits')
;   fits_read, image, im, hd
  im = readfits(image, hd, /silent)
  nx = sxpar(hd, 'NAXIS1')
  ny = sxpar(hd, 'NAXIS2')
  
;identify number of lines in the param table
;lines beginning with '#' are treated as comments
  nobj = n_lines(param)
  
  
  cat = mrd_struct(['id', 'x', 'y', 'xlo', 'xhi', 'ylo', 'yhi'], $
                   ['0L', '0.d0', '0.d0', '0L', '0L', '0L', '0L'], $
                   nobj, /no_execute)
  
  fill_struct, cat, param
  
; only cut the stamps when the content is useful and not filled with 0s
;hlp, cat.id
  IF cat[0].id NE 0 THEN BEGIN
;  ndigits = fix(alog10(max(cat.id)))+1
     FOR i=0ul, nobj-1 DO BEGIN
; postage stamp only cut when required.
        IF cut_list[i] EQ 1 THEN BEGIN
           hextract, im, hd, out, outhd, $
                     cat[i].xlo, cat[i].xhi, cat[i].ylo, cat[i].yhi, /silent
           num = strtrim(cat[i].id, 2)
;    WHILE strlen(num) LT ndigits DO num = '0'+num
           writefits, outpath+pre+num+post+'.fits', out, outhd,/silent
        ENDIF
     ENDFOR
  ENDIF
END

PRO create_skymap, whtfile, segfile, sex, sexparam, mapfile, scale, offset
;for a given weight (WHT) and segmentation image (SEG) plus the
;corresponding sextractor catalogue (SEX) create a small version of
;the obj/sky map (MAP) with slightly different format.
;WHT: file
;SEG: file
;SEX: sextractor catalogue (filename including path)
;MAP: sky map. 2d-int array with the same dimensions as the
;input arrays containing at each position information about the source
;of the flux in this pixel.
;
;the output map has the format:
;-1 means blank
; 0 means sky
;>1 means number of objects
  
  wht = readfits(whtfile, hd,/silent)
  seg = readfits(segfile, hd,/silent)
  
  nx = n_elements(wht[*, 0])
  ny = n_elements(wht[0, *])
  
;initialise the map with sky-pixels only 
  map = intarr(nx, ny)
  
;now calculate sextractor ellipses and mark object positions
  table = read_sex_table(sex, sexparam)
  
  theta = table.theta_image/!radeg
  rad = table.a_image*table.kron_radius*scale+offset
  FOR i=0ul, n_elements(table)-1 DO BEGIN
     xfac = (rad[i]*(abs(sin(theta[i]))+(1-table[i].ellipticity)* $
                     abs(cos(theta[i])))) > 10
     yfac = (rad[i]*(abs(cos(theta[i]))+(1-table[i].ellipticity)* $
                     abs(sin(theta[i])))) > 10
     major = max([xfac, yfac])
     minor = min([xfac, yfac])
     
     ang = table[i].theta_image
     cirrange, ang
     IF ang GT 180 THEN ang -= 180
     IF ang GT 90 THEN ang -= 180
     IF abs(ang) LT 45 THEN BEGIN
        xfac = major & yfac = minor
     ENDIF ELSE BEGIN
        xfac = minor & yfac = major
     ENDELSE
     xlo = round(table[i].x_image-xfac) > 0 < (nx-1)
     xhi = round(table[i].x_image+xfac) > 0 < (nx-1)
     ylo = round(table[i].y_image-yfac) > 0 < (ny-1)
     yhi = round(table[i].y_image+yfac) > 0 < (ny-1)
     dist_ellipse, arr, [xhi-xlo+1, yhi-ylo+1], $
                   table[i].x_image-xlo, table[i].y_image-ylo, $
                   1./(1.-table[i].ellipticity), table[i].theta_image-90
     idx = where(arr LE (rad[i] > 5))
; old version, but crashes if idx is empty    arr = fix(arr) & arr = arr*0 & arr[idx] = 1
     arr = fix(arr) & arr = arr*0
     if idx[0] ne -1 then arr[idx] = 1
     
     FOR j=xlo, xhi DO BEGIN
        FOR k=ylo, yhi DO BEGIN
           IF arr[j-xlo, k-ylo] EQ 1 THEN map[j, k] = map[j, k]+1
        ENDFOR
     ENDFOR
  ENDFOR
  
;mark as off pixels (blank, no light on pixel) those where the weight
;is zero and there is no object in the segmentation map at that
;position
  off = where(wht EQ 0, ct)
  IF ct GT 0 THEN map[off] = -1
  
  writefits, mapfile+'.fits', map, hd,/silent
  
END

FUNCTION rel_pos_ang, tgt_x, tgt_y, ctr_x, ctr_y, ctr_pa
;calculate the position angle of a position tgt_x/tgt_y with respect
;to a position ctr_x/ctr_y with an intrinsic position angle
;ctr_pa. Angles are in radians and counted anticlockwise from x.
  
  alpha = tgt_x*0.
  dist = sqrt((tgt_x-ctr_x)^2.+(tgt_y-ctr_y)^2.)
  j = where(dist GT 0, gt0)
  IF gt0 GT 0 THEN BEGIN
     alpha[j] = asin((tgt_y[j]-ctr_y[j])/dist[j])
     i = where(tgt_x[j] LT ctr_x[j] AND tgt_y[j] LT ctr_y[j], ct)
     IF ct GT 0 THEN alpha[j[i]] = -alpha[j[i]]-!pi
     i = where(tgt_x[j] LT ctr_x[j] AND tgt_y[j] GE ctr_y[j], ct)
     IF ct GT 0 THEN alpha[j[i]] = !pi-alpha[j[i]]
  ENDIF
  
  alpha -= ctr_pa
  i = where(alpha GT 2*!pi, ct)
  IF ct GT 0 THEN alpha[i] -= 2*!pi
  
  return, alpha
END

FUNCTION ell_rad, _a, _b, _phi
;for an ellipse with semi-major axis a and semi-minor axis b calculate
;the radius at an angle phi (radians counter-clockwise from x)
  na = n_elements(_a)
  nb = n_elements(_b)
  nphi = n_elements(_phi)
  IF na EQ 1 THEN a = _a[0] ELSE a = _a
  IF nb EQ 1 THEN b = _b[0] ELSE b = _b
  IF nphi EQ 1 THEN phi = _phi[0] ELSE phi = _phi
;the input arrays have to fulfill one requirement:
;1) all variables with more than 1 element have same number of
;   elements OR
;2) all variables have 1 element
  i = where((nar = [na, nb, nphi]) GT 1, ct)
  
  IF ct GT 1 THEN BEGIN         ;more than 1 array has more than 1 element
     j = where(nar[i[1:*]] NE nar[i[0]], ct)
     IF ct GT 0 THEN message, 'input arrays do not fulfill requirements ' + $
                              '(e.g. a=[1,2] & phi=[1,2,3])'
  ENDIF
  
  i = where(a EQ 0, cta)
  IF i GT 0 THEN a[i] = 1
  j = where(b EQ 0, ctb)
  IF j GT 0 THEN b[j] = 1
  r = 1./sqrt(cos(phi)^2./a^2.+sin(phi)^2./b^2.)
  IF i GT 0 THEN r[i] = 0
  IF j GT 0 THEN r[j] = 0
  
  return, r
END

PRO merge_sex_catalogues, incat, inparam, images, neighbours, outcat
;incat: array of SExtractor table filenames
;inparam: array of SExtractor parameter filenames
;images: array of image filenames
;neighbours: 2d-array n_neighbours x n_images with neighbouring frames
;outcat: filename of the output catalogue
  
  nframes = n_elements(images)
  
;read main table
  main = read_sex_table(incat[0], inparam[0])
  main_d = calc_dist_to_edge(images[0], main)
  n_main = n_elements(main.number)
  main_file = strarr(n_main)+images[0]
  main_id = intarr(n_main)
  
;loop over images
  FOR j=1ul, nframes-1 DO BEGIN
     print, j
     
;read secondary table
     sec = read_sex_table(incat[j], inparam[j])
     n_sec = n_elements(sec.number)
     sec_file = strarr(n_sec)+images[j]
     sec_id = intarr(n_main)+j
     
; ONLY DO ALL THIS IF THERE ARE ACTUALLY DETECTIONS IN THIS FRAME!!
     IF sec[0].number GT 0 THEN BEGIN
        sec_d = calc_dist_to_edge(images[j], sec)
        
;extract neighbours from main -> primary table (remove primary from
;main)
        
;consider only frames from main that are neighbours to sec
        n_neighbours = n_elements(neighbours[*, 0])
        nei = 0
        FOR i=0ul, n_neighbours-1 DO $
           nei = [nei, where(main_file EQ neighbours[i, j])]
        nei = nei[1:*]
        idx = where(nei GT -1, ct)
        IF ct GT 0 THEN BEGIN
           nei = nei[idx]
;neighbouring frames will be split from the main catalogue and added
;back afterwards
           pri = main[nei]
           pri_d = main_d[nei]
           pri_file = main_file[nei]
           pri_id = main_id[nei]
           
; can invert_index be replaced with a_not_b.pro????  inv2=a_not_b(arr1,arr2)
           hlpind = lindgen(n_elements(main.number))
           idx = a_not_b(hlpind, nei)
; old version
;         idx = invert_index(main.number, nei)
           IF idx[0] EQ -1 THEN BEGIN
              main = main[0]
              main_d = -1
              main_file = ''
              main_id = -1
           ENDIF ELSE BEGIN
              main = main[idx]
              main_d = main_d[idx]
              main_file = main_file[idx]
              main_id = main_id[idx]
           ENDELSE
           
;combine primary with secondary
           tot = [pri, sec]
           tot_d = [pri_d, sec_d]
           tot_file = [pri_file, sec_file]
           tot_id = [pri_id, sec_id]
           
;sort by distance from border
           ord = reverse(sort(tot_d))
           tot = tot[ord]
           tot_d = tot_d[ord]
           tot_file = tot_file[ord]
           tot_id = tot_id[ord]
           tot_dupe = fix(tot_d*0)
           
;loop over objects
           cur = 0L
           WHILE 1 DO BEGIN
;find objects in other table within SExtractor ellipse and remove
              IF tot_id[cur] EQ j THEN rc = where(tot_id NE tot_id[cur]) $
              ELSE rc = where(tot_id EQ j)
              
              adxy, headfits(tot_file[cur]), tot[rc].alpha_j2000, $
                    tot[rc].delta_j2000, x_rc, y_rc
              x_rc++
              y_rc++
              
              phi_rc = rel_pos_ang(x_rc, y_rc, tot[cur].x_image, $
                                   tot[cur].y_image, tot[cur].theta_image/!radeg)
              rp_rc = 1/sqrt((cos(phi_rc)/(tot[cur].a_image* $
                                           tot[cur].kron_radius))^2+ $
                             (sin(phi_rc)/(tot[cur].b_image* $
                                           tot[cur].kron_radius))^2.)
              r_rc = sqrt((x_rc-tot[cur].x_image)^2.+(y_rc-tot[cur].y_image)^2.)
              
              dup = where(r_rc LT rp_rc*1.1+5, ct)
              IF ct GT 0 THEN BEGIN
                 tot_dupe[rc[dup]] = 1
                 idx = where(tot_dupe EQ 0, ct)
                 tot = tot[idx]
                 tot_d = tot_d[idx]
                 tot_file = tot_file[idx]
                 tot_id = tot_id[idx]
                 tot_dupe = fix(tot_d*0)
              ENDIF
              cur++
              
              IF cur GT n_elements(tot_d)-1 THEN BREAK
           ENDWHILE
;end loop over objects
           
        ENDIF ELSE BEGIN
;the current frame is too far from already processed tiles (none of
;its neighbours are in the main_file) --> include all objects in main_file
           tot = sec
           tot_d = sec_d
           tot_file = sec_file
           tot_id = sec_id
        ENDELSE
        
;combine main with resulting table
        main = [main, tot]
        main_d = [main_d, tot_d]
        main_file = [main_file, tot_file]
        main_id = [main_id, tot_id]
        
        idx = where(main_d GE 0, ct)
        IF ct EQ 0 THEN message, 'No objects in combined catalogue' $
        ELSE BEGIN
           main = main[idx]
           main_d = main_d[idx]
           main_file = main_file[idx]
           main_id = main_id[idx]
        ENDELSE
     ENDIF
     
;end loop over images
  ENDFOR
  
;calculate position angle in WCS for all sources
  frames = main_file[uniq(main_file, sort(main_file))]
  nframes = n_elements(frames)
  FOR i=0ul, nframes-1 DO BEGIN
     getrot, headfits(frames[i]), rot
     j = where(main_file EQ frames[i], ct)
     IF ct GT 0 THEN main[j].theta_world = main[j].theta_image-rot
  ENDFOR
  i = where(main.theta_world GT 90, ct)
  IF ct GT 0 THEN main[i].theta_world -= 180
  i = where(main.theta_world LT -90, ct)
  IF ct GT 0 THEN main[i].theta_world += 180
  
  write_sex_table, main, outcat, add_column = main_file
END

FUNCTION sky_contrib, iso_x, iso_y, iso_a, iso_q, iso_pa, phi, $
                      targ_x, targ_y, targ_re, targ_q, targ_pa, targ_n, $
                      targ_m, zeropt, exptime, central_flux, psf = psf
;compute the sky contribution of a specified secondary source at a
;specified isophote of the primary source
  
;the isophote of the central source is defined by:
;iso_x, iso_y: position (pixel coordinates)
;iso_a: semi-major axis radius
;iso_q: axis ratio
;iso_pa: position angle
  
;phi: the angle at which the isophote is to be calculated (may be an
;array)
  
;the shape of the to the sky contributing source is defined by:
;targ_x, targ_y: position (pixel coordinates)
;targ_re: half-light radius (pixels)
;targ_q, targ_pa: axis ratio and position angle
;targ_n: sersic index
;targ_m, zeropt, exptime: magnitude, zeropoint and exposure time
  
;psf: psf-array. Useful if iso_a and phi correspond to an
;image (originate from a set of x,y positions)
  
;all angles are measured from x to y
  
;compute the semi-major axis of the isophote
  iso_b = iso_a*iso_q
  
;the current position in the coordinate system of the isophote
  ellrad = ell_rad(iso_a, iso_b, phi-iso_pa)
  
;the current position in the normal coordinate system centred on the
;isophote
  xp = ellrad*cos(phi)
  yp = ellrad*sin(phi)
  
;the current position in the normal coordinate system centred on the
;target
  px = xp+iso_x
  py = yp+iso_y
  
;distance from target to current position
  r = sqrt((px-targ_x)^2.+(py-targ_y)^2.)
  
;position angle of the current position in the system of the target
  xi = rel_pos_ang(px, py, targ_x, targ_y, targ_pa)
  
;semi-major axis radius of the current position in the system of the
;target
  rr = r*sqrt(cos(xi)^2.+sin(xi)^2./targ_q^2.)
  
;the distance between the centre of the isophote and the position of
;the target
  rc = sqrt((targ_x-iso_x)^2.+(targ_y-iso_y)^2.)
  
;the position angle of the centre of the isophote in the coordinate
;system of the target
  xic = rel_pos_ang(iso_x, iso_y, targ_x, targ_y, targ_pa)
  
;the radius at which the flux is to be measured corresponding to the
;centre of the isophote
  rrc = rc*sqrt(cos(xic)^2.+sin(xic)^2./targ_q^2.)
  
;the total flux of the target
  ftot = 10.^(-0.4*(targ_m-zeropt))*exptime
  
  kap = (kappa(targ_n))[0]
  f0 = ftot/(2*!pi*targ_re^2.*exp(kap)*targ_n*kap^(-2.*targ_n)* $
             gamma(2.*targ_n)*targ_q)
  
;the flux at the current position in counts
  f = f0*exp(-kap*((rr/targ_re)^(1./targ_n)-1.))
  
;optional convolution with a psf
  IF keyword_set(psf) THEN f = convolve(f, psf)
  
;the flux at the centre of the isophote in counts
  central_flux = f0*exp(-kap*((rrc/targ_re)^(1./targ_n)-1.))
  
  return, f
END

FUNCTION sersic_flux, _r, _phi, _q, _f0, _re, _n
;for a given distance r at an angle phi relative to the position angle
;of the object with an axis ratio q, central flux f0, half-light
;radius re and sersic index n compute the sersic flux.
  
;the input arrays have to fulfill one requirement:
;1) all variables with more than 1 element have same number of
;   elements OR
;2) all variables have 1 element
  nr = n_elements(_r)
  nphi = n_elements(_phi)
  nq = n_elements(_q)
  nf0 = n_elements(_f0)
  nre = n_elements(_re)
  nn = n_elements(_n)
  
  i = where((nar = [nr, nphi, nq, nf0, nre, nn]) GT 1, ct)
  
  IF ct GT 1 THEN BEGIN         ;more than 1 array has more than 1 element
     j = where(nar[i[1:*]] NE nar[i[0]], ct)
     IF ct GT 0 THEN message, 'input arrays do not fulfill requirements ' + $
                              '(e.g. r=[1,2] & phi=[1,2,3])'
  ENDIF
  
  IF nphi EQ 1 THEN phi = _phi[0] ELSE phi = _phi
  IF nq EQ 1 THEN q = _q[0] ELSE q = _q
  r = cos(phi)^2.
  phi = sin(temporary(phi))^2./q^2.
  delvarx, q
  phi = sqrt(r+temporary(phi))
  IF nr EQ 1 THEN r = _r[0] ELSE r = _r
  r = temporary(r)*phi
  delvarx, phi
  IF nf0 EQ 1 THEN f0 = _f0[0] ELSE f0 = _f0
  IF nre EQ 1 THEN re = _re[0] ELSE re = _re
  IF nn EQ 1 THEN n = _n[0] ELSE n = _n
  IF n_elements((kap = kappa(n))) EQ 1 THEN kap = kap[0]
  
  return, f0*exp(-kap*((r/re)^(1./n)-1.))
END

PRO contrib_targets, exptime, zeropt, scale, offset, power, t, c, cut, $
                     nums, frames, nobj_max
;power: convert flux_radius to proper half-light radius of a spheroid (1.4)
;t: sextractor table (ASSUMES THAT ALL IMAGE POSITIONS ARE ON THE SAME WCS FRAME)
;c: idx of current object id
;cut: magnitude cut (e.g. 4.5)
;nums: OUTPUT, numbers of the most contributing objects
;frames: OUTPUT, frames of the most contributing objects

; set standard values, if no fit has been done yet
  n = t.number*0.+4.
  q = t.b_image/t.a_image
  re = t.flux_radius^power
  mag = t.mag_best
  theta_image = t.theta_world-t[c].theta_world+t[c].theta_image
  IF n_elements(t) GT 0 THEN BEGIN
     
; create a table with all fitted values (for better profiles)
; try using 'match'
     ident1 = strtrim(t.org_image,2)+':'+strtrim(t.number,2)
     ident2 = strtrim(t.frame[0],2)+':'+strtrim(t.number,2)
     match, ident1, ident2, id_idx1, id_idx2
; identify the ones that have finished fits (re>0)
     wh_re_gt0 = where(t[id_idx1].re_galfit GE 0,cntregt0)
     IF cntregt0 GT 0 THEN BEGIN
        n[id_idx2[wh_re_gt0]] = t[id_idx1[wh_re_gt0]].n_galfit
        q[id_idx2[wh_re_gt0]] = t[id_idx1[wh_re_gt0]].q_galfit
        re[id_idx2[wh_re_gt0]] = t[id_idx1[wh_re_gt0]].re_galfit
        mag[id_idx2[wh_re_gt0]] = t[id_idx1[wh_re_gt0]].mag_galfit
        theta_image[id_idx2[wh_re_gt0]] = theta_image[id_idx2[wh_re_gt0]]-t[c].theta_image+ $
                                          90+t[id_idx1[wh_re_gt0]].pa_galfit
     ENDIF
     wh_theta_gt0 = where(theta_image GT 180, cnt_gt)
     IF cnt_gt GT 0 THEN theta_image[wh_theta_gt0] -= 180
     wh_theta_lt0 = where(theta_image LT 180, cnt_lt)
     IF cnt_lt GT 0 THEN theta_image[wh_theta_lt0] += 180
  ENDIF
  
; flux normalization of all objects in sersic profiles
  ftot = 10.^(-0.4*(mag-zeropt))*exptime
  kap = (kappa(n))[0]
  f0 = ftot/(2*!pi*re^2.*exp(kap)*n*kap^(-2.*n)*gamma(2.*n)*q)
  
;distance of all objects from the current object c
  d = sqrt((t[c].x_image-t.x_image)^2.+(t[c].y_image-t.y_image)^2.)

;position angle of the current source in the reference system of all
;other galaxies
  pa_all = rel_pos_ang(t[c].x_image, t[c].y_image, t.x_image, t.y_image, $
                       theta_image/!radeg)

;calculate sersic flux for all objects to get
;contribution of all galaxies at the position of the current source
  c_all = -2.5*alog10(sersic_flux(d, pa_all, q, f0, re, n))
;distance that all objects 'reach' towards the current object
  r_all = ell_rad(t.a_image, t.b_image, pa_all)*t.kron_radius*scale+ $
          offset
  
;position angle of all other galaxies in the reference system of the
;current source
  pa_ctr = rel_pos_ang(t.x_image, t.y_image, t[c].x_image, t[c].y_image, $
                       theta_image[c]/!radeg)
;contribution of the current source at the position of all galaxies
  c_ctr = -2.5*alog10(sersic_flux(d, pa_ctr, q[c], f0[c], re[c], n[c]))
;distance that the current object 'reaches' towards all objects
  r_ctr = ell_rad(t[c].a_image, t[c].b_image, pa_ctr)*t[c].kron_radius* $
          scale+offset
  con = r_ctr+r_all
  
; now sort by influence and find objects above threshold
  o = sort(c_all)
  no_fit = where(con[o] LT d[o], ct)
  IF ct EQ 0 THEN BEGIN
     nums = -1
     frames = ''
  ENDIF ELSE BEGIN
;      print, 'cur', 'num', 'x', 'y', 'dist', 'con', 'mag', 'magc', $
;             'c_all', 'c_ctr', format = '(2(A5),4(A7),2(A8),2(A6))'

;     nno = n_elements(no_fit)-1
;      forprint, t[c].number+fltarr(11 < nno), $
;                t[o[no_fit[0:10 < nno]]].number, $
;                t[o[no_fit[0:10 < nno]]].x_image, $
;                t[o[no_fit[0:10 < nno]]].y_image, $
;                d[o[no_fit[0:10 < nno]]], $
;                con[o[no_fit[0:10 < nno]]], $
;                t[o[no_fit[0:10 < nno]]].mag_best, $
;                t[c].mag_best+fltarr(11 < nno), $
;                c_all[o[no_fit[0:10 < nno]]], $
;                c_ctr[o[no_fit[0:10 < nno]]], $
;                format = '(2(I5),4(F7.0),2(F8.2),2(F6.2))'
     
; limit to worst offenders
; sort all arrays by strength
     c_all_sort = c_all[o[no_fit]]
     t_sort = t[o[no_fit]]
     
;     grad = where(c_all[o[no_fit]] LT cut, ct)
     grad = where(c_all_sort LT cut, ct)
     IF n_elements(grad) GT nobj_max THEN grad = grad[0:nobj_max-1]

     IF ct GT 0 THEN BEGIN
;        nums = t[o[no_fit[grad]]].number
;        frames = t[o[no_fit[grad]]].frame[0]
        nums = t_sort[grad].number
        frames = t_sort[grad].frame[0]
     ENDIF ELSE BEGIN
        nums = -1
        frames = ''
     ENDELSE
  ENDELSE

;   print, 'gradient sources:'
;   forprint, nums, ' '+frames
  
;  print, 'sources to be included in fit:'
;  print, 'num', 'dist', 'connect', format = '(A5,2(A8))'
;  to_fit = where(con GE d, ct)
;  IF ct GT 0 THEN forprint, t[to_fit].number, d[to_fit], con[to_fit], $
;    format = '(I5,2(F8.0))'
  
END

PRO dist_angle, arr, sz, x, y
;similar to dist_circle
;estimate an array of size [sz[0],sz[1]] containing the polar angle
;wrt a given position x,y
  arr = fltarr(sz[0], sz[1])
  dx = findgen(sz[0])-x
  dy = findgen(sz[1])-y
  
  xzero = where(dx EQ 0, ct_xzero)
  IF ct_xzero GT 0 THEN dx[xzero] = 1
  
  FOR i=0ul, sz[1]-1 DO BEGIN
     arr[0, i] = atan(dy[i]/dx)
  ENDFOR 
  
  j = where(dx LT 0, ct)
  IF ct GT 0 THEN arr[j, *] += !pi
  
  k = where(dy EQ 0, ctk)
  IF ct_xzero GT 0 AND ctk GT 0 THEN arr[xzero, k] = 0
  
  j = where(arr GT !pi, ct)
  IF ct GT 0 THEN arr[j] -= 2*!pi
END

PRO getsky_loop, setup, current_obj, table, rad, im0, hd, map, exptime, zero_pt, $
                 scale, offset, power, cut, files, psf, dstep, wstep, gap, $
                 nslope, sky_file, out_file, out_cat, out_param, out_stamps, $
                 global_sky, global_sigsky, conv_box, nums, frames, $
                 b, orgpath_pre, outpath_file, outpath_file_no_band, $
                 nband, xarr, yarr, seed
;current_obj: idx of the current object in table
;table: sextractor table (frame of current object and surrounding
;neighbouring frames)
;rad: a*kron radius for each object in table
;im0: image pixel array
;map: skymap pixel array
;exptime, zero_pt, scale, offset, power, cut: parameters for contrib_targets (now run externally, though)
;files: string - file that contains info about input/output files
;psf: PSF pixel array
;dstep, wstep, gap: parameters controlling the sky annulus
;sizes
;dstep: distance between individual sky annuli
;wstep: width of sky annulus
;gap: gap between sextractor isophote and inner sky annulus
;nslope: number of sky annuli to check slope break criterium
;sky_file: output filename for sky summary
;out_file: galfit output file prefix
;global_sky, global_sigsky: global sky value plus scatter
;conv_box: convolution box size for subtracting sources
;nums, frames: object numbers and frames of potential contributing sources. INPUT parameters now!
;b: current band index
;compute the sky for a single source
;nband: numbers of bands in the data
;xarr, yarr: arrays needed for skymap, created outside for all bands at once to improve speed
;seed: seed for random selection of pixels in the sky determination
  
;computation flag:
;0 - done
;1 - image too small for accurate sky calculation (source is too large)
;2 - secondary contributing source (not subtracted) requires radius
;    larger than image
;4 - sky not converged, still decreasing with radius at image boundary
;    (image too small)
;8 - not enough measurements, value from median(image) taken  ;
;    initially 'SExtractor taken', but not good in multi-band mode,
;    replace if dual-image sextractor is being used
;16 - value from contributing source taken
;32 - image is masked entirey, no sky determination was
;    possible. Assuming 0 as sky value
;64 - distribution of sky pixels too narrow, so gaussian fit can not
;     be run. Instead, resistant_mean and sigma are used
  sky_flag = 0
  
; rename image so im0 is not changed
  im = im0
  
;get the size of the image
  sz_im = (size(im))[1:2]
;make sure that the image is large enough to compute the sky
;compute max radius possible in current image
  idx = where(map EQ 0, ct)
;IF ct EQ 0 THEN message, 'NO PIXELS TO CALCULATE SKY'
  IF ct EQ 0 THEN BEGIN
     openw, 1, sky_file
     printf, 1, 0, 0, 0, table[current_obj].mag_best, 32
     close, 1    
  ENDIF
  IF ct LT 0 THEN message, 'SOMETHING REALLY WRONG IN GETSKY_LOOP'
  
  IF ct GT 0 THEN BEGIN
; The following line has to be done here, as map (and idx) could be different for each band)
     max_rad = max(sqrt((table[current_obj].x_image-xarr[idx])^2+ $
                        (table[current_obj].y_image-yarr[idx])^2))
;if the maximum possible radius for the image is exceeded -> start
;with 0 and set flag
     IF rad[current_obj] GT max_rad THEN BEGIN
        rad[current_obj] = 0
        sky_flag += 1
     ENDIF
     
     contrib_sky = 1e30
     
     IF nums[0] LT 0 THEN BEGIN
;no contributing sources found-------------------------------------------------
;starting radius for sky calculation is already defined
;nothing to be done
     ENDIF ELSE BEGIN
;contributing sources FOUND----------------------------------------------------
        orgim = setup.images
        outpath = setup.outpath
        outpre = setup.outpre
        outpath = set_trailing_slash(outpath)
        
;total number of contributing sources
        n_contrib = n_elements(nums)
        
;array for distance to contributing sources
        dist = fltarr(n_contrib)
        
;flag: if set contributing source was subtracted from the image
        subtract = intarr(n_contrib)
        
;loop over all contributing sources============================================
        FOR current_contrib=0ul, n_contrib-1 DO BEGIN
           i_con = where(table.number EQ nums[current_contrib] AND $
                         strtrim(table.frame[0],2) EQ strtrim(frames[current_contrib],2))
           dist[current_contrib] = $
              sqrt((table[i_con].x_image-table[current_obj].x_image)^2+ $
                   (table[i_con].y_image-table[current_obj].y_image)^2)+ $
              table[i_con].a_image*table[i_con].kron_radius*scale
           
;find the GALFIT output file for the current contributing source
           idx = where(strtrim(orgim[*,1],2) EQ strtrim(table[i_con].frame[1],2))
           
           objnum = round_digit(table[i_con].number, 0, /str)
           current_contrib_file = outpath[idx]+outpre[idx,1]+objnum+'_'+setup.galfit_out+'.fits'
           
           IF file_test(strtrim(current_contrib_file,2)) THEN BEGIN
;a GALFIT result exists for the current contributing source--------------------
              
;read in the GALFIT image fitting results from current_file
              forward_function read_sersic_results ; not quite sure why this is needed
              forward_function read_sersic_results_old_galfit ; not quite sure why this is needed
              IF setup.version ge 4. then par = read_sersic_results(current_contrib_file,nband,setup)
              IF setup.version lt 4. then par = read_sersic_results_old_galfit(current_contrib_file,setup)      
              contrib_sky = [contrib_sky, par.sky_galfit_band[b-1]]
              
;subtract the source from the image               
;position is in the original postage stamp frame
              tb = read_sex_table(outpath_file[idx,0]+out_cat, $
                                  outpath_file[0,0]+out_param)
              
              itb = where(tb.number EQ table[i_con].number)
              
              stamp_file = outpath_file_no_band[idx,0]+setup.stampfile
; stamp_file = outpath[idx]+outpre[idx]+out_stamps
              cat = mrd_struct(['id', 'x', 'y', 'xlo', 'xhi', 'ylo', 'yhi'], $
                               ['0L', '0.d0', '0.d0', '0L', '0L', '0L', '0L'], $
                               n_lines(stamp_file), /no_execute)
              fill_struct, cat, stamp_file
              icat = where(cat.id EQ table[i_con].number)
              
; here, the sersic profiles are subtracted
              par.x_galfit_band[b-1] = par.x_galfit_band[b-1]+cat[icat].xlo-1-tb[itb].x_image+ $
                                       table[i_con].x_image
              par.y_galfit_band[b-1] = par.y_galfit_band[b-1]+cat[icat].ylo-1-tb[itb].y_image+ $
                                       table[i_con].y_image
              
;the total flux of the target
              ftot = 10.^(-0.4*(par.mag_galfit_band[b-1]-zero_pt[b]))*exptime[b]
              
              kap = (kappa(par.n_galfit_band[b-1]))[0]
              f0 = ftot/(2*!pi*par.re_galfit_band[b-1]^2.*exp(kap)*par.n_galfit_band[b-1]* $
                         kap^(-2.*par.n_galfit_band[b-1])*gamma(2.*par.n_galfit_band[b-1])* $
                         par.q_galfit_band[b-1])
              
;arrays for radius and angle of current contributing source
              dist_angle, ang_arr, sz_im, par.x_galfit_band[b-1], par.y_galfit_band[b-1]
              rad_arr = cos(ang_arr)^2.
              ang_arr = sin(temporary(ang_arr))^2./par.q_galfit_band[b-1]^2.
              ang_arr = sqrt(rad_arr+temporary(ang_arr))
              dist_ellipse, rad_arr, sz_im, par.x_galfit_band[b-1], par.y_galfit_band[b-1], $
                            1./par.q_galfit_band[b-1], par.pa_galfit_band[b-1]
              rad_arr = temporary(rad_arr)*ang_arr
              obj = f0*exp(-kap*((rad_arr/par.re_galfit_band[b-1])^(1./par.n_galfit_band[b-1])-1.))
;        obj = sersic_flux(rad_arr, ang_arr, par.q_galfit_band[b-1], f0, par.re_galfit_band[b-1], par.n_galfit_band[b-1])
              delvarx, rad_arr, ang_arr
              
;make sure the convolution size is a power of 2
              cs = 2
              cb = conv_box/2
              WHILE cs LT cb DO cs *= 2
              cs -= 1
              
;convolution box must be fully inside the postage stamp
              x0 = round(par.x_galfit_band[b-1]-1) > (cs+1) < (sz_im[0]-cs-2)
              y0 = round(par.y_galfit_band[b-1]-1) > (cs+1) < (sz_im[1]-cs-2)
              
;convolve the central region of the source with the PSF
              conv = convolve(obj[x0-cs:x0+cs+1, y0-cs:y0+cs+1], psf)
              obj[x0-cs:x0+cs+1, y0-cs:y0+cs+1] = conv
              
;subtract the contributing source
              im = temporary(im)-obj
              delvarx, obj
              
;            print, systime(), ' done'
              
;set subtract flag
              subtract[current_contrib] = 1
              
;current contributing source was subtracted from image-------------------------
           ENDIF
           
        ENDFOR
;loop over all contributing sources============================================
;define new starting radius for sky calculation
        notsubtracted = where(subtract EQ 0, ct)
        IF ct GT 0 THEN BEGIN
           new_rad = max(dist[notsubtracted])
;if requested starting radius is outside frame, set flag
           IF new_rad GT max_rad THEN sky_flag += 2 $
           ELSE rad[current_obj] = new_rad > rad[current_obj]
        ENDIF
        
;contributing sources FOUND----------------------------------------------------
     ENDELSE
;array containing radii
     nstep = ulong(max(sz_im)/float(dstep))
     radius = findgen(nstep)*dstep+rad[current_obj]+gap
     
;array containing sky in one ring
     ringsky = radius*0
     ringsigma = radius*0
     
;this is the loop over different radii
;  plot, [0, 0], [0, 0], /nodata, xr = [0, max_rad < 6000], xsty = 1, $
;        ysty = 1, chars = 2, xtitle = 'radius', ytitle = 'flux', $
;        yr = global_sky+[-global_sigsky*0.3, global_sigsky]
     
;  loadct, 39, /silent
     
;arrays for the radius, sky, scatter of the last nslope 
     sl_sct = (sl_rad = (sl_sky = fltarr(nslope)))
     
;   print,'radii loop'
     last_slope = 1
     slope_change = 0
     min_sky = 1e30
     min_sky_rad = 1e30
     min_sky_sig = 999
     min_sky_flag = 0
;loop over radii ==============================================================
     FOR r=0l, nstep-1 DO BEGIN
;first calculate ellipses
        theta = table[current_obj].theta_image/!radeg
        xfac = ((rad[current_obj]* $
                 (abs(sin(theta))+(1-table[current_obj].ellipticity)* $
                  abs(cos(theta)))) > 10)+dstep*r+wstep
        yfac = ((rad[current_obj]* $
                 (abs(cos(theta))+(1-table[current_obj].ellipticity)* $
                  abs(sin(theta)))) > 10)+dstep*r+wstep
        major = max([xfac, yfac])
        minor = min([xfac, yfac])
        
        ang = theta*!radeg
        cirrange, ang
        IF ang GT 180 THEN ang -= 180
        IF ang GT 90 THEN ang -= 180
        IF abs(ang) LT 45 THEN BEGIN
           xfac = major & yfac = minor
        ENDIF ELSE BEGIN
           xfac = minor & yfac = major
        ENDELSE
        xlo = round(table[current_obj].x_image-xfac) > 0 < (sz_im[0]-1)
        xhi = round(table[current_obj].x_image+xfac) > 0 < (sz_im[0]-1)
        ylo = round(table[current_obj].y_image-yfac) > 0 < (sz_im[1]-1)
        yhi = round(table[current_obj].y_image+yfac) > 0 < (sz_im[1]-1)
;polar coordinates for the current ring
        dist_ellipse, arr, [xhi-xlo+1, yhi-ylo+1], $
                      table[current_obj].x_image-xlo, $
                      table[current_obj].y_image-ylo, $
                      1./(1.-table[current_obj].ellipticity), $
                      theta*!radeg-90
;    dist_angle, alp, [xhi-xlo+1, yhi-ylo+1], table[current_obj].x_image-xlo, $
;                table[current_obj].y_image-ylo
        
;this index contains all pixels that do not contain object flux inside
;the current ring
        ring_empty_idx = where(arr LE rad[current_obj]+dstep*r+wstep AND $
                               arr GT rad[current_obj]+dstep*r AND $
                               map[xlo:xhi, ylo:yhi] EQ 0, ct)
        delvarx, arr
        
;calculate the flux value in the ring as a function of radius
        IF ct LT 5 THEN BEGIN
;no pixels to calculate sky
           ringsky = -1
           ringsigma = 1e30
        ENDIF ELSE BEGIN
;define a square image of sky pixels max 250x250 pix^2 in size (to
;make procedure faster)
           skyim = (im[xlo:xhi, ylo:yhi])[ring_empty_idx]
           n_ring = n_elements(ring_empty_idx)
;clip 3 sigma outliers first
           resistant_mean, skyim, 3, m, s, nr
           s *= sqrt(n_ring-1-nr)
           ring_sig_clip = where(skyim GT m-3*s AND skyim LT m+3*s, nw)
           IF nw GT 4 THEN BEGIN
              skyim = skyim[ring_sig_clip]
              n_ring = n_elements(skyim)
;reduce size to 250x250 pixels
              npix = ulong(sqrt(n_ring)) < 250
              skyim = reform(skyim[randomu(seed, npix^2)*npix^2], npix, npix)
              
              IF n_elements(uniq(skyim)) LT 5 THEN BEGIN
                 ringsky = float(mean(skyim,/double))
                 ringsigma = 1e30
              ENDIF ELSE BEGIN
;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
                 plothist, skyim, x, y, chars = 2, bin = s*3*2/50., /noplot
                 IF n_elements(x) GT 3 THEN BEGIN
                    f = gaussfit(x, y, a, sigma=sig, nterms = 3)
                    ringsky = a[1]
                    ringsigma = sig[1] ;a[2]/(n_elements(x)^2-1)
                 ENDIF ELSE BEGIN
                    resistant_mean, skyim, 3, ringsky, ringsigma, nrej
                    ringsigma *= sqrt(n_elements(skyim)-1-nr)
                    sky_flag += 64
                 ENDELSE
;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
              ENDELSE
           ENDIF ELSE BEGIN
              ringsky = -1
              ringsigma = 1e30
           ENDELSE
           delvarx, skyim, ring_empty_idx
        ENDELSE
        plotsym, 0, /fill
;replace one value in the slope arrays
        sl_rad[r MOD nslope] = radius[r]
        sl_sky[r MOD nslope] = ringsky
        sl_sct[r MOD nslope] = ringsigma
        
        fit = [0, 0]
;if at least nslope sky values are calculated estimate the slope
        IF r GE nslope-1 THEN BEGIN
           good = where(sl_sct LT global_sigsky*3, ct)
;calculate slope robustly
           IF ct GT 1 THEN fit = robust_linefit(sl_rad[good], sl_sky[good])
;         print, r+1, radius[r], fit[1], n_elements(good)
           
;calculate a sky value
           idx = where(sl_rad GT 0, ct)
;check if enough measurements available
           IF ct GT 3 THEN BEGIN
              resistant_mean, sl_sky[idx], 3, new_sky
              new_sky_sig = sqrt(total(sl_sct[idx]^2))/ct
              IF NOT finite(new_sky_sig) THEN new_sky_sig = 1e30
              min_sky_flag0 = 0
           ENDIF ELSE BEGIN
;too few measurements -> take value from SExtractor
; why SExtractor and not previous curvefit value?
              min_sky_flag0 = 8
              new_sky = global_sky
              new_sky_sig = global_sigsky
; SExtractor sky (BAD idea in multi-band mode)
;              new_sky = table[current_obj].background
;              new_sky_sig = 999
           ENDELSE
           IF new_sky LT min_sky and new_sky_sig lt 1e20 THEN BEGIN
              min_sky_flag = min_sky_flag0
              min_sky = new_sky
              IF ct GT 0 THEN min_sky_rad = float(mean(sl_rad[idx],/double)) $
              ELSE min_sky_rad = radius[r]
              min_sky_sig = new_sky_sig
;        oploterror, min_sky_rad, min_sky, min_sky_sig, $
;                    psym = 8, col = 150, errcol = 150
           ENDIF
           
;stop the loop if the slope is positive
           IF fit[1] GT 0 AND slope_change THEN BREAK
           IF fit[1] GT 0 AND last_slope LT 0 THEN slope_change++
           last_slope = fit[1]
        ENDIF
        
;    oploterror, radius[r], ringsky, ringsigma, psym = 8
        
;break criterium not fulfilled, but image boundary reached -> image
;too small -> set flag
        IF radius[r] GT max_rad THEN BEGIN
           sky_flag += 4
           BREAK
        ENDIF
     ENDFOR
;loop over radii done==========================================================
;   print,'radii loop done'
     
;get sky from last nslope measurements
     idx = where(sl_rad GT 0 AND sl_sct lt 1e20, ct)
;check if enough measurements available
     IF ct GT 3 THEN BEGIN
        resistant_mean, sl_sky[idx], 3, new_sky
        new_sky_sig = sqrt(total(sl_sct[idx]^2))/ct
        sky_flag0 = 0
     ENDIF ELSE BEGIN
;too few measurements -> take value from SExtractor
        sky_flag0 = 8
        new_sky = global_sky
        new_sky_sig = global_sigsky
; SExtractor sky (BAD idea in multi-band mode)
;        new_sky = table[current_obj].background
;        new_sky_sig = 999
        fit = [0, 0]
     ENDELSE
     IF ct GT 0 THEN sky_rad = float(mean(sl_rad[idx],/double)) ELSE sky_rad = radius[(r-1) >0]
     IF new_sky GT min_sky THEN BEGIN
        sky_flag0 = min_sky_flag
        new_sky = min_sky
        sky_rad = min_sky_rad
        new_sky_sig = min_sky_sig
     ENDIF
     sky_flag += sky_flag0
     
;  loadct, 39, /silent
;  oplot, [0, max_rad], fit[0]+[0, max_rad]*fit[1], col = 150
     
;  hor, new_sky, col = 50
;  hor, [-1, 1]*2*new_sky_sig+new_sky, linestyle = 2, col = 50
     
;   print, new_sky, new_sky_sig
     contrib_sky = min(contrib_sky)
;   print, contrib_sky
     
     IF contrib_sky LT new_sky THEN BEGIN
        new_sky = contrib_sky
        sky_flag += 16
     ENDIF
     
;write output sky file
     openw, 1, sky_file
     printf, 1, new_sky, new_sky_sig, sky_rad, table[current_obj].mag_best, $
             sky_flag
     close, 1
     print, 'sky estimated by getsky_loop as '+strtrim(new_sky,2)+' +/- '+strtrim(new_sky_sig,2)+' at radius '+strtrim(sky_rad,2)
 ENDIF
END

PRO create_mask, table0, wht, seg, paramfile, mask_file, mask_file_primary, im_file, image, $
                 current, scale, offset, nums, frames, lim_gal, lim_star, $
                 stel_slope, stel_zp, objects, corner, b
;objects and corner are output needed by prepare_galfit
  
;in the case of contributing sources, the TABLE is changed, so keep a
;backup copy
  table = table0
  
;current is the index of the current object
  nx_big = n_elements(wht[*, 0])
  ny_big = n_elements(wht[0, *])
  
;   print, systime()
  
  rad = table.a_image*table.kron_radius*scale+offset
  
  readcol, paramfile, $
           pnum, px, py, pxlo, pxhi, pylo, pyhi, $
           comment = '#', format = 'L,F,F,L,L,L,L', /silent
;            comment = '#', format = 'I,F,F,L,L,L,L', /silent
;            ; could be the problem for >integer detections on one tile
  
  i = where(pnum EQ table[current].number, ct)
  IF ct EQ 0 THEN message, 'no postage stamp definition found ' + $
                           'corresponding to current object'
  pnum = pnum[i]
  px = px[i]
  py = py[i]
  pxlo = (pxlo[i])[0] < (nx_big-1) > 0
  pxhi = (pxhi[i])[0] < (nx_big-1) > 0
  pylo = (pylo[i])[0] < (ny_big-1) > 0
  pyhi = (pyhi[i])[0] < (ny_big-1) > 0
  
;==============================================================================
;create a blank mask image
  mask = wht[pxlo:pxhi, pylo:pyhi]
  segm = seg[pxlo:pxhi, pylo:pyhi]
  badpix = where(mask EQ 0, badpix_ct)
  mask *= 0
  
  nx = n_elements(mask[*, 0])
  ny = n_elements(mask[0, *])
  
;objects for the GALFIT start file (primary source must be included)
  objects = current
  
  ntab = n_elements(table.number)
  
;position angle in radians (this might go across images with different
;coordinate systems: so have to use theta_world)
  theta_image = table.theta_world-table[current].theta_world+ $
                table[current].theta_image
  
  theta = theta_image/!radeg
  
;calculate the radius array for the primary source
  dist_ellipse, arr_current, [pxhi-pxlo+1, pyhi-pylo+1], $
                table[current].x_image-pxlo, $
                table[current].y_image-pylo, $
                1./(1.-table[current].ellipticity), $
                theta[current]*!radeg-90

  con_num = 0ul
;loop over all sources in the table (including neighbouring frames)
  FOR i=0ul, ntab-1 DO BEGIN
     IF i EQ current THEN CONTINUE
;calculate the angle between the loop source and the [current] object
     dy = table[current].y_image-table[i].y_image
     dx = table[current].x_image-table[i].x_image
     IF dx EQ 0 THEN angle = 0.5*!pi $
     ELSE angle = atan(dy/dx)
;calculate the angle pointing from the loop source to the [current]
;object
     angle1 = angle-theta[i]
;calculate the angle pointing from the [current] object to the loop
;source
     angle2 = angle-theta[current]
     
;calculate the extent of the loop source towards the [current] object
     r1 = sqrt(1/(sin(angle1)^2/(rad[i]*(1-table[i].ellipticity))^2+ $
                  cos(angle1)^2/rad[i]^2))
;calculate the extent of the [current] source towards the loop object
     r2 = sqrt(1/(sin(angle2)^2/(rad[current]*( $
          1-table[current].ellipticity))^2+cos(angle2)^2/rad[current]^2))
;calculate the distance between the two objects
     d = sqrt(dx^2+dy^2)
     
;dist_ellipse takes long: calculate only necessary parts!
     xfac = (rad[i]*(abs(sin(theta[i]))+(1-table[i].ellipticity) $
                     *abs(cos(theta[i])))) > 10
     yfac = (rad[i]*(abs(cos(theta[i]))+(1-table[i].ellipticity) $
                     *abs(sin(theta[i])))) > 10
     major = max([xfac, yfac])
     minor = min([xfac, yfac])
     
     ang = theta[i]*!radeg
     cirrange, ang
     IF ang GT 180 THEN ang -= 180
     IF ang GT 90 THEN ang -= 180
     IF abs(ang) LT 45 THEN BEGIN
        xfac = major & yfac = minor
     ENDIF ELSE BEGIN
        xfac = minor & yfac = major
     ENDELSE
     xlo = round(table[i].x_image-xfac) > 0 < (nx_big-1)
     xhi = round(table[i].x_image+xfac) > 0 < (nx_big-1)
     ylo = round(table[i].y_image-yfac) > 0 < (ny_big-1)
     yhi = round(table[i].y_image+yfac) > 0 < (ny_big-1)
;xlo,ylo,xhi,yhi are on the system of the input image (and not the
;postage stamp)
     xlo = (xlo-pxlo) > 0 < (nx-1)
     xhi = (xhi-pxlo) > 0 < (nx-1)
     ylo = (ylo-pylo) > 0 < (ny-1)
     yhi = (yhi-pylo) > 0 < (ny-1)
;now it is on the correct frame
     
;calculate the radius array for the loop source
     IF total([xhi-xlo+1, yhi-ylo+1]) GT 2 THEN $
        dist_ellipse, arr, [xhi-xlo+1, yhi-ylo+1], table[i].x_image-pxlo-xlo, $
                      table[i].y_image-pylo-ylo, 1./(1.-table[i].ellipticity), $
                      theta[i]*!radeg-90 $
     ELSE arr = 1e30
     
;status of loop source
     small_mask = mask[xlo:xhi, ylo:yhi]
;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
     IF table[current].fwhm_image LT $
        10^(stel_slope*table[current].mag_best+stel_zp) THEN $
           faintlim = lim_star $
     ELSE faintlim = lim_gal
     
     IF r1+r2 GT d AND table[i].mag_best LT table[current].mag_best+faintlim THEN BEGIN
;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;loop source has overlap with current --> secondary
        idx = where(arr LE (rad[i])[0] AND (small_mask MOD 2) EQ 0, ct)
        IF ct GT 0 THEN small_mask[idx] += 1
;remember these for the GALFIT start file
        objects = [objects, i]
     ENDIF ELSE BEGIN
;loop source has NO overlap with current --> tertiary
;if loop source is contributing source on current frame, make secondary
        coni = where(table[i].number EQ nums AND strtrim(table[i].frame[0],2) EQ strtrim(frames,2), $
                     con)
        IF con GT 0 THEN BEGIN
           plus = 1
           con_num = [con_num, nums[coni]]
        ENDIF ELSE plus = 2
        idx = where(arr LE (rad[i])[0] AND small_mask LT 2, ct)
        IF ct GT 0 THEN small_mask[idx] += plus
     ENDELSE
     
     IF r1+r2 GT d AND table[i].mag_best GE table[current].mag_best+faintlim THEN BEGIN
;loop source has overlap with current --> secondary
        idx = where(arr LE (rad[i])[0] AND (small_mask MOD 2) EQ 0 $
                    AND arr_current[xlo:xhi, ylo:yhi] LE (rad[current])[0], $
                    ct)
        IF ct GT 0 THEN small_mask[idx] = 0
     ENDIF
     
     mask[xlo:xhi, ylo:yhi] = small_mask
  ENDFOR
  IF n_elements(con_num) GT 1 THEN con_num = con_num[1:*]
  
;bitwise: 0: no object
;         1: secondary
;         2: tertiary
;set secondaries to 0
  idx = where(mask EQ 1 OR mask EQ 3, ct)
  IF ct GT 0 THEN mask[idx] = 0
  
;*****************************************************************************
;clear area of primary
  dist_ellipse, arr, [nx, ny], table[current].x_image-pxlo, $
                table[current].y_image-pylo, $
                1./(1.-table[current].ellipticity), theta[current]*!radeg-90
  idx = where(arr LE (rad[current])[0], ct)
  IF ct GT 0 THEN mask[idx] = 0

; create primary mask if band = 1
  IF b EQ 1 THEN BEGIN
     arr = arr*0
     arr[idx] = 1
     writefits, mask_file_primary+'.fits', byte(arr), headfits(im_file+'.fits'),/silent
  ENDIF
;*****************************************************************************
  
;set rest to 1
  mask = mask < 1
  
;pixels without weight in the wht image will be masked
  IF badpix_ct GT 0 THEN mask[badpix] = 1
  
;any pixel in the segmentation map not from the primary or secondary
;will be masked
  mask1 = segm < 1
  FOR i=0ul, n_elements(objects)-1 DO BEGIN
     idx = where(segm EQ table[objects[i]].number AND $
                 strtrim(table[objects[i]].frame[b],2) EQ strtrim(image,2) AND segm GT 0, ct)
     IF ct GT 0 THEN mask1[idx] = 0
  ENDFOR
  FOR i=0ul, n_elements(con_num)-1 DO BEGIN
     idx = where(segm EQ con_num[i] AND segm GT 0, ct)
     IF ct GT 0 THEN mask1[idx] = 0
  ENDFOR

  mask = (mask+mask1) < 1
  writefits, mask_file+'.fits', byte(mask), headfits(im_file+'.fits'),/silent
  
  corner = [pxlo, pylo]
END

PRO prepare_galfit, setup, objects, files, corner, table0, obj_file, im_file, sigma_file, $ 
                    constr_file, mask_file, mask_file_primary, psf_file, out_file, sky_file, $
                    conv_box, zero_pt, plate_scl, num_contrib, frame_contrib, $
                    current, out_cat, out_param, out_stamps, conmaxre, $
                    conminm, conmaxm, setup_version, nband, $
                    outpre, maxdeg, bd_fit = bd_fit
;   setup_version = 4.
;objects contains an index of secondary sources
;in the case of contributing sources, the TABLE is changed, so keep a
;backup copy
  table = table0
;current is the index of the current object
  hdr = headfits(im_file[1]+'.fits')
  xmax = sxpar(hdr, 'NAXIS1')
  ymax = sxpar(hdr, 'NAXIS2')
  
;write constraint file for secondaries
;if not keyword_set(bd_fit) then begin
  openw, 1, constr_file
  printf, 1, '# Component/    parameter   constraint  Comment'
  printf, 1, '# operation                  values'
  FOR j=2ul, n_elements(objects)+1 DO BEGIN
     lo = strtrim(setup.conminn, 2)
     hi = strtrim(setup.conmaxn, 2)
     printf, 1, j, ' n '+lo+' to '+hi
     printf, 1, j, ' re 0.3 to '+strtrim(conmaxre, 2)
     printf, 1, j, ' q 0.01  to 1.'
     printf, 1, j, ' mag '+strtrim(conminm, 2)+' '+strtrim(conmaxm, 2)
     printf, 1, j, ' mag 0 to 40'
;     printf, 1, j, ' pa -360 to 360'
     printf, 1, j, ' x '+strtrim(-xmax/10.)+' '+strtrim(xmax/10.)
     printf, 1, j, ' y '+strtrim(-ymax/10.)+' '+strtrim(ymax/10.)
  ENDFOR
  close, 1
;ENDIF
  
;write obj file header plus sky
  openw, 1, obj_file, width=1000
; WRITE FILES & STUFF
  printf, 1, '# IMAGE PARAMETERS'
  
; check how many postage stamps contain (a useful amount of) data and
; restrict maximum number of degrees in polynomial
  maxdeg = nband
  IF setup.do_restrict THEN BEGIN
     FOR b=1,nband DO BEGIN
        deg_im = readfits(strtrim(im_file[b],2)+'.fits',1, /silent)
        deg_wht = readfits(strtrim(mask_file[b],2)+'.fits',1,/silent)
        deg_prim = readfits(strtrim(mask_file_primary,2)+'.fits',1,/silent)
        deg_npix_prim = float(n_elements(where(deg_prim EQ 1)))
; masked pixels have value 1!
        hlpin = where(deg_prim EQ 1 AND (deg_wht EQ 1 OR deg_im EQ 0), deg_npix_prim_mask)
        delvarx, hlpin
        
; correct if more than x% of pixels are masked or have value 0 within
; the primary elliipse
        IF deg_npix_prim_mask/deg_npix_prim GT setup.restrict_frac_primary/100. THEN $
           maxdeg = maxdeg-1
     ENDFOR
  ENDIF
  
; INPUT IMAGE
  A_po=''
  FOR b=1,nband DO BEGIN
     A_po=A_po+strtrim(im_file[b],2)+'.fits'
     IF b LT nband THEN A_po=A_po+','
  ENDFOR
  printf, 1, 'A) '+a_po+'   #input file'
  IF setup.version GE 4. THEN BEGIN
     A1_po=''
     FOR b=1,nband DO BEGIN
        A1_po=A1_po+strtrim(setup.stamp_pre[b],2)
        IF b LT nband THEN A1_po=A1_po+','
     ENDFOR
     printf, 1, 'A1) '+A1_po+'     # Band labels (can be omitted if fitting a single band)'
     A2_po=''
     FOR b=1,nband DO BEGIN
        A2_po=A2_po+strtrim(setup.wavelength[b],2)
        IF b LT nband THEN A2_po=A2_po+','
     ENDFOR
     printf, 1, 'A2) '+A2_po+'     # Band wavelengths (choice of wavelength units is arbitrary, as long as consistent)'
  ENDIF
  
; OUTPUT image
  printf, 1, 'B) '+out_file+'.fits    # output file name'
  
; noise image.
  C_po=''
  FOR b=1,nband DO BEGIN
     IF strtrim(sigma_file[b],2) NE 'none' THEN addition = strtrim(sigma_file[b],2)+'.fits'
     IF strtrim(sigma_file[b],2) EQ 'none' THEN addition = 'none'
     C_po=C_po+addition
     IF b LT nband THEN C_po=C_po+','
  ENDFOR
  printf, 1, 'C) '+C_po+'           # Noise image name (made from data if blank or "none")'
  
; PSFs
  D_po=''
  FOR b=1,nband DO BEGIN
     D_po=D_po+strtrim(psf_file[b],2)
     IF b LT nband THEN D_po=D_po+','
  ENDFOR
  printf, 1, 'D) '+D_po+' kernel  # Input PSF image and (optional) diffusion kernel'
  printf, 1, 'E) 1           # PSF oversampling factor relative to data'
  
; mask image
  F_po=''
  FOR b=1,nband DO BEGIN
     F_po=F_po+strtrim(mask_file[b],2)+'.fits'
     IF b LT nband THEN F_po=F_po+','
  ENDFOR
  printf, 1, 'F) '+F_po
  printf, 1, 'G) '+constr_file
  printf, 1, 'H) 1 '+strtrim(xmax, 2)+' 1 '+strtrim(ymax, 2)+'       # Image region to fit (xmin xmax ymin ymax)'
  printf, 1, 'I) '+round_digit(conv_box, 0, /str)+'   '+round_digit(conv_box, 0, /str)+'         # Size of convolution box (x y)'
  J_po=''
  FOR b=1,nband DO BEGIN
     J_po=J_po+strtrim(round_digit(setup.zp[b],4,/str),2)
     IF b LT nband THEN J_po=J_po+','
  ENDFOR
  printf, 1, 'J) '+J_po+'     # Magnitude photometric zeropoint'
  printf, 1, 'K) '+round_digit(plate_scl, 5, /str)+' '+round_digit(plate_scl, 5, /str)+'           # Plate scale (dx dy).'
  printf, 1, 'O) regular             # Display type (regular, curses, both)'
  printf, 1, 'P) 0                   # Create ouput only? (1=yes; 0=optimize)'
  printf, 1, 'S) 0                   # Modify/create objects interactively?'
  printf, 1, 'W) '+setup.gal_output+' # GALFIT output file format'
  printf, 1, ''
  printf, 1, ''
  printf, 1, '# sky'
  printf, 1, ''
  printf, 1, ' 0) sky'
; WRITE SKY
  SKY_po=''
  SKY_po2=''
  FOR b=1, nband DO BEGIN
     IF file_test(strtrim(sky_file[b],2)) EQ 0 THEN $
        message, 'sky file corresponding to current object was not found in band '+strtrim(setup.stamp_pre[b],2)
     openr, 2, sky_file[b]
     readf, 2, sky, dsky, skyrad, sky_magobj, flag
     close, 2

     IF sky GT 1.0 THEN used_sky = round_digit(sky,3,/string,/L64) $
     ELSE used_sky = sigfig(sky,4)
     SKY_po=SKY_po+strtrim(used_sky,2)
     SKY_po2=SKY_po2+'0'
     IF b LT nband THEN SKY_po=SKY_po+','
     IF b LT nband THEN SKY_po2=SKY_po2+','
  ENDFOR
;print, SKY_po
  band_po=' '
  IF setup.version GE 4. AND nband GT 1 THEN band_po='band'
  printf, 1, ' 1) '+SKY_po+'    '+SKY_po2+'  '+band_po+'       # sky background       [ADU counts]'
  SKYG_po=''
  SKYG_po2=''
;   printf, 1, ' 2) 0.000      0       # dsky/dx (sky gradient in x)'
;   printf, 1, ' 3) 0.000      0       # dsky/dy (sky gradient in y)'
  FOR b=1, nband DO BEGIN
     SKYG_po=SKYG_po+'0.000'
     SKYG_po2=SKYG_po2+'0'
     IF b LT nband THEN SKYG_po=SKYG_po+','
     IF b LT nband THEN SKYG_po2=SKYG_po2+','
  ENDFOR
; add BAND here and there!
  printf, 1, ' 2) '+SKYG_po+'    '+SKYG_po2+'  '+band_po+'      # dsky/dx (sky gradient in x)'
  printf, 1, ' 3) '+SKYG_po+'    '+SKYG_po2+'  '+band_po+'      # dsky/dy (sky gradient in y)'
  printf, 1, ' Z) 0                  # output image'
  printf, 1, ''
  printf, 1, ''
  close, 1
  
; define all paths and names again
  orgim = setup.images
  orgwheights = setup.weights
  outpath = set_trailing_slash(setup.outpath)
  outpath_band = set_trailing_slash(setup.outpath_band)
  outpre = setup.outpre
  
  outpath_galfit = strtrim(outpath[*,0]+setup.galfit_out_path,2)
  outpath_pre = outpath_band+outpre
  outpath_file = outpath
  FOR q=0,nband DO outpath_file[*,q]=outpath_pre[*,q]+strtrim(setup.stamp_pre[q],2)+'.'
  outpath_file_no_band = outpath
  FOR q=0,nband DO outpath_file_no_band[*,q]=outpath[*,q]+outpre[*,q]
  
;find the GALFIT output file for the current contributing source
  num_current = round_digit(table[current].number, 0, /str)

;find the faintest source with proper magnitude measurement (non 99) and
;correct 99 sources
  nn = where(table[objects].mag_best GT 80, n_nn)
  IF n_nn GT 0 THEN BEGIN
     good_mag = where(table[objects].mag_best LT 80, ngood_mag)
     IF ngood_mag EQ 0 THEN table[objects[nn]].mag_best = zero_pt[0] $
     ELSE $
        table[objects[nn]].mag_best = max(table[objects[good_mag]].mag_best)+2
  ENDIF

;loop over all (primary & secondary) sources
  FOR i=0ul, n_elements(objects)-1 DO BEGIN
     object_description = ' '
     idx = where(strtrim(orgim[*,0],2) EQ strtrim(table[objects[i]].frame[0],2), ct)
     secout_file = outpath_galfit[idx]+outpre[idx,1]+ $
                   round_digit(table[objects[i]].number, 0, /str)+'_'+strtrim(setup.galfit_out,2)+'.fits'
     object_id = strtrim(outpre[idx,1]+round_digit(table[objects[i]].number, 0, /str),2)
     
; for some reason this is needed to find read_sersic_results as a
; function, not a variable. Worked before and still works fine in the
; command line.
     forward_function read_sersic_results
     forward_function read_sersic_results_old_galfit

     IF file_test(strtrim(secout_file,2)) THEN BEGIN
;sources with existing fit will be included as static source
        IF setup.version ge 4. then par = read_sersic_results(secout_file,nband,setup)
        IF setup.version lt 4. then par = read_sersic_results_old_galfit(secout_file,setup)      
        
;problem: position from GALFIT is relative to original postage
;stamp. Need to compute offset using SExtractor input for that fit
;position is in the original postage stamp frame
        tb = read_sex_table(outpath_file[idx,0]+out_cat, $
                            outpath_file[idx,0]+out_param)
        itb = where(tb.number EQ table[objects[i]].number)
        
        stamp_file = outpath_file_no_band[idx,0]+setup.stampfile
;         stamp_file = outpath[idx]+outpre[idx]+out_stamps
        cat = mrd_struct(['id', 'x', 'y', 'xlo', 'xhi', 'ylo', 'yhi'], $
                         ['0L', '0.d0', '0.d0', '0L', '0L', '0L', '0L'], $
                         n_lines(stamp_file), /no_execute)
        fill_struct, cat, stamp_file
        icat = where(cat.id EQ table[objects[i]].number)
        
; original
;        par.x_galfit = par.x_galfit+cat[icat].xlo-1-tb[itb].x_image+ $
;                       table[objects[i]].x_image
;        par.x_galfit_band = par.x_galfit_band+cat[icat].xlo-1-tb[itb].x_image+ $
;                            table[objects[i]].x_image
;        par.y_galfit = par.y_galfit+cat[icat].ylo-1-tb[itb].y_image+ $
;                       table[objects[i]].y_image
;        par.y_galfit_band = par.y_galfit_band+cat[icat].ylo-1-tb[itb].y_image+ $
;                            table[objects[i]].y_image
; fixed version
        par.x_galfit = par.x_galfit+cat[icat].xlo-tb[itb].x_image+table[objects[i]].x_image
        par.x_galfit_band = par.x_galfit_band+cat[icat].xlo-tb[itb].x_image+ $
                            table[objects[i]].x_image
        par.y_galfit = par.y_galfit+cat[icat].ylo-tb[itb].y_image+table[objects[i]].y_image
        par.y_galfit_band = par.y_galfit_band+cat[icat].ylo-tb[itb].y_image+ $
                            table[objects[i]].y_image
        
        object_description = '(primary/secondary) object has already been fit, galfit output file exists. Reading result from file'
        fix = ['0', '0', '0', '0', '0', '0', '0']
        
     ENDIF ELSE BEGIN

;sources without fit will be included as free fit
        
; reading in file works, because it returns the correct FORMAT, but
; with useless values in it. As they will be replaced here, that does
; not matter
        forward_function read_sersic_results
        forward_function read_sersic_results_old_galfit
        IF setup.version GE 4. then par = read_sersic_results(secout_file,nband,setup)
        IF setup.version LT 4. then par = read_sersic_results_old_galfit(secout_file,setup)      
        par.x_galfit = table[objects[i]].x_image
        par.x_galfit_band = fltarr(nband)+table[objects[i]].x_image
        par.y_galfit = table[objects[i]].y_image
        par.y_galfit_band = fltarr(nband)+table[objects[i]].y_image
; NEED TO CORRECT MAGNITUDE STARTING PARAMS! DONE using offset
        par.mag_galfit = table[objects[i]].mag_best
        par.mag_galfit_band = fltarr(nband)+table[objects[i]].mag_best+setup.mag_offset[1:nband]
; THESE VALUES ARE CIRCULATIZED VALUES!! NEED TO BE AR CORRECTED
; (FROM LEE!)
        par.re_galfit = 10.^(-0.79)*table[objects[i]].flux_radius^1.87
        par.re_galfit_band = fltarr(nband)+10.^(-0.79)*table[objects[i]].flux_radius^1.87
        par.n_galfit = 2.5
        par.n_galfit_band = fltarr(nband)+2.5
        par.q_galfit = 1-table[objects[i]].ellipticity
        par.q_galfit_band = fltarr(nband)+1-table[objects[i]].ellipticity
        par.pa_galfit = table[objects[i]].theta_image-90.
        par.pa_galfit_band = fltarr(nband)+table[objects[i]].theta_image-90.
        
        object_description = '(primary/secondary) object has not yet been fit, output file does not exist'
        fix = ['1', '1', '1', '1', '1', '1', '1']
     ENDELSE
     
;if the source position is off the frame (postage stamp?): fix the position, PA and q
     IF table[objects[i]].x_image-corner[0] LT 1 OR $
        table[objects[i]].x_image-corner[0] GT xmax OR $
        table[objects[i]].y_image-corner[1] LT 1 OR $
        table[objects[i]].y_image-corner[1] GT ymax THEN BEGIN
        object_description = '(primary/secondary) object has not yet been fit, output file does not exist, outside of frame'
        fix[0] = '0' & fix[1] = '0' & fix[5] = '0' & fix[6] = '0'
     ENDIF
     
;;;;;;;;;;;; CONSTRAINTS
; check some constraints and set starting values accordingly if neccessary
     IF finite(par.re_galfit) NE 1 THEN begin
        par.re_galfit = table[objects[i]].flux_radius > 3
        par.re_galfit_band = fltarr(nband)+(table[objects[i]].flux_radius > 3)
     ENDIF
     
     IF par.re_galfit LT 0 THEN BEGIN
        par.re_galfit = table[objects[i]].flux_radius > 3
        par.re_galfit_band = fltarr(nband)+(table[objects[i]].flux_radius > 3)
     ENDIF
     par.re_galfit = par.re_galfit < float(conmaxre) > 0.3
; hard contraints work on ALL bands if this is used below.
     par.re_galfit_band = par.re_galfit_band < float(conmaxre) > 0.3
; hard contraints work only on principle band if this is used
     par.n_galfit = par.n_galfit < setup.conmaxn > setup.conminn
     par.n_galfit_band = par.n_galfit_band < setup.conmaxn > setup.conminn
     par.q_galfit = par.q_galfit > 0.01 < 1
     par.q_galfit_band = par.q_galfit_band > 0.01 < 1
     par.pa_galfit = par.pa_galfit > (-360) < 360
     par.pa_galfit_band = par.pa_galfit_band > (-360) < 360
     
     openu, 1, obj_file, /append
     printf, 1, ''
     printf, 1, '# Sersic function'
     printf, 1, '# '+object_description
     printf, 1, '# '+object_id
     printf, 1, ' 0) sersic             # Object type'
     
; for different GALFIT versions, fit will work, READOUT of parameters
; will NOT work!
     x_po=''
     x_po_fit=''
     y_po=''
     y_po_fit=''
     FOR b=1,nband DO BEGIN
        x_po = x_po+round_digit(par.x_galfit_band[b-1]-corner[0],2,/string)
        IF b LT nband THEN x_po=x_po+','
        y_po = y_po+round_digit(par.y_galfit_band[b-1]-corner[1],2,/string)
        IF b LT nband THEN y_po=y_po+','
     ENDFOR
     IF fix[0] EQ 1 THEN x_po_fit = strtrim((setup.cheb[0]+1)<maxdeg,2) else x_po_fit = '0'
     IF fix[1] EQ 1 THEN y_po_fit = strtrim((setup.cheb[1]+1)<maxdeg,2) else y_po_fit = '0'
     
     IF setup.version GE 4. THEN BEGIN
        printf, 1, ' 1) '+x_po+'  '+x_po_fit+'  '+band_po+'   # position x     [pixel]'
        printf, 1, ' 2) '+y_po+'  '+y_po_fit+'  '+band_po+'   # position y     [pixel]'
     ENDIF ELSE BEGIN
        printf, 1, ' 1) '+x_po+'  '+y_po+'   '+x_po_fit+' '+y_po_fit+'   # position x, y        [pixel]'
     ENDELSE
     mag_po=''
     mag_po_fit=''
     FOR b=1,nband DO BEGIN
        mag_po = mag_po+round_digit(par.mag_galfit_band[b-1],2,/string)
        IF b LT nband THEN mag_po=mag_po+','
     ENDFOR
     IF fix[2] EQ 1 THEN mag_po_fit = strtrim((setup.cheb[2]+1)<maxdeg,2) ELSE mag_po_fit = '0'
;if i eq o and strtrim(mag_po_fit,2) eq '0' then stop
     printf, 1, ' 3) '+mag_po+'    '+mag_po_fit+'   '+band_po+'    # total magnitude'
     
     re_po=''
     re_po_fit=''
     FOR b=1,nband DO BEGIN
        re_po = re_po+round_digit(par.re_galfit_band[b-1],2,/string)
        IF b LT nband THEN re_po=re_po+','
     ENDFOR
     IF fix[3] EQ 1 THEN re_po_fit = strtrim((setup.cheb[3]+1)<maxdeg,2) ELSE re_po_fit = '0'
     printf, 1, ' 4) '+re_po+'    '+re_po_fit+'   '+band_po+'       #     R_e              [Pixels]'
     
     n_po=''
     n_po_fit=''
     FOR b=1,nband DO BEGIN
        n_po = n_po+round_digit(par.n_galfit_band[b-1],2,/string)
        IF b LT nband THEN n_po=n_po+','
     ENDFOR
     IF fix[4] EQ 1 THEN n_po_fit = strtrim((setup.cheb[4]+1)<maxdeg,2) ELSE n_po_fit = '0'
     printf, 1, ' 5) '+n_po+'    '+n_po_fit+'   '+band_po+'       # Sersic exponent (deVauc=4, expdisk=1)'
     
     q_po=''
     q_po_fit=''
     FOR b=1,nband DO BEGIN
        q_po = q_po+round_digit(par.q_galfit_band[b-1],4,/string)
        if b lt nband then q_po=q_po+','
     ENDFOR
     IF fix[5] EQ 1 THEN q_po_fit = strtrim((setup.cheb[5]+1)<maxdeg,2) ELSE q_po_fit = '0'
     IF setup.version EQ 0. THEN str = ' 8) ' ELSE str = ' 9) '
     printf, 1, str+q_po+'    '+q_po_fit+'   '+band_po+'       # axis ratio (b/a)'
     
     
     pa_po=''
     pa_po_fit=''
     FOR b=1,nband DO BEGIN
        pa_po = pa_po+round_digit(par.pa_galfit_band[b-1],2,/string)
        IF b LT nband THEN pa_po=pa_po+','
     ENDFOR
     IF fix[6] EQ 1 THEN pa_po_fit = strtrim((setup.cheb[6]+1)<maxdeg,2) ELSE pa_po_fit = '0'
     IF setup_version EQ 0. THEN str = '9) ' ELSE str = '10) '
     printf, 1, str+pa_po+'    '+pa_po_fit+'   '+band_po+'       # position angle (PA) [Degrees: Up=0, Left=90]'
     
     printf, 1, ' Z) 0                  # output image (see above)'
     printf, 1, ''
     close, 1
  ENDFOR
  
;have to include the contributing sources potentially from other
;frames as well.
  n_nums = n_elements(num_contrib)
  
; break loop if no contributing source is found
  IF n_nums EQ 1 AND num_contrib[0] EQ -1 THEN GOTO, finish
  
;find the GALFIT output file for the current source
  num_current = round_digit(table[current].number, 0, /str)
  
  orgim = setup.images
  outpath = setup.outpath
  outpath = set_trailing_slash(outpath)
  
  openr, 1, constr_file
  line = ''
  WHILE NOT eof(1) DO readf, 1, line
  close, 1
  line = strtrim(line, 2)
  line = strmid(line, 0, strpos(line, ' '))
  ctr = fix(line)+1
  
;loop over all contributing sources.
; Those are all objects where the SExtractor ellipses don't
; overlap, but which might be important
  FOR i=0ul, n_nums-1 DO BEGIN
     object_description = ' '
     i_con = where(table.number EQ num_contrib[i] AND $
                   strtrim(table.frame[0],2) EQ strtrim(frame_contrib[i],2))
     
; this line removes secondaries from contributing sources!!
     dum = where(table[objects].number EQ num_contrib[i] AND $
                 strtrim(table[objects].frame[0],2) EQ strtrim(frame_contrib[i],2), ct)
     IF ct GT 0 THEN CONTINUE
     
     idx = where(strtrim(orgim[*,0],2) EQ strtrim(table[i_con].frame[0],2))
;    objnum = integer2string(table[i_con].number, table.number, /array)
     objnum = round_digit(table[i_con].number, 0, /str)
     current_contrib_file = outpath_galfit[idx]+outpre[idx,1]+objnum+'_'+strtrim(setup.galfit_out,2)+'.fits'
     object_id = strtrim(outpre[idx,1]+objnum,2)

; output file for contributing source exists
     IF file_test(strtrim(current_contrib_file,2)) THEN BEGIN
;sources with existing fit will be included as static source
        forward_function read_sersic_results
        forward_function read_sersic_results_old_galfit
        IF setup.version GE 4. THEN par = read_sersic_results(current_contrib_file,nband,setup)
        IF setup.version LT 4. THEN par = read_sersic_results_old_galfit(current_contrib_file,setup)      
;problem: position from GALFIT is relative to original postage
;stamp. Need to compute offset using SExtractor input for that fit
        tb = read_sex_table(outpath_file[idx,0]+out_cat, $
                            outpath_file[idx,0]+out_param)
        itb = where(tb.number EQ table[i_con].number)
        
        stamp_file = outpath_file_no_band[idx,0]+setup.stampfile
;         stamp_file = outpath[idx]+outpre[idx]+out_stamps
        cat = mrd_struct(['id', 'x', 'y', 'xlo', 'xhi', 'ylo', 'yhi'], $
                         ['0L', '0.d0', '0.d0', '0L', '0L', '0L', '0L'], $
                         n_lines(stamp_file), /no_execute)
        fill_struct, cat, stamp_file
        icat = where(cat.id EQ table[i_con].number)
        
; original
;        par.x_galfit = par.x_galfit+cat[icat].xlo-1-tb[itb].x_image+table[i_con].x_image
;        par.x_galfit_band = par.x_galfit_band+cat[icat].xlo-1-tb[itb].x_image+ $
;                            table[i_con].x_image
;        par.y_galfit = par.y_galfit+cat[icat].ylo-1-tb[itb].y_image+table[i_con].y_image
;        par.y_galfit_band = par.y_galfit_band+cat[icat].ylo-1-tb[itb].y_image+ $
;                            table[i_con].y_image
; fixed version
        par.x_galfit = par.x_galfit+cat[icat].xlo-tb[itb].x_image+table[i_con].x_image
        par.x_galfit_band = par.x_galfit_band+cat[icat].xlo-tb[itb].x_image+ $
                            table[i_con].x_image
        par.y_galfit = par.y_galfit+cat[icat].ylo-tb[itb].y_image+table[i_con].y_image
        par.y_galfit_band = par.y_galfit_band+cat[icat].ylo-tb[itb].y_image+ $
                            table[i_con].y_image
        
        object_description = 'contributing source already has been fit, output file exists. Reading result from file'
        fix = ['0', '0', '0', '0', '0', '0', '0']
     ENDIF ELSE BEGIN

;the source might be in the fit_table
        i_fit = where(table.number EQ num_contrib[i] AND $
; next line already selects objects where the fit has been tried
; already, as org_image is only then defined!
                      table.org_image EQ frame_contrib[i], ct)
        IF ct GT 0 THEN BEGIN
           IF table[i_fit].re_galfit GE 0 THEN BEGIN
              
; reading in file works, because it returns the correct FORMAT, but
; with useless values in it. As they will be replaced here, that does
; not matter
              forward_function read_sersic_results
              forward_function read_sersic_results_old_galfit
              IF setup.version GE 4. THEN par = read_sersic_results(secout_file,nband,setup)
              IF setup.version LT 4. THEN par = read_sersic_results_old_galfit(secout_file,setup)      
; replace correctly!
              par.x_galfit = table[i_con].x_image
              par.x_galfit_band = fltarr(nband)+table[i_con].x_image
              par.y_galfit = table[i_con].y_image
              par.y_galfit_band = fltarr(nband)+table[i_con].y_image
; NEED TO CORRECT MAGNITUDE STARTING PARAMS! DONE using offset
              par.mag_galfit = table[i_fit].mag_galfit
              par.mag_galfit_band = table[i_fit].mag_galfit_band
              par.re_galfit = table[i_fit].re_galfit
              par.re_galfit_band = table[i_fit].re_galfit_band
              par.n_galfit = table[i_fit].n_galfit
              par.n_galfit_band = table[i_fit].n_galfit_band
              par.q_galfit = table[i_fit].q_galfit
              par.q_galfit_band = table[i_fit].q_galfit_band
              par.pa_galfit = table[i_fit].pa_galfit
              par.pa_galfit_band = table[i_fit].pa_galfit_band
              
;if so fixate the fit
              object_description = 'contributing source already has a known fit, just subtracting'
              fix = ['0', '0', '0', '0', '0', '0', '0']
           ENDIF ELSE BEGIN
;source is in fit_table but no fit exists -> bombed -> free fit
              forward_function read_sersic_results
              forward_function read_sersic_results_old_galfit
              IF setup.version GE 4. THEN par = read_sersic_results(secout_file,nband,setup)
              IF setup.version LT 4. THEN par = read_sersic_results_old_galfit(secout_file,setup)      
              
              par.x_galfit = table[i_con].x_image
              par.x_galfit_band = fltarr(nband)+table[i_con].x_image
              par.y_galfit = table[i_con].y_image
              par.y_galfit_band = fltarr(nband)+table[i_con].y_image
; NEED TO CORRECT MAGNITUDE STARTING PARAMS! DONE using offset
              par.mag_galfit = table[i_con].mag_best
              par.mag_galfit_band = fltarr(nband)+table[i_con].mag_best+setup.mag_offset[1:nband]
              par.re_galfit = 10.^(-0.79)*table[i_con].flux_radius^1.87
              par.re_galfit_band = fltarr(nband)+10.^(-0.79)*table[i_con].flux_radius^1.87
              par.n_galfit = 2.5
              par.n_galfit_band = fltarr(nband)+2.5
              par.q_galfit = 1-table[i_con].ellipticity
              par.q_galfit_band = fltarr(nband)+1-table[i_con].ellipticity
              par.pa_galfit = table[i_con].theta_image-90.
              par.pa_galfit_band = fltarr(nband)+table[i_con].theta_image-90.
              
;the source is off the frame so just fit profile and magnitude, position fixed
              object_description = 'contributing source has no fit parameters. Fitting only basic parameters, SExtractor values as start parameters'
              fix = ['0', '0', '1', '1', '1', '0', '0']
           ENDELSE
        ENDIF ELSE BEGIN
;source is not in fit_table and not yet fit -> free fit
           forward_function read_sersic_results
           forward_function read_sersic_results_old_galfit
           IF setup.version GE 4. THEN par = read_sersic_results(secout_file,nband,setup)
           IF setup.version LT 4. THEN par = read_sersic_results_old_galfit(secout_file,setup)      
           
           par.x_galfit = table[i_con].x_image
           par.x_galfit_band = fltarr(nband)+table[i_con].x_image
           par.y_galfit = table[i_con].y_image
           par.y_galfit_band = fltarr(nband)+table[i_con].y_image
; NEED TO CORRECT MAGNITUDE STARTING PARAMS!
           par.mag_galfit = table[i_con].mag_best
           par.mag_galfit_band = fltarr(nband)+table[i_con].mag_best+setup.mag_offset[1:nband]
           par.re_galfit = 10.^(-0.79)*table[i_con].flux_radius^1.87
           par.re_galfit_band = fltarr(nband)+10.^(-0.79)*table[i_con].flux_radius^1.87
           par.n_galfit = 2.5
           par.n_galfit_band = fltarr(nband)+2.5
           par.q_galfit = 1-table[i_con].ellipticity
           par.q_galfit_band = fltarr(nband)+1-table[i_con].ellipticity
           par.pa_galfit = table[i_con].theta_image-90.
           par.pa_galfit_band = fltarr(nband)+table[i_con].theta_image-90.
           
;the source is off the frame so just fit profile and magnitude
           object_description = 'contributing source has not yet been fit or is NOT on neighbouring frames, but further away. Fixing position, only fitting basic parameters, startvalues from SExtractor'
           fix = ['0', '0', '1', '1', '1', '0', '0']
        ENDELSE
;else make it a fully free fit (unless the source is in the fit_table:
;then fit only the position, which comes still from SExtractor)
        IF par.x_galfit GT 1 AND par.x_galfit LT xmax-1 AND $
           par.y_galfit GT 1 AND par.y_galfit LT ymax-1 THEN BEGIN
           IF ct GT 0 THEN BEGIN
              IF table[i_fit].re_galfit GE 0 THEN $
                 fix = ['1', '1', '0', '0', '0', '0', '0'] $
              ELSE fix = ['1', '1', '1', '1', '1', '1', '1']
           ENDIF ELSE fix = ['1', '1', '1', '1', '1', '1', '1']
        ENDIF
     ENDELSE

     
     par.re_galfit = par.re_galfit < conmaxre > 0.3
; hard contraints work on ALL bands if this is used below. USED!!
     par.re_galfit_band = par.re_galfit_band < conmaxre > 0.3
; hard contraints work only on principle band if this is used
     par.n_galfit = par.n_galfit < setup.conmaxn > setup.conminn
     par.n_galfit_band = par.n_galfit_band < setup.conmaxn > setup.conminn
     par.q_galfit = par.q_galfit > 0.01 < 1
     par.q_galfit_band = par.q_galfit_band > 0.01 < 1
     par.pa_galfit = par.pa_galfit > (-360) < 360
     par.pa_galfit_band = par.pa_galfit_band > (-360) < 360
     
     openu, 1, obj_file, /append
     printf, 1, ''
     printf, 1, '# Sersic function'
     printf, 1, '# '+object_description
     printf, 1, '# '+object_id
     printf, 1, ' 0) sersic             # Object type'
; for different GALFIT versions, fit will work, READOUT of parameters
; will NOT work!
     band_po=' '
     IF setup.version GE 4. AND nband GT 1 THEN band_po='band'
     
     x_po=''
     x_po_fit=''
     y_po=''
     y_po_fit=''
     FOR b=1,nband DO BEGIN
        x_po = x_po+round_digit(par.x_galfit_band[b-1]-corner[0],2,/string)
        IF b LT nband THEN x_po=x_po+','
        y_po = y_po+round_digit(par.y_galfit_band[b-1]-corner[1],2,/string)
        IF b LT nband THEN y_po=y_po+','
     ENDFOR
     IF fix[0] EQ 1 THEN x_po_fit = strtrim((setup.cheb[0]+1)<maxdeg,2) ELSE x_po_fit = '0'
     IF fix[1] EQ 1 THEN y_po_fit = strtrim((setup.cheb[1]+1)<maxdeg,2) ELSE y_po_fit = '0'
     
     IF setup.version GE 4. THEN BEGIN
        printf, 1, ' 1) '+x_po+'  '+x_po_fit+'  '+band_po+'   # position x     [pixel]'
        printf, 1, ' 2) '+y_po+'  '+y_po_fit+'  '+band_po+'   # position y     [pixel]'
     ENDIF ELSE BEGIN
        printf, 1, ' 1) '+x_po+'  '+y_po+'   '+x_po_fit+' '+y_po_fit+'   # position x, y        [pixel]'
     ENDELSE
     
     mag_po=''
     mag_po_fit=''
     FOR b=1,nband DO BEGIN
        mag_po = mag_po+round_digit(par.mag_galfit_band[b-1],2,/string)
        IF b LT nband THEN mag_po=mag_po+','
     ENDFOR
     IF fix[2] EQ 1 THEN mag_po_fit = strtrim((setup.cheb[2]+1)<maxdeg,2) ELSE mag_po_fit = '0'
     printf, 1, ' 3) '+mag_po+'    '+mag_po_fit+'   '+band_po+'    # total magnitude'
     
     re_po=''
     re_po_fit=''
     FOR b=1,nband DO BEGIN
        re_po = re_po+round_digit(par.re_galfit_band[b-1],2,/string)
        IF b LT nband THEN re_po=re_po+','
     ENDFOR
     IF fix[3] EQ 1 THEN re_po_fit = strtrim((setup.cheb[3]+1)<maxdeg,2) ELSE re_po_fit = '0'
     printf, 1, ' 4) '+re_po+'    '+re_po_fit+'   '+band_po+'       #     R_e              [Pixels]'
     
     n_po=''
     n_po_fit=''
     FOR b=1,nband DO BEGIN
        n_po = n_po+round_digit(par.n_galfit_band[b-1],2,/string)
        IF b LT nband THEN n_po=n_po+','
     ENDFOR
     IF fix[4] EQ 1 THEN n_po_fit = strtrim((setup.cheb[4]+1)<maxdeg,2) ELSE n_po_fit = '0'
     printf, 1, ' 5) '+n_po+'    '+n_po_fit+'   '+band_po+'       # Sersic exponent (deVauc=4, expdisk=1)'
     
     q_po=''
     q_po_fit=''
     FOR b=1,nband DO BEGIN
        q_po = q_po+round_digit(par.q_galfit_band[b-1],4,/string)
        IF b LT nband THEN q_po=q_po+','
     ENDFOR
     IF fix[5] EQ 1 THEN q_po_fit = strtrim((setup.cheb[5]+1)<maxdeg,2) ELSE q_po_fit = '0'
     IF setup.version EQ 0. THEN str = ' 8) ' ELSE str = ' 9) '
     printf, 1, str+q_po+'    '+q_po_fit+'   '+band_po+'       # axis ratio (b/a)'
     
     
     pa_po=''
     pa_po_fit=''
     FOR b=1,nband DO BEGIN
        pa_po = pa_po+round_digit(par.pa_galfit_band[b-1],2,/string)
        IF b LT nband THEN pa_po=pa_po+','
     ENDFOR
     IF fix[6] EQ 1 THEN pa_po_fit = strtrim((setup.cheb[6]+1)<maxdeg,2) ELSE pa_po_fit = '0'
     IF setup_version EQ 0. THEN str = '9) ' ELSE str = '10) '
     printf, 1, str+pa_po+'    '+pa_po_fit+'   '+band_po+'       # position angle (PA) [Degrees: Up=0, Left=90]'
     
     printf, 1, ' Z) 0                  # output image (see above)'
     printf, 1, ''
     close, 1
     
     dum = where(fix EQ '1', ct)
     IF ct GT 0 THEN BEGIN
;write constraint file
        openu, 1, constr_file, /append
        printf, 1, ctr, ' n '+lo+' to '+hi
        printf, 1, ctr, ' re 0.3 to '+strtrim(conmaxre, 2)
        printf, 1, ctr, ' q 0.01  to 1.'
        printf, 1, ctr, ' mag '+strtrim(conminm, 2)+' '+strtrim(conmaxm, 2)
        printf, 1, ctr, ' mag 0 to 40'
;        printf, 1, ctr, ' pa -360 to 360'
        printf, 1, ctr, ' x '+strtrim(-xmax/5.)+' '+strtrim(xmax/5.)
        printf, 1, ctr, ' y '+strtrim(-ymax/5.)+' '+strtrim(ymax/5.)
        close, 1
     ENDIF
     ctr += 1
  ENDFOR
finish:
  
END

PRO read_setup, setup_file, setup
;read in the main setup file
;example:
  if file_test(strtrim(setup_file,2)) eq 0 then begin
     print, 'input file does not exist'
     stop
  ENDIF
  
  ON_IOERROR, bad_input
  
  len_num = 4                   ;length of the numbering scheme, e.g. 4 for 'A00)'
  
  setup = create_struct('files', '', $
                        'outdir', '', $
                        'dosex', 0, $
                        'sexexe', '', $
                        'sexout', '', $
                        'cold', '', $
                        'coldcat', '', $
                        'coldseg', '', $
                        'hot', '', $
                        'hotcat', '', $
                        'hotseg', '', $
                        'enlarge', -1., $
                        'outcat', '', $
                        'outseg', '', $
                        'outparam', '', $
                        'check', '', $
                        'chktype', '', $
                        'sex_rms', 0, $
                        'exclude', '', $
                        'exclude_rad', -1., $
                        'outonly', 0, $
                        'bad', '', $
                        'sexcomb', '', $
                        'dostamps', 0, $
                        'stampfile', '', $
                        'stamp_pre', strarr(1), $
                        'stampsize', -1., $
                        'dosky', 0, $
                        'skymap', '', $
                        'outsky', '', $
                        'skyscl', -1., $
                        'neiscl', -1., $
                        'skyoff', -1., $
                        'dstep', -1, $
                        'wstep', -1, $
                        'gap', -1, $
                        'cut', 0.0, $
                        'nobj_max', 0, $
                        'power', 0.0, $
                        'nslope', 0, $
                        'stel_slope', 0.0, $
                        'stel_zp', 0.0, $
                        'maglim_gal', -1, $
                        'maglim_star', -1., $
                        'nneighb', 0, $
                        'max_proc', 0, $
                        'min_dist', 0.0, $
                        'min_dist_block', -1., $
                        'srclist', '', $
                        'srclistrad', 0.0, $
                        'galexe', '', $ 
                        'gal_output', '',$
                        'gal_kill_time', 0.,$
                        'batch', '', $
                        'obj', '', $
                        'galfit_out', '', $
                        'psf', '', $
                        'mask', '', $
                        'constr', '', $
                        'convbox', 0, $
                        'zp', 0.0, $
                        'platescl', 0.0, $
                        'expt', 0.0, $
                        'conmaxre', 0.0, $
                        'conminm', 0.0, $
                        'conmaxm', 0.0, $
                        'conminn', 0.2, $
                        'conmaxn', 8.0, $
                        'nice', 0, $
                        'version', 0.,$
                        'cheb', intarr(7)-1, $
                        'galfit_out_path',' ', $
                        'do_restrict', 0, $
                        'restrict_frac_primary', 20., $
                        'mindeg', 0, $
                        'dobd', 0, $
                        'cheb_b', intarr(7)-1, $
                        'cheb_d', intarr(7)-1, $
                        'bd_label',' ', $
                        'gal_output_bd', '',$
                        'bd_hpc',0, $
                        'bd_hpc_path',' ', $
                        'bd_srclist', '', $
                        'bd_srclistrad', 0.0, $
                        'bd_maglim', 99., $
                        'bd_psf_corr', strarr(2), $
                        'docombine', 0, $
                        'docombinebd', 0, $
                        'cat', '', $
                        'galfitoutput', 0)
  
;   setup.enlarge = -1
;   setup.exclude_rad = -1
;   setup.stampsize = -1
;   setup.skyscl = -1
;   setup.neiscl = -1
;   setup.skyoff = -1
;   setup.dstep = -1
;   setup.wstep = -1
;   setup.gap = -1
  setup.stel_slope = 1e20
  setup.stel_zp = 1e20
;   setup.maglim_gal = -1
;   setup.maglim_star = -1
;   setup.min_dist_block = -1
  setup.srclist = ''
  setup.srclistrad = -1
;   for n=0,n_elements(setup.cheb)-1 do setup.cheb[n] = -1
  setup.galfit_out_path = ''
  
; check format for backwards compatibility
  block_bd = 0
  line = ''
  openr, 1, setup_file
  WHILE NOT eof(1) DO BEGIN
     readf, 1, line       
;get rid of leading and trailing blanks
     line = strtrim(line, 2)
;comment or empty line encountered?
     IF strmid(line, 0, 1) EQ '#' OR strlen(line) EQ 0 THEN CONTINUE       
;comment at end of line?
     pos = strpos(line, '#')
     IF pos EQ -1 THEN pos = strlen(line)
     content = strtrim(strmid(line, len_num, pos-len_num), 2)
     IF strupcase(strmid(line, 0, len_num)) eq 'G00)' then block_bd = 1
  ENDWHILE
  close, 1
  
  line = ''
  openr, 1, setup_file
  WHILE NOT eof(1) DO BEGIN
     readf, 1, line
     
;get rid of leading and trailing blanks
     line = strtrim(line, 2)
;    print, linestop
     
     
;comment or empty line encountered?
     IF strmid(line, 0, 1) EQ '#' OR strlen(line) EQ 0 THEN CONTINUE
     
;comment at end of line?
     pos = strpos(line, '#')
     IF pos EQ -1 THEN pos = strlen(line)
     
     content = strtrim(strmid(line, len_num, pos-len_num), 2)
     
     CASE strupcase(strmid(line, 0, len_num)) OF
;CHANGE=====have to trigger bad input / proper filenames
        
        'A00)': setup.files = content
        'A01)': setup.outdir = set_trailing_slash(content)
        
        'B00)': setup.dosex = (content EQ 'execute') ? 1 : 0
        'B01)': setup.sexexe = content
        'B02)': setup.sexout = content
        'B03)': setup.cold = content
        'B04)': setup.coldcat = content
        'B05)': setup.coldseg = content
        'B06)': setup.hot = content
        'B07)': setup.hotcat = content
        'B08)': setup.hotseg = content
        'B09)': setup.enlarge = float(content)
        'B10)': setup.outcat = content
        'B11)': setup.outseg = content
        'B12)': setup.outparam = content
        'B13)': IF content EQ 'none' OR content EQ '' THEN setup.check = '' $
        ELSE setup.check = content
        'B14)': setup.chktype = content
        'B15)': setup.sex_rms = (content EQ 'rms') ? 1 : 0
        'B16)': setup.exclude = content
        'B17)': setup.exclude_rad = float(content)
        'B18)': setup.outonly = (content EQ 'outonly') ? 1 : 0
        'B19)': setup.bad = content
        'B20)': setup.sexcomb = content
        
        'C00)': setup.dostamps = (content EQ 'execute') ? 1 : 0
        'C01)': setup.stampfile = content
        'C02)': setup.stamp_pre = content
        'C03)': setup.stampsize = float(content)
        
        'D00)': setup.dosky = (content EQ 'execute') ? 1 : 0
        'D01)': setup.skymap = content
        'D02)': setup.outsky = content
        'D03)': setup.skyscl = float(content)
        'D04)': setup.neiscl = float(content)
        'D05)': setup.skyoff = float(content)
        'D06)': setup.dstep = fix(content)
        'D07)': setup.wstep = fix(content)
        'D08)': setup.gap = fix(content)
        'D09)': setup.cut = float(content)
        'D10)': setup.nobj_max = fix(content)
        'D11)': setup.power = float(content)
        'D12)': setup.nslope = fix(content)
        'D13)': setup.stel_slope = float(content)
        'D14)': setup.stel_zp = float(content)
        'D15)': setup.maglim_gal = float(content)
        'D16)': setup.maglim_star = float(content)
        'D17)': setup.nneighb = fix(content)
        'D18)': setup.max_proc = fix(content)
        'D19)': setup.min_dist = float(content)
        'D20)': setup.min_dist_block = float(content)
        'D21)': IF content EQ 'none' OR content EQ '' THEN setup.srclist = '' $
        ELSE setup.srclist = content
        'D22)': setup.srclistrad = float(content)
        
        'E00)': setup.galexe = content
        'E01)': setup.batch = content
        'E02)': setup.obj = content
        'E03)': setup.galfit_out = content
        'E04)': setup.psf = content
        'E05)': setup.mask = content
        'E06)': setup.constr = content
        'E07)': setup.convbox = fix(content)
        'E08)': setup.zp = float(content)
        'E09)': setup.platescl = float(content)
        'E10)': setup.expt = float(content)
        'E11)': setup.conmaxre = float(content)
        'E12)': setup.conminm = float(content)
        'E13)': setup.conmaxm = float(content)
        'E14)': setup.conminn = float(content)
        'E15)': setup.conmaxn = float(content)
        'E16)': setup.nice = (content EQ 'nice') ? 1 : 0
        'E17)': BEGIN
           setup.version = 1.
           IF (pos = stregex(content, '[0123456789]')) GT 0 THEN $
              content = strmid(content, pos, strlen(content)-pos)
           pos = strpos(content, '.')
           content = strrep(content, '.', ' ')
           strput, content, '.', pos
           content = strcompress(content, /remove_all)
           if float(content) lt 2.1 then setup.version = 0.
           if (float(content) GE 2.1 and float(content) lt 4.) then setup.version = 1.
           if float(content) GE 4.0 then setup.version = float(content)
        END
        'E18)': setup.gal_output = content
        'E19)': setup.gal_kill_time = content
        'E20)': BEGIN
           for n=0,5 do begin
              pos=strpos(content, ',')
              setup.cheb[n]=strmid(content,0,pos)
              content=strmid(content,pos+1)
           ENDFOR
           setup.cheb[6]=content
        END
        'E21)': BEGIN
           if content eq '' then setup.galfit_out_path = content
           if content ne '' then setup.galfit_out_path = set_trailing_slash(content)
        END
        'E22)': setup.do_restrict = (content EQ 'restrict') ? 1 : 0
        'E23)': setup.restrict_frac_primary = content
        'E24)': setup.mindeg = content
        
        'F00)': BEGIN
           if block_bd eq 1 then setup.dobd = (content EQ 'execute') ? 1 : 0 $
           else setup.docombine = (content EQ 'execute') ? 1 : 0
        END
        'F01)': BEGIN
           if block_bd eq 1 then begin
              for n=0,5 do begin
                 pos=strpos(content, ',')
                 setup.cheb_b[n]=strmid(content,0,pos)
                 content=strmid(content,pos+1)
              ENDFOR 
              setup.cheb_b[6]=content
           ENDIF ELSE setup.docombinebd = (content EQ 'execute') ? 1 : 0
        END
        'F02)': BEGIN
           if block_bd eq 1 then begin
              for n=0,5 do begin
                 pos=strpos(content, ',')
                 setup.cheb_d[n]=strmid(content,0,pos)
                 content=strmid(content,pos+1)
              ENDFOR
              setup.cheb_d[6]=content
           ENDIF ELSE setup.cat = content
        END
        'F03)': setup.bd_label = content
        'F04)': IF content EQ 'none' OR content EQ '' THEN setup.bd_srclist = '' $
        ELSE setup.bd_srclist = content
        'F05)': setup.bd_srclistrad = float(content)
        'F06)': IF valid_num(content) EQ 1 THEN setup.bd_maglim = float(content) $
        ELSE setup.bd_maglim = 99.
        'F07)': setup.gal_output_bd = content
        'F08)': setup.bd_hpc = (content EQ 'HPC') ? 1 : 0
        'F09)': setup.bd_hpc_path = set_trailing_slash(content)
        'F10)': BEGIN
           pos=strpos(content, ',')
           setup.bd_psf_corr[0] = strtrim(strmid(content,0,pos),2)
           content=strmid(content,pos+1)
           setup.bd_psf_corr[1] = strtrim(content,2)
        END
        'G00)': setup.docombine = (content EQ 'execute') ? 1 : 0
        'G01)': setup.docombinebd = (content EQ 'execute') ? 1 : 0
        'G02)': setup.cat = content
        
     ENDCASE
  ENDWHILE
  close, 1
; Ensure backwards compatibility for D21, E18, E19. They do not exist
; in the old input file. Also, the format of the image setup file changed!!
  
;default values
  IF setup.enlarge EQ -1 THEN setup.enlarge = 1.1
  IF setup.exclude_rad EQ -1 THEN setup.exclude_rad = 2.0
  IF setup.check EQ '' THEN setup.chktype = 'none'
  IF setup.stampsize EQ -1 THEN setup.stampsize = 2.5
  IF setup.skyscl EQ -1 THEN setup.skyscl = 3.0
  IF setup.neiscl EQ -1 THEN setup.neiscl = 1.5
  IF setup.skyoff EQ -1 THEN setup.skyoff = 20
  IF setup.dstep EQ -1 THEN setup.dstep = 30
  IF setup.wstep EQ -1 THEN setup.wstep = 60
  IF setup.gap EQ -1 THEN setup.gap = 30
  IF setup.stel_slope EQ 1e20 THEN setup.stel_slope = -0.3
  IF setup.stel_zp EQ 1e20 THEN setup.stel_zp = 6.8
  IF setup.maglim_gal EQ -1 THEN setup.maglim_gal = 5
  IF setup.maglim_star EQ -1 THEN setup.maglim_star = 2
  IF setup.min_dist_block EQ -1 THEN setup.min_dist_block = setup.min_dist/3.
  IF setup.cheb[0] EQ -1 THEN for n=0,n_elements(setup.cheb)-1 do setup.cheb[n] = 0
  IF setup.mindeg lt 1 THEN setup.mindeg = 1
;IF setup.cheb_b[0] EQ -1 THEN for n=0,n_elements(setup.cheb_b)-1 do setup.cheb_b[n] = 0
;IF setup.cheb_d[0] EQ -1 THEN for n=0,n_elements(setup.cheb_d)-1 do setup.cheb_d[n] = 0
  
; check whether executables exist
  
  IF file_test(strtrim(setup.sexexe,2)) NE 1 AND setup.dosex EQ 1 THEN message, 'SExtractor executable does not exist'
  IF file_test(strtrim(setup.galexe,2)) NE 1 AND (setup.dosky EQ 1 OR setup.dobd EQ 1) THEN message, 'Galfit executable does not exist'
  
  return
  
bad_input:
  message, 'Invalid Entry in '+setup_file
END

PRO read_image_files, setup, save_folder, silent=silent,nocheck_read=nocheck_read
; reads in the image file and returns the results to main routine
; count number of columns in file
  lineone = ''
  openr, 1, setup.files
  readf, 1, lineone
  close, 1
  lineone = strtrim(lineone, 2)
  columnsf = strsplit(lineone, ' ', COUNT=ncolf)      
  
; if number of columns eq 4 then assume 1 band survey and fits files
; in the table
  IF (ncolf EQ 4) OR (ncolf EQ 5) THEN BEGIN
     IF NOT keyword_set(silent) THEN print, 'assuming one band dataset. Are all files within A00) fits images?'
     
; read the image names and data like zeropoints, mag_offsets,...
; stored in setup structure
     IF ncolf EQ 4 THEN readcol, setup.files, images, weights, outpath, outpre, $
                                 format = 'A,A,A,A', comment = '#', /silent
     IF ncolf EQ 5 THEN readcol, setup.files, images, weights, sigmaps, outpath, outpre, $
                                 format = 'A,A,A,A,A', comment = '#', /silent
     
     nband=1
     images = strtrim(images,2)
; create arrays in setup needed to store all the data 
     add_tag, setup, 'images', strarr(n_elements(images),nband+1), setup2
     setup=setup2
     add_tag, setup, 'weights', strarr(n_elements(images),nband+1), setup2
     setup=setup2
     add_tag, setup, 'sigmaps', strarr(n_elements(images),nband+1), setup2
     setup=setup2
     add_tag, setup, 'sigflags', intarr(nband+1), setup2
     setup=setup2
     add_tag, setup, 'outpath', strarr(n_elements(images),nband+1), setup2
     setup=setup2
     add_tag, setup, 'outpath_band', strarr(n_elements(images),nband+1), setup2
     setup=setup2
     add_tag, setup, 'outpre', strarr(n_elements(images),nband+1), setup2
     setup=setup2
     add_tag, setup, 'nband', nband, setup2
     setup=setup2
     
     hlpsigmaps = 'none'
     hlpsigflags = 0
     IF ncolf EQ 5 THEN hlpsigmaps = sigmaps
     IF ncolf EQ 5 THEN hlpsigflags = 1
     IF ncolf EQ 5 THEN print, 'sigma maps handed over to galfit'
; doubling the arrays to get [*,0] SExtractor images and [*,1] fitting images
     setup.images = [[images],[images]]
     setup.weights = [[weights],[weights]]
     setup.sigmaps = [[hlpsigmaps],[hlpsigmaps]]
     setup.sigflags = [[hlpsigflags],[hlpsigflags]]
     setup.outpre = [[outpre],[outpre]]
     setup.outpath = set_trailing_slash(setup.outdir)+[[outpath],[outpath]]
     setup.outpath_band = setup.outpath
     
; define additional parameters
     add_tag, setup, 'wavelength', [0,0], setup2
     setup=setup2
     add_tag, setup, 'mag_offset', [0,0], setup2
     setup=setup2
     hlppre=setup.stamp_pre
     setup=remove_tags(setup,'stamp_pre')
     add_tag, setup, 'stamp_pre', [hlppre, hlppre], setup2
     setup=setup2
     hlpzp=setup.zp
     setup=remove_tags(setup,'zp')
     add_tag, setup, 'zp', [hlpzp, hlpzp], setup2
     setup=setup2
     hlpexp=setup.expt
     setup=remove_tags(setup,'expt')
     add_tag, setup, 'expt', [hlpexp, hlpexp], setup2
     setup=setup2
     delvarx, setup2
     nband=1
; wavelength
; band
  endif 
  
; if number of columns eq 6 then assume multi band survey and
; filesnames pointing to other file lists that contain all the images.
  IF ncolf EQ 6 THEN BEGIN
     IF NOT keyword_set(silent) THEN print, 'assuming multi-wavelength dataset. Assuming first line to be for SExtractor, rest for fitting!'
     IF setup.version LT 4. THEN BEGIN
        print, 'you seem to be using mulit-wavelength data, but the GALFIT version you have specified only supports one-band data'
        print, 'Multi-band fitting needs GALFITM in order to be able to read out the fitting parameters (output has to be in a fits table)'
        stop
     ENDIF
     readcol, setup.files, band, wavelength, mag_offset, filelist, zeropoint, exptime, $
              format = 'A,F,F,A,F,F', comment = '#', /silent
     
; copy the setup files to the folder
     for f = 0, n_elements(filelist)-1 do spawn, 'cp '+filelist[f]+' '+save_folder
     
     nband=fix(n_elements(band)-1)
     
; read first (sextractor) file to get number of images...
     readcol, filelist[0], hlpimages, hlpweights, hlpoutpath, hlpoutpre, $
              format = 'A,A,A,A', comment = '#', /silent
     cnt=intarr(nband+1)

     hlpimages = strtrim(hlpimages,2)
; create arrays in setup needed to store all the data
     add_tag, setup, 'images', strarr(n_elements(hlpimages),nband+1), setup2
     setup=setup2
     add_tag, setup, 'weights', strarr(n_elements(hlpimages),nband+1), setup2
     setup=setup2
     add_tag, setup, 'sigmaps', strarr(n_elements(hlpimages),nband+1), setup2
     setup=setup2
     add_tag, setup, 'sigflags', intarr(nband+1), setup2
     setup=setup2
     add_tag, setup, 'outpath', strarr(n_elements(hlpimages),nband+1), setup2
     setup=setup2
     add_tag, setup, 'outpath_band', strarr(n_elements(hlpimages),nband+1), setup2
     setup=setup2
     add_tag, setup, 'outpre', strarr(n_elements(hlpimages),nband+1), setup2
     setup=setup2
     add_tag, setup, 'nband', nband, setup2
     setup=setup2
     
; define additional parameters
     setup=remove_tags(setup,'stamp_pre')
     add_tag, setup, 'stamp_pre', band, setup2
     setup=setup2
     add_tag, setup, 'wavelength', wavelength, setup2
     setup=setup2
     add_tag, setup, 'mag_offset', mag_offset, setup2
     setup=setup2
     setup=remove_tags(setup,'zp')
     add_tag, setup, 'zp', zeropoint, setup2
     setup=setup2
     setup=remove_tags(setup,'expt')
     add_tag, setup, 'expt', exptime, setup2
     setup=setup2
     delvarx,  setup2
     
     delvarx, hlpimages, hlpweights, hlpoutpath, hlpoutpre
     
; read sextractor bit
     readcol, filelist[0], hlpimages, hlpweights, hlpoutpath, hlpoutpre, $
              format = 'A,A,A,A', comment = '#', /silent
     
     cnt[0] = n_elements(hlpimages)
     setup.images[*,0] = hlpimages
     setup.weights[*,0] = hlpweights
     setup.sigmaps[*,0] = 'none' ; SExtractor with sigma map does not makes sense in multi-band setup
     setup.sigflags[0] = 0 ; SExtractor with sigma map does not makes sense in multi-band setup
     setup.outpre[*,0] = hlpoutpre
     setup.outpath[*,0] = set_trailing_slash(setup.outdir)+set_trailing_slash(strtrim(hlpoutpath,2))
     setup.outpath_band[*,0] = setup.outpath[*,0]+strtrim(band[0],2)
     delvarx, hlpimages, hlpweights, hlpoutpath, hlpoutpre
     
; READ OTHER BANDS (format: 2 columns only, 3 columns when using sigma
; image!!)
     FOR b=1,nband DO BEGIN
        ncolfb = 0
        lineone = ''
        openr, 1, filelist[b]
        readf, 1, lineone
        close, 1
        lineone = strtrim(lineone, 2)
        columnsf = strsplit(lineone, ' ', COUNT=ncolfb)
        IF ncolfb EQ 2 THEN readcol, filelist[b], hlpimages, hlpweights, $
                                     format = 'A,A', comment = '#', /silent
        IF ncolfb EQ 3 THEN readcol, filelist[b], hlpimages, hlpweights, hlpsigmaps, $
                                     format = 'A,A,A', comment = '#', /silent
        cnt[b]=n_elements(hlpimages)
        IF (cnt[b] NE cnt[0]) AND NOT keyword_set(silent) THEN print, 'input list '+strtrim(band[b])+' contains a wrong number of entries (tiles), copared to SExtractor list'
        IF (cnt[b] NE cnt[0]) AND NOT keyword_set(silent) THEN stop
        setup.images[*,b] = hlpimages
        setup.weights[*,b] = hlpweights
        setup.sigmaps[*,b] = 'none'
        setup.sigflags[b] = 0
        IF ncolfb EQ 3 THEN setup.sigmaps[*,b] = hlpsigmaps 
        IF ncolfb EQ 3 THEN setup.sigflags[b] = 1
        IF ncolfb EQ 3 THEN print, 'sigma maps used for band '+strtrim(band[b])
        setup.outpre[*,b] = setup.outpre[*,0]
        setup.outpath[*,b] = setup.outpath[*,0]
        setup.outpath_band[*,b] = setup.outpath[*,0]+strtrim(band[b],2)
        delvarx, hlpimages, hlpweights
        IF ncolfb NE 2 AND ncolfb NE 3 THEN message, 'Invalid Entry in '+filelist[b]
     ENDFOR 
  ENDIF
  IF ncolf NE 6 AND ncolf NE 5 AND ncolf NE 4 THEN message, 'Invalid Entry in '+setup.files
  
; now check whether all images exist
  IF NOT keyword_set(nocheck_read) THEN BEGIN
     IF setup.dosex+setup.dostamps+setup.dosky+setup.dobd GE 1 THEN BEGIN
        image_exist = file_test(strtrim(setup.images,2))
        weight_exist = file_test(strtrim(setup.weights,2))
        im_non_exist = where(image_exist EQ 0, cntimne)
        wh_non_exist = where(weight_exist EQ 0, cntwhne)
        IF cntimne NE 0 THEN BEGIN
           stopnow=1
           print, ' '
           print, 'there is at least one image missing as currently defined (typo?)'
           print, 'missing images:'
           forprint, setup.images(im_non_exist), textout=1
        ENDIF
        IF cntwhne NE 0 THEN BEGIN
           stopnow=1
           print, ' '
           print, 'there is at least one weight image missing as currently defined (typo?)'
           print, 'missing weights:'
           forprint, setup.weights(wh_non_exist), textout=1
        ENDIF
        
        IF (cntimne NE 0) OR (cntwhne NE 0) THEN stop
        IF (cntimne EQ 0) AND (cntwhne EQ 0) THEN print, 'all images and weights have been checked to exist'
     ENDIF
  ENDIF
END

FUNCTION read_sersic_results, obj, nband, setup, bd=bd, final=final
  IF file_test(strtrim(obj[0],2)) THEN BEGIN
     result = mrdfits(obj[0], 'FINAL_BAND',/silent)
     res_cheb = mrdfits(obj[0], 'FINAL_CHEB',/silent)
     fit_info = mrdfits(obj[0], 'FIT_INFO',/silent)
     band_info = mrdfits(obj[0], 'BAND_INFO',/silent)
;       hd = headfits(obj[0], exten = nband+1)
     comp=1
     REPEAT comp = comp +1 UNTIL tag_exist(result, 'COMP'+strtrim(comp,2)+'_MAG') eq 0
     IF tag_exist(band_info, 'NGOOD') THEN ngood_g = band_info.ngood ELSE ngood_g = -99
     IF tag_exist(band_info, 'NMASK') THEN nmask_g = band_info.nmask ELSE nmask_g = -99
     
; only run in final readout stage?
; run galfit to derive primary target on these latest parameters
     fit_info_primary_file = strtrim(fit_info.logfile,2)+'_primary_fit_info'
     
     IF NOT FILE_TEST(strtrim(fit_info_primary_file,2)) THEN BEGIN
        IF NOT FILE_TEST(strtrim(fit_info.logfile,2)) THEN BEGIN
           print, 'galfit restart file missing although galfit output file exists'
           print, strtrim(fit_info.logfile,2)
           stop
        ENDIF ELSE BEGIN
           derive_primary_chi2, strtrim(fit_info.logfile,2),setup.galexe
           wait,0.2
        ENDELSE
     ENDIF
; read out these values from ascii file
     readcol, fit_info_primary_file, ndof_prime, chi2_prime, chi2nu_prime, format='L,F,F',/silent
     
; delete feedback, just in case the format of one is different, avoiding crash
     delvarx, feedback
     IF NOT keyword_set(bd) THEN BEGIN
        feedback = create_struct('mag_galfit', result[0].COMP2_MAG, 'magerr_galfit',result[0].COMP2_MAG_ERR, $
                                 're_galfit', result[0].COMP2_RE, 'reerr_galfit', result[0].COMP2_RE_ERR, $
                                 'n_galfit', result[0].COMP2_N, 'nerr_galfit' ,result[0].COMP2_N_ERR, $
                                 'q_galfit', result[0].COMP2_AR, 'qerr_galfit', result[0].COMP2_AR_ERR, $
                                 'pa_galfit', result[0].COMP2_PA, 'paerr_galfit', result[0].COMP2_PA_ERR, $
                                 'x_galfit', result[0].COMP2_XC, 'xerr_galfit', result[0].COMP2_XC_ERR, $
                                 'y_galfit', result[0].COMP2_YC, 'yerr_galfit', result[0].COMP2_YC_ERR, $
                                 'psf_galfit', strtrim(band_info[0].psf,2), 'sky_galfit', result[0].COMP1_SKY, $
                                 'mag_galfit_band', result.COMP2_MAG, 'magerr_galfit_band',result.COMP2_MAG_ERR, $
                                 're_galfit_band', result.COMP2_RE, 'reerr_galfit_band', result.COMP2_RE_ERR, $
                                 'n_galfit_band', result.COMP2_N, 'nerr_galfit_band' ,result.COMP2_N_ERR, $
                                 'q_galfit_band', result.COMP2_AR, 'qerr_galfit_band', result.COMP2_AR_ERR, $
                                 'pa_galfit_band', result.COMP2_PA, 'paerr_galfit_band', result.COMP2_PA_ERR, $
                                 'x_galfit_band', result.COMP2_XC, 'xerr_galfit_band', result.COMP2_XC_ERR, $
                                 'y_galfit_band', result.COMP2_YC, 'yerr_galfit_band', result.COMP2_YC_ERR, $
                                 'sky_galfit_band', result.COMP1_SKY, $
                                 'mag_galfit_cheb', res_cheb.COMP2_MAG, 'magerr_galfit_cheb',res_cheb.COMP2_MAG_ERR, $
                                 're_galfit_cheb', res_cheb.COMP2_RE, 'reerr_galfit_cheb', res_cheb.COMP2_RE_ERR, $
                                 'n_galfit_cheb', res_cheb.COMP2_N, 'nerr_galfit_cheb' ,res_cheb.COMP2_N_ERR, $
                                 'q_galfit_cheb', res_cheb.COMP2_AR, 'qerr_galfit_cheb', res_cheb.COMP2_AR_ERR, $
                                 'pa_galfit_cheb', res_cheb.COMP2_PA, 'paerr_galfit_cheb', res_cheb.COMP2_PA_ERR, $
                                 'x_galfit_cheb', res_cheb.COMP2_XC, 'xerr_galfit_cheb', res_cheb.COMP2_XC_ERR, $
                                 'y_galfit_cheb', res_cheb.COMP2_YC, 'yerr_galfit_cheb', res_cheb.COMP2_YC_ERR, $
                                 'sky_galfit_cheb', res_cheb.COMP1_SKY, $
                                 'initfile', strtrim(fit_info.initfile,2), $
                                 'logfile', strtrim(fit_info.logfile,2), $
                                 'constrnt', strtrim(fit_info.constrnt,2), $
                                 'fitsect', strtrim(fit_info.fitsect,2), $
                                 'convbox', strtrim(fit_info.convbox,2), $
                                 'psf_galfit_band', strtrim(band_info.psf, 2), $
                                 'chisq_galfit', fit_info.chisq, $
                                 'ndof_galfit', fit_info.ndof, $
                                 'nfree_galfit', fit_info.nfree, $
                                 'ngood_galfit_band', ngood_g, $
                                 'nmask_galfit_band', nmask_g, $
                                 'nfix_galfit', fit_info.nfix, $
                                 'cputime_setup_galfit', fit_info.cputime_setup, $
                                 'cputime_fit_galfit', fit_info.cputime_fit, $
                                 'cputime_total_galfit', fit_info.cputime_total, $
                                 'chi2nu_galfit', fit_info.chi2nu, $
                                 'niter_galfit', fit_info.niter, $
                                 'galfit_version', fit_info.version, $
                                 'firstcon_galfit', fit_info.firstcon, $
                                 'lastcon_galfit', fit_info.lastcon, $
                                 'neigh_galfit', comp-3, 'flag_galfit', 2, $
                                 'X_GALFIT_DEG', total(res_cheb.comp2_xc_fit), $
                                 'Y_GALFIT_DEG', total(res_cheb.comp2_yc_fit), $
                                 'MAG_GALFIT_DEG', total(res_cheb.comp2_mag_fit), $
                                 'RE_GALFIT_DEG', total(res_cheb.comp2_re_fit), $
                                 'N_GALFIT_DEG', total(res_cheb.comp2_n_fit), $
                                 'Q_GALFIT_DEG', total(res_cheb.comp2_ar_fit), $
                                 'PA_GALFIT_DEG', total(res_cheb.comp2_pa_fit), $
                                 'NDOF_GALFIT_PRIME', ndof_prime, $
                                 'CHISQ_GALFIT_PRIME', chi2_prime, $
                                 'CHI2NU_GALFIT_PRIME', chi2nu_prime)
     ENDIF
     IF keyword_set(bd) THEN BEGIN
        feedback = create_struct('mag_galfit_d', result[0].COMP2_MAG, 'magerr_galfit_d',result[0].COMP2_MAG_ERR, $
                                 're_galfit_d', result[0].COMP2_RE, 'reerr_galfit_d', result[0].COMP2_RE_ERR, $
                                 'n_galfit_d', result[0].COMP2_N, 'nerr_galfit_d' ,result[0].COMP2_N_ERR, $
                                 'q_galfit_d', result[0].COMP2_AR, 'qerr_galfit_d', result[0].COMP2_AR_ERR, $
                                 'pa_galfit_d', result[0].COMP2_PA, 'paerr_galfit_d', result[0].COMP2_PA_ERR, $
                                 'x_galfit_d', result[0].COMP2_XC, 'xerr_galfit_d', result[0].COMP2_XC_ERR, $
                                 'y_galfit_d', result[0].COMP2_YC, 'yerr_galfit_d', result[0].COMP2_YC_ERR, $
                                 'mag_galfit_b', result[0].COMP3_MAG, 'magerr_galfit_b',result[0].COMP3_MAG_ERR, $
                                 're_galfit_b', result[0].COMP3_RE, 'reerr_galfit_b', result[0].COMP3_RE_ERR, $
                                 'n_galfit_b', result[0].COMP3_N, 'nerr_galfit_b' ,result[0].COMP3_N_ERR, $
                                 'q_galfit_b', result[0].COMP3_AR, 'qerr_galfit_b', result[0].COMP3_AR_ERR, $
                                 'pa_galfit_b', result[0].COMP3_PA, 'paerr_galfit_b', result[0].COMP3_PA_ERR, $
                                 'x_galfit_b', result[0].COMP3_XC, 'xerr_galfit_b', result[0].COMP3_XC_ERR, $
                                 'y_galfit_b', result[0].COMP3_YC, 'yerr_galfit_b', result[0].COMP3_YC_ERR, $
                                 'psf_galfit_bd', strtrim(band_info[0].psf,2), 'sky_galfit_bd', result[0].COMP1_SKY, $
                                 'mag_galfit_band_d', result.COMP2_MAG, 'magerr_galfit_band_d',result.COMP2_MAG_ERR, $
                                 're_galfit_band_d', result.COMP2_RE, 'reerr_galfit_band_d', result.COMP2_RE_ERR, $
                                 'n_galfit_band_d', result.COMP2_N, 'nerr_galfit_band_d' ,result.COMP2_N_ERR, $
                                 'q_galfit_band_d', result.COMP2_AR, 'qerr_galfit_band_d', result.COMP2_AR_ERR, $
                                 'pa_galfit_band_d', result.COMP2_PA, 'paerr_galfit_band_d', result.COMP2_PA_ERR, $
                                 'x_galfit_band_d', result.COMP2_XC, 'xerr_galfit_band_d', result.COMP2_XC_ERR, $
                                 'y_galfit_band_d', result.COMP2_YC, 'yerr_galfit_band_d', result.COMP2_YC_ERR, $
                                 'mag_galfit_band_b', result.COMP3_MAG, 'magerr_galfit_band_b',result.COMP3_MAG_ERR, $
                                 're_galfit_band_b', result.COMP3_RE, 'reerr_galfit_band_b', result.COMP3_RE_ERR, $
                                 'n_galfit_band_b', result.COMP3_N, 'nerr_galfit_band_b' ,result.COMP3_N_ERR, $
                                 'q_galfit_band_b', result.COMP3_AR, 'qerr_galfit_band_b', result.COMP3_AR_ERR, $
                                 'pa_galfit_band_b', result.COMP3_PA, 'paerr_galfit_band_b', result.COMP3_PA_ERR, $
                                 'x_galfit_band_b', result.COMP3_XC, 'xerr_galfit_band_b', result.COMP3_XC_ERR, $
                                 'y_galfit_band_b', result.COMP3_YC, 'yerr_galfit_band_b', result.COMP3_YC_ERR, $
                                 'sky_galfit_band_bd', result.COMP1_SKY, $
                                 'mag_galfit_cheb_d', res_cheb.COMP2_MAG, 'magerr_galfit_cheb_d',res_cheb.COMP2_MAG_ERR, $
                                 're_galfit_cheb_d', res_cheb.COMP2_RE, 'reerr_galfit_cheb_d', res_cheb.COMP2_RE_ERR, $
                                 'n_galfit_cheb_d', res_cheb.COMP2_N, 'nerr_galfit_cheb_d' ,res_cheb.COMP2_N_ERR, $
                                 'q_galfit_cheb_d', res_cheb.COMP2_AR, 'qerr_galfit_cheb_d', res_cheb.COMP2_AR_ERR, $
                                 'pa_galfit_cheb_d', res_cheb.COMP2_PA, 'paerr_galfit_cheb_d', res_cheb.COMP2_PA_ERR, $
                                 'x_galfit_cheb_d', res_cheb.COMP2_XC, 'xerr_galfit_cheb_d', res_cheb.COMP2_XC_ERR, $
                                 'y_galfit_cheb_d', res_cheb.COMP2_YC, 'yerr_galfit_cheb_d', res_cheb.COMP2_YC_ERR, $
                                 'mag_galfit_cheb_b', res_cheb.COMP3_MAG, 'magerr_galfit_cheb_b',res_cheb.COMP3_MAG_ERR, $
                                 're_galfit_cheb_b', res_cheb.COMP3_RE, 'reerr_galfit_cheb_b', res_cheb.COMP3_RE_ERR, $
                                 'n_galfit_cheb_b', res_cheb.COMP3_N, 'nerr_galfit_cheb_b' ,res_cheb.COMP3_N_ERR, $
                                 'q_galfit_cheb_b', res_cheb.COMP3_AR, 'qerr_galfit_cheb_b', res_cheb.COMP3_AR_ERR, $
                                 'pa_galfit_cheb_b', res_cheb.COMP3_PA, 'paerr_galfit_cheb_b', res_cheb.COMP3_PA_ERR, $
                                 'x_galfit_cheb_b', res_cheb.COMP3_XC, 'xerr_galfit_cheb_b', res_cheb.COMP3_XC_ERR, $
                                 'y_galfit_cheb_b', res_cheb.COMP3_YC, 'yerr_galfit_cheb_b', res_cheb.COMP3_YC_ERR, $
                                 'sky_galfit_cheb_bd', res_cheb.COMP1_SKY, $
                                 'initfile_bd', strtrim(fit_info.initfile,2), $
                                 'logfile_bd', strtrim(fit_info.logfile,2), $
                                 'constrnt_bd', strtrim(fit_info.constrnt,2), $
                                 'psf_galfit_band_bd', strtrim(band_info.psf, 2), $
                                 'chisq_galfit_bd', fit_info.chisq, $
                                 'ndof_galfit_bd', fit_info.ndof, $
                                 'nfree_galfit_bd', fit_info.nfree, $
                                 'nfix_galfit_bd', fit_info.nfix, $
                                 'cputime_setup_galfit_bd', fit_info.cputime_setup, $
                                 'cputime_fit_galfit_bd', fit_info.cputime_fit, $
                                 'cputime_total_galfit_bd', fit_info.cputime_total, $
                                 'chi2nu_galfit_bd', fit_info.chi2nu, $
                                 'niter_galfit_bd', fit_info.niter, $
                                 'galfit_version_bd', fit_info.version, $
                                 'firstcon_galfit_bd', fit_info.firstcon, $
                                 'lastcon_galfit_bd', fit_info.lastcon, $
                                 'neigh_galfit_bd', comp-4, 'flag_galfit_bd', 2, $
                                 'X_GALFIT_DEG_B', total(res_cheb.comp3_xc_fit), $
                                 'Y_GALFIT_DEG_B', total(res_cheb.comp3_yc_fit), $
                                 'MAG_GALFIT_DEG_B', total(res_cheb.comp3_mag_fit), $
                                 'RE_GALFIT_DEG_B', total(res_cheb.comp3_re_fit), $
                                 'N_GALFIT_DEG_B', total(res_cheb.comp3_n_fit), $
                                 'Q_GALFIT_DEG_B', total(res_cheb.comp3_ar_fit), $
                                 'PA_GALFIT_DEG_B', total(res_cheb.comp3_pa_fit), $
                                 'X_GALFIT_DEG_D', total(res_cheb.comp2_xc_fit), $
                                 'Y_GALFIT_DEG_D', total(res_cheb.comp2_yc_fit), $
                                 'MAG_GALFIT_DEG_D', total(res_cheb.comp2_mag_fit), $
                                 'RE_GALFIT_DEG_D', total(res_cheb.comp2_re_fit), $
                                 'N_GALFIT_DEG_D', total(res_cheb.comp2_n_fit), $
                                 'Q_GALFIT_DEG_D', total(res_cheb.comp2_ar_fit), $
                                 'PA_GALFIT_DEG_D', total(res_cheb.comp2_pa_fit), $
                                 'NDOF_GALFIT_BD_PRIME', ndof_prime, $
                                 'CHISQ_GALFIT_BD_PRIME', chi2_prime, $
                                 'CHI2NU_GALFIT_BD_PRIME', chi2nu_prime)
     ENDIF
; to include:
; there is more band_info which is not used yet (band, wl, datain,
; sigma, MASL, magzpt) Not sure we will need them!
     
  ENDIF ELSE BEGIN
     psf=strarr(nband)
     FOR n=0,nband-1 DO psf[n]='none'
     
     IF NOT keyword_set(bd) THEN BEGIN
        feedback = create_struct('mag_galfit', -999., 'magerr_galfit',99999., $
                                 're_galfit', -99., 'reerr_galfit', 99999., $
                                 'n_galfit', -99., 'nerr_galfit' ,99999., $
                                 'q_galfit', -99., 'qerr_galfit', 99999., $
                                 'pa_galfit', 0., 'paerr_galfit', 99999., $
                                 'x_galfit', 0., 'xerr_galfit', 99999., $
                                 'y_galfit', 0., 'yerr_galfit', 99999., $
                                 'psf_galfit', 'none', 'sky_galfit', -999., $
                                 'mag_galfit_band', fltarr(nband)-999., 'magerr_galfit_band',fltarr(nband)+99999., $
                                 're_galfit_band', fltarr(nband)-99., 'reerr_galfit_band', fltarr(nband)+99999., $
                                 'n_galfit_band', fltarr(nband)-99., 'nerr_galfit_band' ,fltarr(nband)+99999., $
                                 'q_galfit_band', fltarr(nband)-99., 'qerr_galfit_band', fltarr(nband)+99999., $
                                 'pa_galfit_band', fltarr(nband), 'paerr_galfit_band', fltarr(nband)+99999., $
                                 'x_galfit_band', fltarr(nband), 'xerr_galfit_band', fltarr(nband)+99999., $
                                 'y_galfit_band', fltarr(nband), 'yerr_galfit_band', fltarr(nband)+99999., $ 
                                 'sky_galfit_band', fltarr(nband)-999., $
                                 'mag_galfit_cheb', fltarr(nband)-999., 'magerr_galfit_cheb',fltarr(nband)+99999., $
                                 're_galfit_cheb', fltarr(nband)-99., 'reerr_galfit_cheb', fltarr(nband)+99999., $
                                 'n_galfit_cheb', fltarr(nband)-99., 'nerr_galfit_cheb' ,fltarr(nband)+99999., $
                                 'q_galfit_cheb', fltarr(nband)-99., 'qerr_galfit_cheb', fltarr(nband)+99999., $
                                 'pa_galfit_cheb', fltarr(nband), 'paerr_galfit_cheb', fltarr(nband)+99999., $
                                 'x_galfit_cheb', fltarr(nband), 'xerr_galfit_cheb', fltarr(nband)+99999., $
                                 'y_galfit_cheb', fltarr(nband), 'yerr_galfit_cheb', fltarr(nband)+99999., $
                                 'sky_galfit_cheb', fltarr(nband)-999., $
                                 'initfile', ' ', $
                                 'logfile', ' ', $
                                 'constrnt', ' ', $
                                 'psf_galfit_band', psf, $
                                 'chisq_galfit', -99., $
                                 'ndof_galfit', -99l, $
                                 'nfree_galfit', -99l, $
                                 'nfix_galfit', -99l, $
                                 'cputime_setup_galfit', -99., $
                                 'cputime_fit_galfit', -99., $
                                 'cputime_total_galfit', -99., $
                                 'chi2nu_galfit', -99., $
                                 'niter_galfit', -99, $
                                 'galfit_version', 'crash', $
                                 'firstcon_galfit', -99, $
                                 'lastcon_galfit', -99, $
                                 'neigh_galfit', -99, 'flag_galfit', 1, $
                                 'X_GALFIT_DEG', -99, $
                                 'Y_GALFIT_DEG', -99, $
                                 'MAG_GALFIT_DEG', -99, $
                                 'RE_GALFIT_DEG', -99, $
                                 'N_GALFIT_DEG', -99, $
                                 'Q_GALFIT_DEG', -99, $
                                 'PA_GALFIT_DEG', -99, $
                                 'NDOF_GALFIT_PRIME', -99l, $
                                 'CHISQ_GALFIT_PRIME', -99., $
                                 'CHI2NU_GALFIT_PRIME', -99.)
     ENDIF
     IF keyword_set(bd) THEN BEGIN
        feedback = create_struct('mag_galfit_d', -999., 'magerr_galfit_d',99999., $
                                 're_galfit_d', -99., 'reerr_galfit_d', 99999., $
                                 'n_galfit_d', -99., 'nerr_galfit_d' ,99999., $
                                 'q_galfit_d', -99., 'qerr_galfit_d', 99999., $
                                 'pa_galfit_d', 0., 'paerr_galfit_d', 99999., $
                                 'x_galfit_d', 0., 'xerr_galfit_d', 99999., $
                                 'y_galfit_d', 0., 'yerr_galfit_d', 99999., $
                                 'mag_galfit_b', -999., 'magerr_galfit_b',99999., $
                                 're_galfit_b', -99., 'reerr_galfit_b', 99999., $
                                 'n_galfit_b', -99., 'nerr_galfit_b' ,99999., $
                                 'q_galfit_b', -99., 'qerr_galfit_b', 99999., $
                                 'pa_galfit_b', 0., 'paerr_galfit_b', 99999., $
                                 'x_galfit_b', 0., 'xerr_galfit_b', 99999., $
                                 'y_galfit_b', 0., 'yerr_galfit_b', 99999., $
                                 'psf_galfit_bd', 'none', 'sky_galfit_bd', -999., $
                                 'mag_galfit_band_d', fltarr(nband)-999., 'magerr_galfit_band_d',fltarr(nband)+99999., $
                                 're_galfit_band_d', fltarr(nband)-99., 'reerr_galfit_band_d', fltarr(nband)+99999., $
                                 'n_galfit_band_d', fltarr(nband)-99., 'nerr_galfit_band_d' ,fltarr(nband)+99999., $
                                 'q_galfit_band_d', fltarr(nband)-99., 'qerr_galfit_band_d', fltarr(nband)+99999., $
                                 'pa_galfit_band_d', fltarr(nband), 'paerr_galfit_band_d', fltarr(nband)+99999., $
                                 'x_galfit_band_d', fltarr(nband), 'xerr_galfit_band_d', fltarr(nband)+99999., $
                                 'y_galfit_band_d', fltarr(nband), 'yerr_galfit_band_d', fltarr(nband)+99999., $ 
                                 'mag_galfit_band_b', fltarr(nband)-999., 'magerr_galfit_band_b',fltarr(nband)+99999., $
                                 're_galfit_band_b', fltarr(nband)-99., 'reerr_galfit_band_b', fltarr(nband)+99999., $
                                 'n_galfit_band_b', fltarr(nband)-99., 'nerr_galfit_band_b' ,fltarr(nband)+99999., $
                                 'q_galfit_band_b', fltarr(nband)-99., 'qerr_galfit_band_b', fltarr(nband)+99999., $
                                 'pa_galfit_band_b', fltarr(nband), 'paerr_galfit_band_b', fltarr(nband)+99999., $
                                 'x_galfit_band_b', fltarr(nband), 'xerr_galfit_band_b', fltarr(nband)+99999., $
                                 'y_galfit_band_b', fltarr(nband), 'yerr_galfit_band_b', fltarr(nband)+99999., $ 
                                 'sky_galfit_band_bd', fltarr(nband)-999., $
                                 'mag_galfit_cheb_d', fltarr(nband)-999., 'magerr_galfit_cheb_d',fltarr(nband)+99999., $
                                 're_galfit_cheb_d', fltarr(nband)-99., 'reerr_galfit_cheb_d', fltarr(nband)+99999., $
                                 'n_galfit_cheb_d', fltarr(nband)-99., 'nerr_galfit_cheb_d' ,fltarr(nband)+99999., $
                                 'q_galfit_cheb_d', fltarr(nband)-99., 'qerr_galfit_cheb_d', fltarr(nband)+99999., $
                                 'pa_galfit_cheb_d', fltarr(nband), 'paerr_galfit_cheb_d', fltarr(nband)+99999., $
                                 'x_galfit_cheb_d', fltarr(nband), 'xerr_galfit_cheb_d', fltarr(nband)+99999., $
                                 'y_galfit_cheb_d', fltarr(nband), 'yerr_galfit_cheb_d', fltarr(nband)+99999., $
                                 'mag_galfit_cheb_b', fltarr(nband)-999., 'magerr_galfit_cheb_b',fltarr(nband)+99999., $
                                 're_galfit_cheb_b', fltarr(nband)-99., 'reerr_galfit_cheb_b', fltarr(nband)+99999., $
                                 'n_galfit_cheb_b', fltarr(nband)-99., 'nerr_galfit_cheb_b' ,fltarr(nband)+99999., $
                                 'q_galfit_cheb_b', fltarr(nband)-99., 'qerr_galfit_cheb_b', fltarr(nband)+99999., $
                                 'pa_galfit_cheb_b', fltarr(nband), 'paerr_galfit_cheb_b', fltarr(nband)+99999., $
                                 'x_galfit_cheb_b', fltarr(nband), 'xerr_galfit_cheb_b', fltarr(nband)+99999., $
                                 'y_galfit_cheb_b', fltarr(nband), 'yerr_galfit_cheb_b', fltarr(nband)+99999., $
                                 'sky_galfit_cheb_bd', fltarr(nband)-999., $
                                 'initfile_bd', ' ', $
                                 'logfile_bd', ' ', $
                                 'constrnt_bd', ' ', $
                                 'psf_galfit_band_bd', psf, $
                                 'chisq_galfit_bd', -99., $
                                 'ndof_galfit_bd', -99l, $
                                 'nfree_galfit_bd', -99l, $
                                 'nfix_galfit_bd', -99l, $
                                 'cputime_setup_galfit_bd', -99., $
                                 'cputime_fit_galfit_bd', -99., $
                                 'cputime_total_galfit_bd', -99., $
                                 'chi2nu_galfit_bd', -99., $
                                 'niter_galfit_bd', -99, $
                                 'galfit_version_bd', 'crash', $
                                 'firstcon_galfit_bd', -99, $
                                 'lastcon_galfit_bd', -99, $
                                 'neigh_galfit_bd', -99, $
                                 'flag_galfit_bd', 1, $
                                 'X_GALFIT_DEG_B', -99, $
                                 'Y_GALFIT_DEG_B', -99, $
                                 'MAG_GALFIT_DEG_B', -99, $
                                 'RE_GALFIT_DEG_B', -99, $
                                 'N_GALFIT_DEG_B', -99, $
                                 'Q_GALFIT_DEG_B', -99, $
                                 'PA_GALFIT_DEG_B', -99, $
                                 'X_GALFIT_DEG_D', -99, $
                                 'Y_GALFIT_DEG_D', -99, $
                                 'MAG_GALFIT_DEG_D', -99, $
                                 'RE_GALFIT_DEG_D', -99, $
                                 'N_GALFIT_DEG_D', -99, $
                                 'Q_GALFIT_DEG_D', -99, $
                                 'PA_GALFIT_DEG_D', -99, $
                                 'NDOF_GALFIT_BD_PRIME', -99l, $
                                 'CHISQ_GALFIT_BD_PRIME', -99., $
                                 'CHI2NU_GALFIT_BD_PRIME', -99.)
     ENDIF
     
  ENDELSE
  return, feedback
;[mag, magerr, re, reerr, n, nerr, q, qerr, pa, paerr, $
;            x, xerr, y, yerr, sky, neigh_galfit, chisq_galfit, ndof_galfit, $
;            nfree_galfit, nfix_galfit, chi2nu_galfit]
END

FUNCTION read_sersic_results_old_galfit, obj, setup, bd=bd
  IF file_test(strtrim(obj,2)) THEN BEGIN
     hd = headfits(obj, exten = 2)
     mag0 = sxpar(hd, '2_MAG')
; cut number and error
     mag = strtrim(strmid(mag0, 0, strpos(mag0, '+/-')),2)
     magerr = strtrim(strmid(mag0, strpos(mag0, '+/-')+3, strlen(mag0)),2)
; correct '*' in the values, substitue with ' '
     mag = strtrim(strrep(mag,'*',' '),2)
     magerr = strtrim(strrep(magerr,'*',' '),2)
     mag = strtrim(strrep(mag,'[',' '),2)
     magerr = strtrim(strrep(magerr,']',' '),2)
     mag = strtrim(strrep(mag,'[',' '),2)
     magerr = strtrim(strrep(magerr,']',' '),2)
; correct 'nan' in the values
     IF mag eq 'nan' THEN mag = !VALUES.F_NAN ELSE mag = float(mag)
     IF magerr eq 'nan' THEN magerr = !VALUES.F_NAN ELSE magerr = float(magerr)

     re0 = sxpar(hd, '2_RE')
     re = strtrim(strmid(re0, 0, strpos(re0, '+/-')),2)
     reerr = strtrim(strmid(re0, strpos(re0, '+/-')+3, strlen(re0)),2)
; correct '*' in the values, substitue with ' '
     re = strtrim(strrep(re,'*',' '),2)
     reerr = strtrim(strrep(reerr,'*',' '),2)
     re = strtrim(strrep(re,'[',' '),2)
     reerr = strtrim(strrep(reerr,']',' '),2)
     re = strtrim(strrep(re,'[',' '),2)
     reerr = strtrim(strrep(reerr,']',' '),2)
; correct 'nan' in the values
     IF re eq 'nan' THEN re = !VALUES.F_NAN ELSE re = float(re)
     IF reerr eq 'nan' THEN reerr = !VALUES.F_NAN ELSE reerr = float(reerr)

     n0 = sxpar(hd, '2_N')
     n = strtrim(strmid(n0, 0, strpos(n0, '+/-')),2)
     nerr = strtrim(strmid(n0, strpos(n0, '+/-')+3, strlen(n0)),2)
; correct '*' in the values, substitue with ' '
     n = strtrim(strrep(n,'*',' '),2)
     nerr = strtrim(strrep(nerr,'*',' '),2)
     n = strtrim(strrep(n,'[',' '),2)
     nerr = strtrim(strrep(nerr,']',' '),2)
     n = strtrim(strrep(n,'[',' '),2)
     nerr = strtrim(strrep(nerr,']',' '),2)
; correct 'nan' in the values
     IF n eq 'nan' THEN n = !VALUES.F_NAN ELSE n = float(n)
     IF nerr eq 'nan' THEN nerr = !VALUES.F_NAN ELSE nerr = float(nerr)

     q0 = sxpar(hd, '2_AR')
     q = strtrim(strmid(q0, 0, strpos(q0, '+/-')),2)
     qerr = strtrim(strmid(q0, strpos(q0, '+/-')+3, strlen(q0)),2)
; correct '*' in the values, substitue with ' '
     q = strtrim(strrep(q,'*',' '),2)
     qerr = strtrim(strrep(qerr,'*',' '),2)
     q = strtrim(strrep(q,'[',' '),2)
     qerr = strtrim(strrep(qerr,']',' '),2)
     q = strtrim(strrep(q,'[',' '),2)
     qerr = strtrim(strrep(qerr,']',' '),2)
; correct 'nan' in the values
     IF q eq 'nan' THEN q = !VALUES.F_NAN ELSE q = float(q)
     IF qerr eq 'nan' THEN qerr = !VALUES.F_NAN ELSE qerr = float(qerr)

     pa0 = sxpar(hd, '2_PA')
     pa = strtrim(strmid(pa0, 0, strpos(pa0, '+/-')),2)
     paerr = strtrim(strmid(pa0, strpos(pa0, '+/-')+3, strlen(pa0)),2)
; correct '*' in the values, substitue with ' '
     pa = strtrim(strrep(pa,'*',' '),2)
     paerr = strtrim(strrep(paerr,'*',' '),2)
     pa = strtrim(strrep(pa,'[',' '),2)
     paerr = strtrim(strrep(paerr,']',' '),2)
     pa = strtrim(strrep(pa,'[',' '),2)
     paerr = strtrim(strrep(paerr,']',' '),2)
; correct 'nan' in the values
     IF pa eq 'nan' THEN pa = !VALUES.F_NAN ELSE pa = float(pa)
     IF paerr eq 'nan' THEN paerr = !VALUES.F_NAN ELSE paerr = float(paerr)

     x0 = sxpar(hd, '2_XC')
     x = strtrim(strmid(x0, 0, strpos(x0, '+/-')),2)
     xerr = strtrim(strmid(x0, strpos(x0, '+/-')+3, strlen(x0)),2)
; correct '*' in the values, substitue with ' '
     x = strtrim(strrep(x,'*',' '),2)
     xerr = strtrim(strrep(xerr,'*',' '),2)
     x = strtrim(strrep(x,'[',' '),2)
     xerr = strtrim(strrep(xerr,']',' '),2)
     x = strtrim(strrep(x,'[',' '),2)
     xerr = strtrim(strrep(xerr,']',' '),2)
; correct 'nan' in the values
     IF x eq 'nan' THEN x = !VALUES.F_NAN ELSE x = float(x)
     IF xerr eq 'nan' THEN xerr = !VALUES.F_NAN ELSE xerr = float(xerr)

     y0 = sxpar(hd, '2_YC')
     y = strtrim(strmid(y0, 0, strpos(y0, '+/-')),2)
     yerr = strtrim(strmid(y0, strpos(y0, '+/-')+3, strlen(y0)),2)
; correct '*' in the values, substitue with ' '
     y = strtrim(strrep(y,'*',' '),2)
     yerr = strtrim(strrep(yerr,'*',' '),2)
     y = strtrim(strrep(y,'[',' '),2)
     yerr = strtrim(strrep(yerr,']',' '),2)
     y = strtrim(strrep(y,'[',' '),2)
     yerr = strtrim(strrep(yerr,']',' '),2)
; correct 'nan' in the values
     IF y eq 'nan' THEN y = !VALUES.F_NAN ELSE y = float(y)
     IF yerr eq 'nan' THEN yerr = !VALUES.F_NAN ELSE yerr = float(yerr)

     s0 = sxpar(hd, '1_SKY')
     sky = strtrim(strmid(s0, 1, strpos(s0, ']')),2)
; correct '*' in the values, substitue with ' '
     sky = strtrim(strrep(sky,'*',' '),2)
     sky = strtrim(strrep(sky,'[',' '),2)
     sky = strtrim(strrep(sky,'[',' '),2)
; correct 'nan' in the values
     IF sky eq 'nan' THEN sky = !VALUES.F_NAN ELSE sky = float(sky)

     psf0 = sxpar(hd, 'PSF') 
     psf= strtrim(psf0, 2)
; find number of neighbours
     comp=0
     REPEAT comp = comp +1 UNTIL sxpar(hd, 'COMP_'+strtrim(comp,2)) eq '0'
     neigh_galfit = comp-3
     flag_galfit = 2
     chisq_galfit = float(strmid(sxpar(hd, 'CHISQ'),2)) 
     ndof_galfit = long(strmid(sxpar(hd, 'NDOF'),2))
     nfree_galfit = long(strmid(sxpar(hd, 'NFREE'),2))
     nfix_galfit = long(strmid(sxpar(hd, 'NFIX'),2))
     chi2nu_galfit = float(strmid(sxpar(hd, 'CHI2NU'),2))
  ENDIF ELSE BEGIN
     mag = -999.
     magerr = 99999.
     re = -99.
     reerr = 99999.
     n = -99.
     nerr = 99999.
     q = -99.
     qerr = 99999.
     pa = 0.
     paerr = 99999.
     x = 0.
     xerr = 99999.
     y = 0.
     yerr = 99999.
     sky = -999.
     neigh_galfit = -99
     flag_galfit = 1
     chisq_galfit = -99.
     ndof_galfit = -99l
     nfree_galfit = -99l
     nfix_galfit = -99l
     chi2nu_galfit = -99.
     psf='none'
  ENDELSE
  
  IF NOT keyword_set(bd) THEN BEGIN
     feedback = create_struct('mag_galfit', mag, 'magerr_galfit', magerr, $
                              're_galfit', re, 'reerr_galfit', reerr, $
                              'n_galfit', n, 'nerr_galfit', nerr, $
                              'q_galfit', q, 'qerr_galfit', qerr, $
                              'pa_galfit', pa, 'paerr_galfit', paerr, $
                              'x_galfit', x, 'xerr_galfit', xerr, $
                              'y_galfit', y, 'yerr_galfit', yerr, $
                              'psf_galfit', psf, 'sky_galfit', sky, $
                              'mag_galfit_band', mag, 'magerr_galfit_band', magerr, $
                              're_galfit_band', re, 'reerr_galfit_band', reerr, $
                              'n_galfit_band', n, 'nerr_galfit_band', nerr, $
                              'q_galfit_band', q, 'qerr_galfit_band', qerr, $
                              'pa_galfit_band', pa, 'paerr_galfit_band', paerr, $
                              'x_galfit_band', x, 'xerr_galfit_band', xerr, $
                              'y_galfit_band', y, 'yerr_galfit_band', yerr, $
                              'sky_galfit_band', sky, $
                              'mag_galfit_cheb', mag, 'magerr_galfit_cheb', magerr, $
                              're_galfit_cheb', re, 'reerr_galfit_cheb', reerr, $
                              'n_galfit_cheb', n, 'nerr_galfit_cheb', nerr, $
                              'q_galfit_cheb', q, 'qerr_galfit_cheb', qerr, $
                              'pa_galfit_cheb', pa, 'paerr_galfit_cheb', paerr, $
                              'x_galfit_cheb', x, 'xerr_galfit_cheb', xerr, $
                              'y_galfit_cheb', y, 'yerr_galfit_cheb', yerr, $
                              'sky_galfit_cheb', sky, $
                              'psf_galfit_band', psf, $
                              'chisq_galfit', chisq_galfit, $
                              'ndof_galfit', ndof_galfit, $
                              'nfree_galfit', nfree_galfit, $
                              'nfix_galfit', nfix_galfit, $
                              'chi2nu_galfit', chi2nu_galfit, $
                              'neigh_galfit', neigh_galfit, 'flag_galfit', flag_galfit, $
                              'X_GALFIT_DEG', -99, $
                              'Y_GALFIT_DEG', -99, $
                              'MAG_GALFIT_DEG', -99, $
                              'RE_GALFIT_DEG', -99, $
                              'N_GALFIT_DEG', -99, $
                              'Q_GALFIT_DEG', -99, $
                              'PA_GALFIT_DEG', -99)
  ENDIF
  
  IF keyword_set(bd) THEN BEGIN
     IF file_test(strtrim(obj,2)) THEN BEGIN
        hd = headfits(obj, exten = 2)
        mag0_b = sxpar(hd, '3_MAG')
        mag_b = float(strmid(mag0_b, 0, strpos(mag0_b, '+/-')))
        magerr_b = float(strmid(mag0_b, strpos(mag0_b, '+/-')+3, strlen(mag0_b)))
        re0_b = sxpar(hd, '3_RE')
        re_b = float(strmid(re0_b, 0, strpos(re0_b, '+/-')))
        reerr_b = float(strmid(re0_b, strpos(re0_b, '+/-')+3, strlen(re0_b)))
        n0_b = sxpar(hd, '3_N')
        n_b = float(strmid(n0_b, 0, strpos(n0_b, '+/-')))
        nerr_b = float(strmid(n0_b, strpos(n0_b, '+/-')+3, strlen(n0_b)))
        q0_b = sxpar(hd, '3_AR')
        q_b = float(strmid(q0_b, 0, strpos(q0_b, '+/-')))
        qerr_b = float(strmid(q0_b, strpos(q0_b, '+/-')+3, strlen(q0_b)))
        pa0_b = sxpar(hd, '3_PA')
        pa_b = float(strmid(pa0_b, 0, strpos(pa0_b, '+/-')))
        paerr_b = float(strmid(pa0_b, strpos(pa0_b, '+/-')+3, strlen(pa0_b)))
        x0_b = sxpar(hd, '3_XC')
        x_b = float(strmid(x0_b, 0, strpos(x0_b, '+/-')))
        xerr_b = float(strmid(x0_b, strpos(x0_b, '+/-')+3, strlen(x0_b)))
        y0_b = sxpar(hd, '3_YC')
        y_b = float(strmid(y0_b, 0, strpos(y0_b, '+/-')))
        yerr_b = float(strmid(y0_b, strpos(y0_b, '+/-')+3, strlen(y0_b)))
; find number of neighbors
        neigh_galfit = comp-4
     ENDIF ELSE BEGIN
        mag_b = -999.
        magerr_b = 99999.
        re_b = -99.
        reerr_b = 99999.
        n_b = -99.
        nerr_b = 99999.
        q_b = -99.
        qerr_b = 99999.
        pa_b = 0.
        paerr_b = 99999.
        x_b = 0.
        xerr_b = 99999.
        y_b = 0.
        yerr_b = 99999.
     ENDELSE
     
     feedback = create_struct('mag_galfit_d', mag, 'magerr_galfit_d', magerr, $
                              're_galfit_d', re, 'reerr_galfit_d', reerr, $
                              'n_galfit_d', n, 'nerr_galfit_d', nerr, $
                              'q_galfit_d', q, 'qerr_galfit_d', qerr, $
                              'pa_galfit_d', pa, 'paerr_galfit_d', paerr, $
                              'x_galfit_d', x, 'xerr_galfit_d', xerr, $
                              'y_galfit_d', y, 'yerr_galfit_d', yerr, $
                              'mag_galfit_b', mag_b, 'magerr_galfit_b', magerr_b, $
                              're_galfit_b', re_b, 'reerr_galfit_b', reerr_b, $
                              'n_galfit_b', n_b, 'nerr_galfit_b', nerr_b, $
                              'q_galfit_b', q_b, 'qerr_galfit_b', qerr_b, $
                              'pa_galfit_b', pa_b, 'paerr_galfit_b', paerr_b, $
                              'x_galfit_b', x_b, 'xerr_galfit_b', xerr_b, $
                              'y_galfit_b', y_b, 'yerr_galfit_b', yerr_b, $
                              'psf_galfit_bd', psf, 'sky_galfit_bd', sky, $
                              'mag_galfit_band_d', mag, 'magerr_galfit_band_d', magerr, $
                              're_galfit_band_d', re, 'reerr_galfit_band_d', reerr, $
                              'n_galfit_band_d', n, 'nerr_galfit_band_d', nerr, $
                              'q_galfit_band_d', q, 'qerr_galfit_band_d', qerr, $
                              'pa_galfit_band_d', pa, 'paerr_galfit_band_d', paerr, $
                              'x_galfit_band_d', x, 'xerr_galfit_band_d', xerr, $
                              'y_galfit_band_d', y, 'yerr_galfit_band_d', yerr, $
                              'mag_galfit_band_b', mag_b, 'magerr_galfit_band_b', magerr_b, $
                              're_galfit_band_b', re_b, 'reerr_galfit_band_b', reerr_b, $
                              'n_galfit_band_b', n_b, 'nerr_galfit_band_b', nerr_b, $
                              'q_galfit_band_b', q_b, 'qerr_galfit_band_b', qerr_b, $
                              'pa_galfit_band_b', pa_b, 'paerr_galfit_band_b', paerr_b, $
                              'x_galfit_band_b', x_b, 'xerr_galfit_band_b', xerr_b, $
                              'y_galfit_band_b', y_b, 'yerr_galfit_band_b', yerr_b, $
                              'sky_galfit_band_bd', sky, $
                              'mag_galfit_cheb_d', mag, 'magerr_galfit_cheb_d', magerr, $
                              're_galfit_cheb_d', re, 'reerr_galfit_cheb_d', reerr, $
                              'n_galfit_cheb_d', n, 'nerr_galfit_cheb_d', nerr, $
                              'q_galfit_cheb_d', q, 'qerr_galfit_cheb_d', qerr, $
                              'pa_galfit_cheb_d', pa, 'paerr_galfit_cheb_d', paerr, $
                              'x_galfit_cheb_d', x, 'xerr_galfit_cheb_d', xerr, $
                              'y_galfit_cheb_d', y, 'yerr_galfit_cheb_d', yerr, $
                              'mag_galfit_cheb_b', mag_b, 'magerr_galfit_cheb_b', magerr_b, $
                              're_galfit_cheb_b', re_b, 'reerr_galfit_cheb_b', reerr_b, $
                              'n_galfit_cheb_b', n_b, 'nerr_galfit_cheb_b', nerr_b, $
                              'q_galfit_cheb_b', q_b, 'qerr_galfit_cheb_b', qerr_b, $
                              'pa_galfit_cheb_b', pa_b, 'paerr_galfit_cheb_b', paerr_b, $
                              'x_galfit_cheb_b', x_b, 'xerr_galfit_cheb_b', xerr_b, $
                              'y_galfit_cheb_b', y_b, 'yerr_galfit_cheb_b', yerr_b, $
                              'sky_galfit_cheb_bd', sky, $
                              'psf_galfit_band_bd', psf, $
                              'chisq_galfit_bd', chisq_galfit, $
                              'ndof_galfit_bd', ndof_galfit, $
                              'nfree_galfit_bd', nfree_galfit, $
                              'nfix_galfit_bd', nfix_galfit, $
                              'chi2nu_galfit_bd', chi2nu_galfit, $
                              'neigh_galfit_bd', neigh_galfit, 'flag_galfit_bd', flag_galfit, $
                              'X_GALFIT_DEG_B', -99, $
                              'Y_GALFIT_DEG_B', -99, $
                              'MAG_GALFIT_DEG_B', -99, $
                              'RE_GALFIT_DEG_B', -99, $
                              'N_GALFIT_DEG_B', -99, $
                              'Q_GALFIT_DEG_B', -99, $
                              'PA_GALFIT_DEG_B', -99, $
                              'X_GALFIT_DEG_D', -99, $
                              'Y_GALFIT_DEG_D', -99, $
                              'MAG_GALFIT_DEG_D', -99, $
                              'RE_GALFIT_DEG_D', -99, $
                              'N_GALFIT_DEG_D', -99, $
                              'Q_GALFIT_DEG_D', -99, $
                              'PA_GALFIT_DEG_D', -99)
  ENDIF       
  return, feedback
END

PRO update_table, table, i, out_file, obj_file, sky_file, nband, setup, final = final, bd=bd
                                ; HAS TO BE CHANGED FOR POSSIBILITY FOR B/D DECOMPOSITION
  forward_function read_sersic_results
  forward_function read_sersic_results_old_galfit
; this routine takes care of objects with non-existent output files (e.g. crashed)
; this routine takes care of deriving primary Chi^2 values
  IF setup.version GE 4. THEN res = read_sersic_results(out_file+'.fits', nband, setup, bd=bd, final=final)
  IF setup.version LT 4. THEN res = read_sersic_results_old_galfit(out_file+'.fits', setup, bd=bd)      
  name_table = tag_names(table)
  name_res = tag_names(res)

  FOR j=0,n_elements(name_res)-1 DO BEGIN
     tagidx=where(name_table EQ name_res[j], ct)
     IF ct GT 0 THEN BEGIN
        type=size(res.(j))
; if keyword is INT 
        ntype = n_elements(type)
        IF type[ntype-2] EQ 2 OR type[ntype-2] EQ 3 THEN BEGIN
           wh=where(finite(res.(j)) NE 1, ct)
           IF ct GT 0 THEN res[wh].(j)=-99999
        ENDIF
; if keyword is FLOAT
        IF type[ntype-2] EQ 4 THEN BEGIN
           wh=where(finite(res.(j)) NE 1, ct)
           IF ct GT 0 THEN res[wh].(j)=-99999.
        ENDIF
; if keyword is DOUBLE
        IF type[ntype-2] EQ 5 THEN BEGIN
           wh=where(finite(res.(j)) NE 1, ct)
           IF ct GT 0 THEN res[wh].(j)=double(-99999.)
        ENDIF
; if keyword is STRING
        IF type[ntype-2] EQ 7 THEN BEGIN
           wh=where(res.(j) EQ ' ', ct)
           IF ct GT 0 THEN res[wh].(j)='null'
        ENDIF
;          if ct gt 0 then print, 'changed'
        table[i].(tagidx) = strtrim(res.(j),2)
     ENDIF
  ENDFOR
  
; set galfit_flag
; always set, e.g. as given by read_sersic_result
  IF NOT keyword_set(bd) THEN table[i].flag_galfit = res.flag_galfit
  IF keyword_set(bd) THEN table[i].flag_galfit_bd = res.flag_galfit_bd    
  
; deal with 'crashed' objects
  IF NOT file_test(strtrim(out_file+'.fits',2)) THEN BEGIN
; object has deliberately not been started as the maximum degree of freedom is too small
     IF file_test(strtrim(obj_file+'_not_started',2)) THEN BEGIN
        IF NOT keyword_set(bd) THEN table[i].flag_galfit = -1
        IF keyword_set(bd) THEN table[i].flag_galfit_bd = -1
     ENDIF
     
     IF NOT file_test(strtrim(obj_file+'_not_started',2)) THEN BEGIN
        IF NOT file_test(strtrim(obj_file,2)) AND NOT file_test(strtrim(out_file+'.sav',2)) THEN BEGIN
; object has not YET been started.
           IF NOT keyword_set(bd) THEN table[i].flag_galfit = 0
           IF keyword_set(bd) THEN table[i].flag_galfit_bd = 0
        ENDIF ELSE BEGIN
; object has been started and has actually crashed (or is currently doing sky determination)
           IF NOT keyword_set(bd) THEN BEGIN
              table[i].flag_galfit = 1
              table[i].initfile = strtrim(obj_file,2)
           ENDIF
           IF keyword_set(bd) THEN BEGIN
              table[i].flag_galfit_bd = 1
              table[i].initfile_bd = strtrim(obj_file,2)
           ENDIF
        ENDELSE
     ENDIF
  ENDIF ELSE BEGIN
     
; set flag if outfile actually exists (e.g. successful fit)
; IN B/D THIS BIT WILL BE DONE IN EACH READIN, BUT RESULT IS EQUIVALENT to SS
     IF keyword_set(final) THEN BEGIN
        table[i].org_image = strtrim(table[i].tile,2)
        table[i].org_image_band = strtrim(table[i].tile,2)
     ENDIF
     IF NOT keyword_set(final) THEN table[i].org_image = strtrim(table[i].frame[0],2)
     
     IF NOT keyword_set(bd) THEN BEGIN
        table[i].flag_galfit = 2
        table[i].file_galfit = strtrim(out_file,2)+'.fits'
     ENDIF
     IF keyword_set(bd) THEN BEGIN
        table[i].flag_galfit_bd = 2
        table[i].file_galfit_bd = strtrim(out_file,2)+'.fits'
     ENDIF
  ENDELSE
  
; read all sky values and flags
  IF NOT keyword_set(bd) THEN BEGIN
     FOR b=1,nband DO BEGIN
; check whether sky file exists first
; overwrite -999. from above with true value
        IF file_test(strtrim(sky_file[b],2)) EQ 1 THEN BEGIN
           openr, 99, sky_file[b]
           readf, 99, sky, dsky, skyrad, sky_magobj, skyflag
           close, 99
           IF sky GT 1.0 THEN table[i].sky_gala_band[b-1] = round_digit(sky,3,/L64) $
              ELSE table[i].sky_gala_band[b-1] = sigfig(sky,4)
           IF sky GT 1.0 THEN table[i].sky_sig_band[b-1] = round_digit(dsky,6,/L64) $
              ELSE table[i].sky_sig_band[b-1] = sigfig(dsky,6)

;           table[i].sky_gala_band[b-1] = round_digit(sky,3,/L64)
;           table[i].sky_sig_band[b-1] = round_digit(dsky,5,/L64)
           table[i].sky_rad_band[b-1] = round_digit(skyrad,5)
           table[i].sky_flag_band[b-1] = round_digit(skyflag,5)
           IF b EQ 1 THEN $
              table[i].sky_galfit = round_digit(sky,3,/L64)
        ENDIF
     ENDFOR
  ENDIF
END

PRO update_log, logfile, message
  openw, lun, logfile, /get_lun, /append
  printf, lun, message
  free_lun, lun
END

PRO start_log, logfile, message
  openw, lun, logfile, /get_lun
  printf, lun, message
  free_lun, lun
END

PRO galapagos, setup_file, gala_pro, logfile=logfile, plot=plot, bridgejournal=bridgejournal, galfitoutput=galfitoutput, jump1=jump1, jump2=jump2, sex_skip=sex_skip, nocheck=nocheck
  galapagos_version = 'GALAPAGOS-v2.4.4'
  galapagos_date = '(Aug 22th, 2022)'
  print, 'THIS IS '+galapagos_version+' '+galapagos_date+' '
  print, ''
  start=systime(0)
  print, 'start time: '+start
  IF n_params() LE 1 THEN gala_pro = 'galapagos'
;   gala_pro = '/home/boris/IDL/gala/galapagos.pro'
;   logfile = '/data/gama/galapagos_multi_wl_galapagos.log'
; .run galapagos.pro
;  galapagos,'~/megamorph_dev/astro-megamorph/scripts_boris/megamorph/gala_setup/multi-wl/gala.gama_mwl_1'
;  galapagos,'~/IDL/megamorph/gala_setup/gala.gama_set3.2_test'
;==============================================================================
;main input: location of the setup file
  IF n_params() LT 1 THEN BEGIN
     print, 'start galapagos by typing: '
     print, 'galapagos, "\path\to\setup_file"'
     print, 'allowed keyword:'
     print, '   logfile = "filename"    ; creates a log file for the galapagos run'
     print, '   /plot                   ; creates a plot that shows which objects are being worked on '
     print, '                           ; and which objects are being blocked'
     print, '                           ; (might not work on all systems and setups)'
     print, '   /bridgejournal          ; switches on creation of journal files for all bridges, e.g. to examine the Galfit/GalfitM outputs directly'
     print, '                           ; can be found in a subfolder "journal" in the output folder'
     print, '   /sex_skip               ; avoids running SExtractor on the individual tiles again, e.g. when SExtractor has already been run. '
     print, '                           ; Only the catalogue combination will be redone'
     print, '   /jump1                  ; jumps some parts at the beginning of the code that do not need to be repeated (for speedup)'
     print, '                           ; more precisely the creation of some catalogue files'
     print, '                           ; typically can be set when Galapagos has crashed or has been stopped somewhere during the fits'
     print, '                           ; AND NO NEW TILES HAVE BEEN ADDED (e.g. object catalogue did not change!)'
     print, '   /jump2                  ; before B/D fits jumps the read-in of single-profile fits (for speedup)'
     print, '                           ; typically can be set when Galapagos has crashed or has been stopped somewhere during the B/D fits'
     print, '                           ; AND NO NEW SINGLE-SERSIC FITS SHOULD EXIST!'
     print, ''
     setup_file = ''
     read, prompt = 'Location of setup file: ', setup_file
  ENDIF
;==============================================================================
;read in the setup file
  read_setup, setup_file, setup
  print, 'using batch file '+setup.batch
; add additional values
  IF keyword_set(galfitoutput) THEN setup.galfitoutput = 1
  
;copy setup file to output folder for future reference
  date=systime(0)
  date=strsplit(date,' ',/extract)  
  save_folder = setup.outdir+'setups/setup_'+date[4]+'_'+date[1]+'_'+date[2]
  IF NOT file_test(strtrim(save_folder,2)) THEN spawn, 'mkdir -p '+save_folder
  spawn, 'cp '+setup_file+' '+save_folder
  spawn, 'cp '+setup.files+' '+save_folder
  spawn, 'cp '+setup.sexout+' '+save_folder
  IF file_test(strtrim(setup.cold,2)) THEN spawn, 'cp '+setup.cold+' '+save_folder
  IF file_test(strtrim(setup.hot,2)) THEN spawn, 'cp '+setup.hot+' '+save_folder
  IF file_test(strtrim(setup.exclude,2)) THEN spawn, 'cp '+setup.exclude+' '+save_folder
  IF file_test(strtrim(setup.bad,2)) THEN spawn, 'cp '+setup.bad+' '+save_folder
  IF file_test(strtrim(setup.srclist,2)) THEN spawn, 'cp '+setup.srclist+' '+save_folder
  IF file_test(strtrim(setup.bd_srclist,2)) THEN spawn, 'cp '+setup.bd_srclist+' '+save_folder
  journal_folder = setup.outdir+'journals/bridgejournal_'+date[4]+'_'+date[1]+'_'+date[2]
  IF keyword_set(bridgejournal) THEN spawn, 'mkdir -p '+journal_folder
  
  IF keyword_set(logfile) THEN $
     start_log, logfile, systime()+': Reading setup file... done!'
  IF setup.dobd EQ 1 THEN print, 'You are trying to do B/D decomposition? Are you sure you know what you are doing?!' 
  
;==============================================================================   
;read input files into arrays
  read_image_files, setup, save_folder,nocheck_read=nocheck
  images = setup.images
  weights = setup.weights
  outpath = setup.outpath
  outpath_band = setup.outpath_band
  outpre = setup.outpre
  nband = setup.nband
  
; correct too high degrees of freedom as galfitm would crash!
  IF setup.nband LT setup.cheb[2]+1 THEN BEGIN
     print, '**Your degree of freedom (set by E20 +1) is higher than the number of bands you are using.' 
     print, 'This will be corrected in the code to be full freedom so GALFITM does not crash'
     print, 'This is only a warning for you to check whether these settings are indeed what you meant to do'
  ENDIF
  IF setup.nband LT setup.cheb_b[2]+1 THEN BEGIN
     print, '**Your degree of freedom (set by F01 +1) is higher than the number of bands you are using.' 
     print, 'This will be corrected in the code to be full freedom so GALFITM does not crash'
     print, 'This is only a warning for you to check whether these settings are indeed what you meant to do'
  ENDIF
  IF setup.nband LT setup.cheb_d[2]+1 THEN BEGIN
     print, '**Your degree of freedom (set by F02 +1) is higher than the number of bands you are using.' 
     print, 'This will be corrected in the code to be full freedom so GALFITM does not crash'
     print, 'This is only a warning for you to check whether these settings are indeed what you meant to do'
  ENDIF
  
; now that number of bands is known, correct number of additional cheb
; components to max nband -1
  setup.cheb = setup.cheb < (setup.nband-1)
  setup.cheb_b = setup.cheb_b < (setup.nband-1)
  setup.cheb_d = setup.cheb_d < (setup.nband-1)
  
;; NAMING CONVENTIONS AND EXAMPLES
; MULTIBAND
; outpath:              /data/gama/galapagos_multi_wl/tile10_5/
; outpath_galfit:       /data/gama/galapagos_multi_wl/tile10_5/galfit
; outpath_band[0]:      /data/gama/galapagos_multi_wl/tile10_5/sex/
; outpath_pre[0]:       /data/gama/galapagos_multi_wl/tile10_5/sex/t10_5.
; outpath_file[0]:      /data/gama/galapagos_multi_wl/tile10_5/sex/t10_5.sex.
; outpath_file_no_band: /data/gama/galapagos_multi_wl/tile10_5/t10_5.
; 1 BAND
; outpath:              /data/gama/galapagos_multi_wl/tile10_5/
; outpath_galfit:       /data/gama/galapagos_multi_wl/tile10_5/
; outpath_band:         /data/gama/galapagos_multi_wl/tile10_5/
; outpath_pre:          /data/gama/galapagos_multi_wl/tile10_5/t10_5.
; outpath_file:         /data/gama/galapagos_multi_wl/tile10_5/t10_5.v.
  outpath = set_trailing_slash(outpath)
  outpath_galfit = strtrim(outpath[*,0]+setup.galfit_out_path,2)
  outpath_band = set_trailing_slash(outpath_band)
  outpath_pre = outpath_band+outpre
  outpath_file = outpath
  FOR q=0,nband DO outpath_file[*,q]=outpath_pre[*,q]+strtrim(setup.stamp_pre[q],2)+'.'
  outpath_file_no_band = outpath
  FOR q=0,nband DO outpath_file_no_band[*,q]=outpath[*,q]+outpre[*,q]
;total number of frames
  
  nframes = n_elements(images[*,0])
;calculate image centres, but only if needed in further program

;  IF setup.dosex+setup.dostamps+setup.dosky+setup.dobd GE 1 THEN BEGIN
  IF setup.dosex+setup.dostamps+setup.dosky GE 1 THEN BEGIN
     dec_cnt = (ra_cnt = dblarr(nframes))
     FOR i=0ul, nframes-1 DO BEGIN
        head = headfits(images[i,0])
        xcnt = sxpar(head, 'NAXIS1')*0.5
        ycnt = sxpar(head, 'NAXIS2')*0.5
        xyad, head, xcnt, ycnt, a, d
        ra_cnt[i] = a
        dec_cnt[i] = d
     ENDFOR
     
;create an array, that contains the filenames of neighbouring frames
;for each tile
     neighbours = strarr(setup.nneighb >1, nframes)
     FOR i=0ul, nframes-1 DO BEGIN
        gcirc, 1, ra_cnt[i]/15., dec_cnt[i], ra_cnt/15., dec_cnt, dist
        ord = sort(dist)
        n = setup.nneighb < (n_elements(ord)-1)
        IF n GT 0 THEN neighbours[0:n-1, i] = images[ord[1:n]]
     ENDFOR
  ENDIF
;==============================================================================
;check if output path exists, if not create them
  IF NOT file_test(strtrim(setup.outdir,2)) THEN $
     spawn, 'mkdir -p '+setup.outdir
  FOR i=0ul, n_elements(outpath_band)-1 DO IF NOT file_test(strtrim(outpath_band[i],2)) THEN $
     spawn, 'mkdir -p '+strtrim(outpath_band[i],2)
  FOR i=0ul, n_elements(outpath_galfit)-1 DO IF NOT file_test(strtrim(outpath_galfit[i],2)) THEN $
     spawn, 'mkdir -p  '+outpath_galfit[i]
  IF keyword_set(logfile) THEN $
     update_log, logfile, systime()+': Initialisation... done!'
;===============================================================================
  
; define the columns that have to be added to the SExtractor catalogue
; to get output catalogue
; This already defines ALL parameters, even the B/D ones if later
; needed. Nothing is done to those until starting the BD-block!
  define_addcol, addcol, nband, bd_fit = setup.dobd, read_bd = setup.docombinebd

;==============================================================================
; check if psf in setup file is an image or a list and read into structure used
  print, 'reading PSFs'
  readin_psf_file, setup.psf, images[*,1:nband], psf_struct, nband, save_folder
;==============================================================================

;run SExtractor
  IF setup.dosex THEN BEGIN
     print, 'starting SExtractor: '+systime(0)
     IF file_test(strtrim(setup.exclude,2)) THEN $
        readcol, setup.exclude, exclude_files, exclude_x, exclude_y, $
                 format = 'A,F,F', /silent $
     ELSE exclude_files = ''
     
     FOR i=0ul, nframes-1 DO BEGIN
        j = where(strtrim(exclude_files,2) EQ strtrim(images[i,0],2), ct)
        IF ct GT 0 THEN BEGIN
           exclude = [[transpose(exclude_x[j]), transpose(exclude_y[j])]] 
        ENDIF ELSE exclude = [[-1, -1]]
        IF NOT keyword_set(sex_skip) THEN $
           run_sextractor, setup, images, weights, outpath_file, i, exclude
     ENDFOR
     
;combine all SExtractor catalogues
     merge_sex_catalogues, outpath_file[*,0]+setup.outcat, $
                           outpath_file[*,0]+setup.outparam, images[*,0], neighbours, $
                           setup.outdir+setup.sexcomb
     
;creare ds9 region file for all detections
     sex2ds9reg, setup.outdir+setup.sexcomb, outpath_file[0,0]+setup.outparam, $
                 setup.outdir+'sexcomb.reg', 10, color='green', tag = 'comb'
     IF keyword_set(logfile) THEN $
        update_log, logfile, 'SExtraction... done!'
     print, 'finished SExtractor: '+systime(0)
  ENDIF

;==============================================================================
;create postage stamp description files 
  IF setup.dostamps THEN BEGIN
; use bridge to speed up process
;create object array for child processes 
; create maximum max_proc parallels. Anything more blocks the harddrive,
; limiting factor seems to be writing speed of disk.
     max_proc = 6 < nframes
     post_bridge_arr = objarr(setup.max_proc <max_proc)
;allow main to see which process is free
     post_bridge_use = bytarr(setup.max_proc <max_proc)
;initialise every bridge (specify output property to allow debugging)
     IF setup.max_proc <(max_proc) GT 1 THEN BEGIN
        IF keyword_set(bridgejournal) THEN BEGIN
           FOR i=0, setup.max_proc-1 <(max_proc-1) DO post_bridge_arr[i] = obj_new('IDL_IDLBridge', output=journal_folder+'/bridge_journal_postage_stamps_'+strtrim(i,2))
        ENDIF ELSE BEGIN
           FOR i=0, setup.max_proc-1 <(max_proc-1) DO post_bridge_arr[i] = obj_new('IDL_IDLBridge')
        ENDELSE
        FOR i=0, setup.max_proc-1 <(max_proc-1) DO BEGIN
           post_bridge_arr[i]->execute, 'astrolib'
           post_bridge_arr[i]->execute, '.r '+gala_pro
        ENDFOR
     ENDIF 
     done_cnt=0L
     i=0
     
     REPEAT BEGIN
;get status of bridge elements
        IF setup.max_proc <(max_proc) GT 1 THEN BEGIN
           FOR l=0, setup.max_proc-1 < (max_proc-1) DO post_bridge_use[l] = post_bridge_arr[l]->status()
        ENDIF
;check for free bridges
        free = where(post_bridge_use eq 0, ct)
        
        IF ct GT 0 AND done_cnt NE nframes THEN BEGIN
;at least one bridge is free --> start newobject
;the available bridge is free[0]
           print, 'cutting postages for images '+strtrim(outpath_file_no_band[i,0],2)+' and similar'
           save, i, images, outpath_file, setup, outpath_file_no_band, outpath_band, $
                 outpre, nband, filename=outpath_file_no_band[i,0]+setup.stampfile+'.sav'
           
           IF setup.max_proc <max_proc GT 1 THEN BEGIN
              post_bridge_arr[free[0]]->execute, $
                 'stamp_file_bridge, "'+outpath_file_no_band[i,0]+setup.stampfile+'.sav"', /nowait
           ENDIF ELSE BEGIN
              stamp_file_bridge, outpath_file_no_band[i,0]+setup.stampfile+'.sav'
           ENDELSE
           done_cnt = done_cnt+1
           i=i+1
           wait, 1
;switch to next object
        ENDIF ELSE BEGIN
;all bridges are busy --> wait
           wait, 5
        ENDELSE
;stop when all done and no bridge in use any more
     ENDREP UNTIL done_cnt EQ nframes AND total(post_bridge_use) EQ 0
     
; kill bridge, make new one!
     print, 'finished cutting postage stamps, now killing bridge: '+systime(0)
     IF n_elements(post_bridge_arr) GT 0 THEN obj_destroy, post_bridge_arr
     
     post_bridge_arr = objarr(setup.max_proc < nframes)
;allow main to see which process is free
     post_bridge_use = bytarr(setup.max_proc < nframes)
;initialise every bridge (specify output property to allow debugging)
     IF setup.max_proc <(nframes) GT 1 THEN BEGIN
        IF keyword_set(bridgejournal) THEN BEGIN
           FOR i=0, setup.max_proc-1 < (nframes-1) DO post_bridge_arr[i] = obj_new('IDL_IDLBridge', output=journal_folder+'/bridge_journal_skymaps_'+strtrim(i,2))
        ENDIF ELSE BEGIN
           FOR i=0, setup.max_proc-1 < (nframes-1) DO post_bridge_arr[i] = obj_new('IDL_IDLBridge')
        ENDELSE
        FOR i=0, setup.max_proc-1 < (nframes-1) DO BEGIN
           post_bridge_arr[i]->execute, 'astrolib'
           post_bridge_arr[i]->execute, '.r '+gala_pro
        ENDFOR
     ENDIF
     
;create skymap files using bridge
     done_cnt=0L
     i=0
     print, 'starting skymaps: '+systime(0)
     REPEAT BEGIN
;get status of bridge elements
        IF setup.max_proc <(nframes) GT 1 THEN BEGIN
           FOR l=0, setup.max_proc-1 < (nframes-1) DO post_bridge_use[l] = post_bridge_arr[l]->status()
        ENDIF
;check for free bridges
        free = where(post_bridge_use EQ 0, ct)
        
        IF ct GT 0 AND done_cnt NE nframes THEN BEGIN
;at least one bridge is free --> start newobject
;the available bridge is free[0]
;       print, 'cutting postage stamps for '+strtrim(setup.stamp_pre[b],2)+'-band'
           
           save, i, weights, outpath_file, setup, outpath_file_no_band, nband, filename=outpath_file_no_band[i,0]+'skymap.sav'
           IF setup.max_proc GT 1 AND nframes GT 1 THEN BEGIN
              print, 'starting skymap '+outpath_file_no_band[i,0]+'skymap.sav'
              post_bridge_arr[free[0]]->execute, $
                 'skymap_bridge, "'+outpath_file_no_band[i,0]+'skymap.sav"', /nowait
           ENDIF ELSE BEGIN
              print, 'starting skymap '+outpath_file_no_band[i,0]+'skymap.sav'
              skymap_bridge, outpath_file_no_band[i,0]+'skymap.sav'
           ENDELSE
           done_cnt = done_cnt+1
           i=i+1
           wait, 1
;switch to next object
        ENDIF ELSE BEGIN
;all bridges are busy --> wait
           wait, 5
        ENDELSE
;stop when all done and no bridge in use any more
     ENDREP UNTIL done_cnt eq nframes and total(post_bridge_use) EQ 0
     
     print, 'finished writing skymaps, now killing bridge: '+systime(0)
     IF n_elements(post_bridge_arr) GT 0 THEN obj_destroy, post_bridge_arr
     
     IF keyword_set(logfile) THEN $
        update_log, logfile, systime()+': Postage stamps... done!'
     print, 'finished cutting postage stamps: '+systime(0)
  ENDIF 
  
; check whether skymaps exist, if not, something went wrong above
  skymap_exist = intarr(nframes,nband)
  for t=0ul,nframes-1 do begin
     for b=1,nband do begin
        skymap_exist[t,b-1] = file_test(strtrim(outpath_file_no_band[t,b]+setup.stamp_pre[b]+'.'+setup.skymap+'.fits',2))
     endfor
  endfor

  if (total(skymap_exist) ne nframes*nband) and (setup.dosky eq 1 or setup.dobd eq 1) then begin
     print, ' '
     print, 'WARNING'
     print, 'The number of skymap files on your disk does not correspond to the number that should exist. It seems that your block C ' + $
            'did not finish correctly. It has been reported that when you have one frame only, you need to set D18 to "1" for this block to work. ' + $
            'I think I fixed this bug and have never seen this behaviour myself, but if you can read this, maybe give it a try. ' + $
            'This will mean that the processes crash in the "bridge", which will be hard to find given the lack of feedback. This is why I am ' +$
            'telling you here'
     stop
  endif
  
;==============================================================================
  IF keyword_set(logfile) THEN $
     update_log, logfile, systime()+': Setting up output catalogue...'
;==============================================================================
;read in the combined SExtractor table
  IF keyword_set(jump1) THEN GOTO, jump_over_this_1
  print, 'reading SExtractor output'
  sexcat = read_sex_table(setup.outdir+setup.sexcomb, $
                          outpath_file[0,0]+setup.outparam, $
                          add_col = ['frame', '" "'])
  
;sort the total catalogue by magnitude and select the brightest BRIGHT percent
  print, 'setting up table'
  br = sort(sexcat.mag_best)
  
; reordering objects to be in brightness order
  table = sexcat[br]
; make table.frame multi-wavelength-ready to be passed onto gala_bridge
  tableim = strarr(nband+1,n_elements(table.frame))
  
  for i = 0l, n_elements(images[*,0])-1 do begin
     whtableim = where(strtrim(table.frame,2) eq strtrim(images[i,0],2), ct)
     if ct gt 0 then for b=0,nband do tableim[b,whtableim] = images[i,b]
  ENDFOR 
  table=remove_tags(table,'frame')
  add_tag, table, 'frame', strarr(nband+1), table2
  table=table2
  
  table.frame = strtrim(tableim,2)
  add_tag, table, 'flag_galfit', 0, table2
  table=table2
  delvarx, table2
  
; this line only created an empty structure, no values in yet!
; fittab read in because for now this seems to be easier. Will be renamed to 'table' below.
; could and should be cleaned up at some point
  print, 'setting up second table, needed only for a short time, deleted afterwards'
  fittab = read_sex_param(outpath_file[0,0]+setup.outparam, n_elements(sexcat.mag_best), $
                          add_column = addcol)
  
  add_tag, fittab, 'frame', strarr(nband+1), fittab2
  fittab = fittab2
  delvarx, fittab2
  
; fill in sextractor values from table into fittab
  struct_assign, table, fittab
  
; rename table in order to work in the rest of the code
; this overwrites 'table' with fittab
  table = fittab
  delvarx, fittab
  
  save, sexcat, table, filename = setup.outdir+'table_before_start.sav'
jump_over_this_1:
  IF keyword_set(jump1) THEN restore, setup.outdir+'table_before_start.sav'
  
  nbr = n_elements(sexcat.mag_best)
  
  orgim = setup.images
  orgwht = setup.weights
  orgpath = set_trailing_slash(setup.outpath)
  orgpath_band = set_trailing_slash(setup.outpath_band)
  orgpre = setup.outpre
  nband = setup.nband
  
  orgpath_pre = orgpath_band+orgpre
  orgpath_file = orgpath
  FOR q=0, nband DO orgpath_file[*,q]=orgpath_pre[*,q]+strtrim(setup.stamp_pre[q],2)+'.'
  orgpath_file_no_band = orgpath
  FOR q=0, nband DO orgpath_file_no_band[*,q]=orgpath[*,q]+orgpre[*,q]
  
  IF setup.dosky or setup.dobd  THEN BEGIN 
     IF keyword_set(logfile) THEN $
        update_log, logfile, systime()+': Setting up objects list to be fit...'
     add_tag, table, 'do_list', 0, table_new
     table = table_new
     delvarx, table_new
     
; get sourcelist of intereting (primary) sources and correlate to catalogue
     IF (setup.srclist EQ '' OR setup.srclistrad LE 0) THEN BEGIN
        table.do_list = 1
     ENDIF ELSE BEGIN
; only do this when the sav file does not exist or is older than the
; sextractor table!
        IF NOT file_test(strtrim(setup.srclist,2)) THEN BEGIN
           print, "your target list in D21 does not exist. Please provide a valid file name of use 'none' or an empty string to indicate that this feature should not be used"
           stop
        ENDIF
        srclist_name = strtrim(strmid(setup.srclist, strpos(setup.srclist,'/',/reverse_search)+1),2)
        sav_file_test = file_info(strtrim(setup.outdir+'primary_list_'+srclist_name+'.sav',2))
        sex_file_test = file_info(strtrim(setup.outdir+setup.sexcomb,2))
        
        IF sav_file_test.exists EQ 0 OR (sav_file_test.exists EQ 1 AND sav_file_test.mtime LT sex_file_test.mtime) THEN BEGIN
           print, 'correlating SExtractor catalogue to source list. Might take some time'
           readcol, setup.srclist, do_ra, do_dec, format='F,F', comment='#', /SILENT   
           
           srccor, table.alpha_j2000/15., table.delta_j2000, do_ra/15., do_dec, $
                   setup.srclistrad, tab_i, do_i, OPTION=0, /SPHERICAL, /SILENT
           
; print indices in to file to be read in next time (much faster)
; This has to be done here and not when cutting the postage stamps,
; because the order of objects is different, so indices would be wrong
           save, tab_i, filename=setup.outdir+'primary_list_'+srclist_name+'.sav'
        ENDIF ELSE BEGIN
           print, 'source correlation has already been done, simply reading result!'
           restore, setup.outdir+'primary_list_'+srclist_name+'.sav'
        ENDELSE
        
; if sav file exists and is newer than sextractor table, simply read
; in the indices from there!
        table[tab_i].do_list = 1
        delvarx, tab_i
     ENDELSE
     
  ENDIF
  
;==============================================================================
;measure sky and run galfit?
;==============================================================================
  IF setup.dosky THEN BEGIN
     IF keyword_set(logfile) THEN $
        update_log, logfile, systime()+': Beginning sky loop...'
     
; fittab does NOT contain FRAME (which is needed quite often!) Other
; than that, table is a subset of parameters, fittab is a subset of
; objects (only [br])
; set standard values
     table.mag_galfit = 999.
     table.mag_galfit_band = fltarr(nband)+999.
     table.re_galfit = -1.
     table.re_galfit_band = fltarr(nband)-1.
     table.n_galfit = -1.
     table.n_galfit_band = fltarr(nband)-1.
     table.q_galfit = -1.
     table.q_galfit_band = fltarr(nband)-1.
     
;calculate sky for the brightest objects
;******************************************************************************
;******************************************************************************
;current object (will be used and overwritten by the new, optimized, queue)
     cur = 0l
;create object array for child processes 
     bridge_arr = objarr(setup.max_proc)
;allow main to see which process is free
     bridge_use = bytarr(setup.max_proc)
;save the object number (needed to update the table)
     bridge_obj = lonarr(setup.max_proc)-1
;save the positions of the objects in the bridges
     bridge_pos = dblarr(2, setup.max_proc)+!values.F_NAN
     
;initialise every bridge (specify output property to allow debugging)
     IF setup.max_proc GT 1 THEN BEGIN
        IF keyword_set(bridgejournal) THEN BEGIN
           FOR i=0, setup.max_proc-1 DO bridge_arr[i] = obj_new('IDL_IDLBridge', output=journal_folder+'/bridge_journal_fitting_'+strtrim(i,2))
        ENDIF ELSE BEGIN
           FOR i=0, setup.max_proc-1 DO bridge_arr[i] = obj_new('IDL_IDLBridge')
        ENDELSE
        FOR i=0, setup.max_proc-1 DO BEGIN
           bridge_arr[i]->execute, 'astrolib'
           bridge_arr[i]->execute, '.r '+gala_pro
           bridge_arr[i]->execute, '.r gala_bridge'
           bridge_arr[i]->execute, '.r derive_primary_chi2'
        ENDFOR
     ENDIF
     
     IF keyword_set(plot) THEN BEGIN
        loadct,39,/silent
        plot, table.alpha_j2000, table.delta_j2000, psym=3, ystyle=1, xstyle=1
     ENDIF
     
; set up batch mode! (has to be done for single-sersic and B/D
; independently, otherwise '/jump2' will not work
     print, 'Bridge started, now adding some columns to the table to define objects to be done'
     add_tag, table, 'do_batch', 0, table_new
     table = table_new
     delvarx, table_new
     table[*].do_batch = 0
     
;loop over all batch frames and set do_batch =1
     IF file_test(strtrim(setup.batch,2)) AND strlen(setup.batch) GT 0 THEN BEGIN
        readcol, setup.batch, batch, format = 'A', comment = '#', /silent
        print, 'batch file detected, only doing the following image:'
        IF n_elements(batch) GT 0 THEN BEGIN

           FOR f=0ul, n_elements(batch)-1 DO BEGIN
              print, batch[f]
              dum = where(strtrim(table.frame[0],2) EQ strtrim(batch[f],2), ct)
              IF ct EQ 0 THEN CONTINUE
              table[dum].do_batch = 1
           ENDFOR
        ENDIF
        
     ENDIF ELSE table[*].do_batch = 1
     
;loop over all objects
     loop = 0l
     print, 'starting fitting at '+systime()
   
     REPEAT BEGIN
        IF loop MOD 100000 EQ 0 AND keyword_set(logfile) THEN BEGIN
           update_log, logfile, systime()+': last in cue... '+strtrim(cur, 2)
           FOR i=0, setup.max_proc-1 DO $
              update_log, logfile, systime()+': Bridge status... '+ $
                          strtrim(bridge_arr[i]->status(), 2)
        ENDIF
        loop++
        
;figure out which object to do next
loopstart:
        todo=where(table.flag_galfit EQ 0 AND table.do_list EQ 1 AND table.do_batch EQ 1, ctr)
        IF ctr EQ 0 THEN BEGIN
           IF setup.max_proc gt 1 THEN BEGIN
              FOR i=0, setup.max_proc-1 DO bridge_use[i] = bridge_arr[i]->status()
              GOTO, loopend
           ENDIF
        ENDIF
        ct = 0
        
loopstart2:
;get status of bridge elements
        IF setup.max_proc GT 1 THEN BEGIN
           FOR i=0, setup.max_proc-1 DO bridge_use[i] = bridge_arr[i]->status()
        ENDIF
;check for free bridges
        free = where(bridge_use EQ 0, ct)
        
        IF ct GT 0 AND todo[0] NE -1 THEN BEGIN
           
;at least one bridge is free --> start newobject
;the available bridge is free[0]
           
;treat finished objects first
           IF bridge_obj[free[0]] GE 0 THEN BEGIN
;read in feedback data
              idx = where(strtrim(table[bridge_obj[free[0]]].frame[0],2) EQ strtrim(orgim[*,0],2))
              objnum = round_digit(table[bridge_obj[free[0]]].number, 0, /str)
              obj_file = (outpath_galfit[idx]+orgpre[idx]+objnum+'_'+setup.obj)[0]
              out_file = (outpath_galfit[idx]+orgpre[idx]+objnum+'_'+setup.galfit_out)[0]
              sky_file = strarr(nband+1)
              FOR q=1,nband DO sky_file[q] = (outpath_galfit[idx]+orgpre[idx,q]+objnum+'_'+setup.stamp_pre[q]+'_'+setup.outsky)[0]
              
;check if file was done successfully or bombed
              update_table, table, bridge_obj[free[0]], out_file, obj_file, sky_file, nband, setup
;else table is automatically filled with standard values
              
;clear object
              bridge_obj[free[0]] = -1
;clear position of finished object
              bridge_pos[*, free[0]]= [!values.F_NAN, !values.F_NAN]
           ENDIF
           
;check if current position is far enough from bridge positions
           filled = where(finite(bridge_pos[0, *]) EQ 1 AND $
                          bridge_use GT 0, ct)
           ob=0l
           blocked=-1
           IF ct GT 0 THEN BEGIN
              REPEAT BEGIN
; get distance to all active objects
;                    print, table[todo[ob]].alpha_j2000, table[todo[ob]].delta_j2000
                 gcirc, 1, table[todo[ob]].alpha_j2000/15d, table[todo[ob]].delta_j2000, $
                        bridge_pos[0, filled]/15d, bridge_pos[1, filled], dist
; get distance to all blocked objects
                 IF n_elements(blocked) GT 1 THEN $
                    gcirc, 1, table[todo[ob]].alpha_j2000/15d, table[todo[ob]].delta_j2000, $
                           table[blocked[1:n_elements(blocked)-1]].alpha_j2000/15d, $
                           table[blocked[1:n_elements(blocked)-1]].delta_j2000, dist_block
                 IF n_elements(blocked) EQ 1 THEN dist_block = 2*setup.min_dist
; can be simplified when the plots are taken out!
                 IF min(dist) LT setup.min_dist or min(dist_block) lt setup.min_dist_block THEN BEGIN
                    blocked = [[blocked],todo[ob]]
                 ENDIF
                 
                 ob++
                 IF ob EQ n_elements(todo) AND $
                    (min(dist) LT setup.min_dist OR min(dist_block) LT setup.min_dist_block) THEN BEGIN
                    wait, 1
                    ob=0l
;                        print, 'starting over'
;                        wait, 1
                    goto, loopstart2
                 ENDIF
                 
              ENDREP UNTIL (min(dist) GE setup.min_dist AND min(dist_block) GE setup.min_dist_block) OR ob GE n_elements(todo)-1
              IF min(dist) LT setup.min_dist or min(dist_block) lt setup.min_dist_block THEN CONTINUE
           ENDIF
           ob=ob-1>0; because I already added  1 to ob above in the repeat loop. I want the CURRENT object, not the next
           cur=todo[ob]
           
; check whether this object has already been done, if so, read in
; result
           ct = 0l 
           idx = where(strtrim(table[cur].frame[0],2) EQ strtrim(orgim[*,0],2), ct)
           IF ct GT 0 THEN BEGIN
              objnum = round_digit(table[cur].number, 0, /str)
              obj_file = (outpath_galfit[idx]+orgpre[idx]+objnum+'_'+setup.obj)[0]
              out_file = (outpath_galfit[idx]+orgpre[idx]+objnum+'_'+setup.galfit_out)[0]
              sky_file = strarr(nband+1)
              for q=1,nband do sky_file[q] = (outpath_galfit[idx]+orgpre[idx,q]+objnum+'_'+setup.stamp_pre[q]+'_'+setup.outsky)[0]
;check if file was done successfully or bombed and update table                  
              IF file_test(strtrim(obj_file,2)) THEN BEGIN
                 print, 'Updating table now! ('+strtrim(cur, 2)+'/'+strtrim(nbr, 1)+')'                      
                 update_table, table, cur, out_file, obj_file, sky_file, nband, setup
                 IF n_elements(todo) NE 1 THEN GOTO, loopstart
                 IF n_elements(todo) EQ 1 THEN GOTO, loopend
              ENDIF
           ENDIF
           
;store position of new object
           bridge_obj[free[0]] = cur
           bridge_pos[*, free[0]] = [table[cur].alpha_j2000, table[cur].delta_j2000]
           table[cur].flag_galfit = 1
; get numbers for statusline
           nr_obj = n_elements(where(table.flag_galfit ne 0))
           nr_todo = strtrim(n_elements(where(table.do_list EQ 1 AND table.do_batch eq 1)),2)
           nr_total = strtrim(n_elements(table),2)
           objects_done = where(table.flag_galfit EQ 2)
           nr_done = strtrim(n_elements(objects_done),2)
           IF objects_done[0] EQ '-1' THEN nr_done = '0'
           objects_not_done = where(table.flag_galfit EQ -1)
           nr_not_done = strtrim(n_elements(objects_not_done),2)
           IF objects_not_done[0] EQ '-1' THEN nr_not_done = '0'
; print information on progress
           IF nr_obj MOD 10 EQ 0 THEN print, systime()+': starting object No. '+strtrim(nr_obj,2)+' of '+nr_todo+' (of '+nr_total+' objects detected). For now: '+nr_done+' fits finished,  '+nr_not_done+' not started (too few images with data)'
           IF keyword_set(plot) THEN BEGIN
              plot, table.alpha_J2000,table.delta_J2000, psym=3, ystyle=1, xstyle=1
              IF n_elements(blocked) GT 1 THEN BEGIN
                 plots, table[blocked[1:n_elements(blocked)-1]].alpha_J2000,table[blocked[1:n_elements(blocked)-1]].delta_J2000, psym=4, col=200, symsize=2
                 FOR r=1,n_elements(blocked)-1 DO tvellipse, setup.min_dist_block/3600., setup.min_dist_block/3600., table[blocked[r]].alpha_J2000,table[blocked[r]].delta_J2000,col=200,/data
              ENDIF                
              done=where(table.flag_galfit ge 1, count)
              IF count GE 1 THEN BEGIN
                 plots, table[done].alpha_J2000,table[done].delta_J2000, psym=1, col=135, thick=2, symsize=0.5
              ENDIF
              plots, bridge_pos[0,*], bridge_pos[1,*], psym=1, col=235
              FOR q=0,n_elements(bridge_pos[0,*])-1 DO tvellipse, setup.min_dist/3600., setup.min_dist/3600., bridge_pos[0,q], bridge_pos[1,q], col=235,/data,thick=2
           ENDIF
           
;find the matching filenames
           idx = where(strtrim(table[cur].frame[0],2) EQ strtrim(orgim[*,0],2))
;define the file names for the:
;postage stamp parameters
           stamp_param_file = (orgpath_file_no_band[idx,0]+setup.stampfile)[0]
           objnum = round_digit(table[cur].number, 0, /str)
;galfit masks
           mask_file = strarr(nband+1)
           FOR q=1,nband DO mask_file[q] = (outpath_galfit[idx]+outpre[idx,q]+objnum+'_'+setup.stamp_pre[q]+'_'+setup.mask)[0]
           mask_file_primary = (outpath_galfit[idx]+outpre[idx,1]+objnum+'_'+setup.mask+'_primary')[0]
;galfit obj file
           obj_file = (outpath_galfit[idx]+orgpre[idx]+objnum+'_'+setup.obj)[0]
;galfit constraint file
           constr_file = (outpath_galfit[idx]+orgpre[idx]+objnum+'_'+setup.constr)[0]
;galfit input file
           im_file = strarr(nband+1)
           FOR q=1,nband DO im_file[q] = (orgpath_pre[idx,q]+objnum+'_'+setup.stamp_pre[q])[0]
;galfit sigma maps
           sigma_file = strarr(nband+1)
           FOR q=1,nband DO sigma_file[q] = (orgpath_pre[idx,q]+objnum+'_'+setup.stamp_pre[q]+'_sigma')[0]
           wh_no_sigma = where(setup.sigflags EQ 0,cntns)
           IF cntns GT 0 THEN sigma_file[wh_no_sigma] = 'none'
           
;galfit output path
           out_file = (outpath_galfit[idx]+orgpre[idx]+objnum+'_'+setup.galfit_out)[0]
;sky summary file
           sky_file = strarr(nband+1)
           FOR q=1,nband DO sky_file[q] = (outpath_galfit[idx]+outpre[idx,q]+objnum+'_'+setup.stamp_pre[q]+'_'+setup.outsky)[0]
           
; choose closest PSF according to RA & DEC and subtract filename from structure 'psf_struct'
; read in chosen_psf into 'psf', filename in chosen_psf_file   
           choose_psf, table[cur].alpha_j2000, table[cur].delta_j2000, $
                       psf_struct, table[cur].frame, chosen_psf_file, nband
           
; change seed for random in getsky_loop
; this isn't really random, but random enough for the purpose and
; allows exact re-running of the code
           seed=table[cur].number
; create sav file for gala_bridge to read in
           
; writing out the fittab into the sav file is HUGE for big surveys!
           
; USE 'NEIGHBOURS' TO CUT DOWN TABLE SIZE OF FITTAB!!
;select part of table with frames neighbouring the current frame;
           int_obj = where(strtrim(table.frame[0],2) EQ strtrim(table[cur].frame[0],2))
           
           FOR i=0ul, n_elements(neighbours[*, idx])-1 DO BEGIN
              tabi = where(strtrim(table.frame[0],2) EQ strtrim(neighbours[i, idx[0]],2), ct)
              IF ct GT 0 THEN int_obj = [int_obj, tabi]
           ENDFOR
           save_table = table[int_obj]
           
; find new values of [cur] and 
; [idx] will stay the same because it's not the object, but the tile it is on!
           save_cur = where(strtrim(save_table.frame[0],2) eq strtrim(table[cur].frame[0],2) and save_table.number eq table[cur].number)
           save_cur = save_cur[0]
           
;print, systime()+'  writing '+out_file+'.sav'
           save, save_cur, orgwht, idx, orgpath, orgpre, setup, chosen_psf_file,$
                 sky_file, stamp_param_file, mask_file, mask_file_primary, im_file, sigma_file, obj_file, $
                 constr_file, out_file, save_table, nband, orgpath_pre, outpath_file, $
                 outpath_file_no_band, orgpath_file_no_band, outpath_galfit, $
                 orgpath_band, orgpath_file, seed,$
                 filename=out_file+'.sav'
;print, systime()+'  written '+out_file+'.sav'
           
           IF setup.max_proc GT 1 THEN BEGIN
              IF keyword_set(logfile) THEN $
                 update_log, logfile, systime()+': starting new bridge... ('+out_file+' on '+strtrim(free[0],2)+')'
;print, systime()+'  starting '+out_file+'.sav on bridge'+strtrim(free[0],2)
              bridge_arr[free[0]]->execute, $
                 'gala_bridge, "'+out_file+'.sav"', /nowait
           ENDIF ELSE BEGIN
              IF keyword_set(logfile) THEN $
                 update_log, logfile, systime()+': Starting next object... ('+out_file+')'
              cd, orgpath[idx,0]
              gala_bridge, out_file+'.sav'
              file_delete, orgpath[idx,0]+'galfit.[0123456789]*', /quiet, $
                           /allow_nonexistent, /noexpand_path
           ENDELSE
           wait, 1
;switch to next object
        ENDIF ELSE BEGIN
;all bridges are busy --> wait 
           wait, 1
        ENDELSE
        
loopend:
;stop when all done and no bridge in use any more
     ENDREP UNTIL todo[0] EQ -1 AND total(bridge_use) EQ 0
     
;kill bridge
     print, 'finished all fits, now killing bridge: '+systime(0)
     IF n_elements(bridge_arr) GT 0 THEN obj_destroy, bridge_arr
     
     print, 'bridge killed, now reading in last batch: '+systime(0)
     
;have to read in the last batch of objects
     remain = where(bridge_obj ge 0, ct)
     IF ct GT 0 THEN BEGIN
        FOR i=0, ct-1 DO BEGIN
           idx = where(strtrim(table[bridge_obj[remain[i]]].frame[0],2) EQ strtrim(orgim[*,0],2))
           objnum = round_digit(table[bridge_obj[remain[i]]].number, 0, /str)
           obj_file = (outpath_galfit[idx]+orgpre[idx]+objnum+'_'+setup.obj)[0]
           out_file = (outpath_galfit[idx]+orgpre[idx]+objnum+'_'+setup.galfit_out)[0]
           sky_file = strarr(nband+1)
           FOR q=1,nband DO sky_file[q] = (outpath_galfit[idx]+orgpre[idx,q]+objnum+'_'+setup.stamp_pre[q]+'_'+setup.outsky)[0]
           
           IF keyword_set(plot) THEN BEGIN
              plots, table[bridge_obj[remain[i]]].alpha_J2000,table[bridge_obj[remain[i]]].delta_J2000, psym=1, col=135, thick=2, symsize=2
              tvellipse, setup.min_dist/3600., setup.min_dist/3600., $
                         table[bridge_obj[remain[i]]].alpha_J2000,table[bridge_obj[remain[i]]].delta_J2000, col=0,/data
           ENDIF
;            bridge_obj[remain[i]] = -1
;            bridge_pos[*, remain[i]]= [!values.F_NAN, !values.F_NAN]            
           
;check if file was done successfully or bombed
; if succesfully, fill fitting parameters into fittab
           update_table, table, bridge_obj[remain[i]], out_file, obj_file, sky_file, nband, setup
;print, 'out file exists -- fittab updated'
;else output file does not exist --> bombed
           
; overwrite indices
           bridge_obj[remain[i]] = -1
           bridge_pos[*, remain[i]]= [!values.F_NAN, !values.F_NAN]            
        ENDFOR
     ENDIF
     print, 'done fitting single sersic with batch file '+setup.batch
  ENDIF
  
;==============================================================================
; start B/D fitting.
; ?? a) optimized queue is not needed, all the single sersic fits already exist and should be read in
; !! b) sky estimations can be re-used.
; !! c) decision on neighbours could be re-used, too. Information need to be
; written out into a file, though. Or decision is reconsidered, as
; object parameters have changed and decision might be a different one.
; ?? d) Neighbours will only be deblended as single sersics!
  
  IF setup.dobd THEN BEGIN
; this sets minimum distance ==0, so disables the queue
; system. Objects are done in whatever order, as it does not matter,
; no results from other objects are used for the successive B/D fits
     setup.min_dist = 0
     setup.min_dist_block = 0
     
     outpath_galfit_bd = strtrim(outpath[*,0]+strmid(setup.galfit_out_path,0,strlen(setup.galfit_out_path)-1)+'_'+setup.bd_label,2)
     outpath_galfit_bd = set_trailing_slash(outpath_galfit_bd)
     FOR i=0ul, n_elements(outpath_galfit_bd)-1 DO $
        IF NOT file_test(strtrim(outpath_galfit_bd[i],2)) THEN $
           spawn, 'mkdir -p '+outpath_galfit_bd[i]
     
     IF setup.bd_hpc THEN print, 'only preparing the galfit feedme files, NO fits done!'
     print, 'reading all single sersic results so that B/D has the best possible knowledge' 
     
; first read in all single sersic results (ALL)
; This should NOT be neccessary, when table and fittab contain the
; same objects and/or if a proper database is used!!
     
     ntab = n_elements(table)
     
     IF keyword_set(jump2) THEN GOTO, jump_over_this_2
     
;find the image files for the sources
     print,' '
     sscnt =0ul 
     FOR i=0ul, ntab-1 DO BEGIN
;        statusline, ' reading single sersic result from object '+strtrim(i+1,2)+' of '+strtrim(ntab,2)+', '+strtrim(sscnt,2)+' objects succesfully read in'
        print, ' reading single sersic result from object '+strtrim(i+1,2)+' of '+strtrim(ntab,2)+', '+strtrim(sscnt,2)+' objects succesfully read in'
        objnum = round_digit(table[i].number, 0, /str)
        
        idx = where(strtrim(table[i].frame[0],2) EQ strtrim(orgim[*,0],2))
        out_file = (outpath_galfit[idx]+orgpre[idx]+objnum+'_'+setup.galfit_out)[0]
        
        obj_file = (outpath_galfit[idx]+orgpre[idx]+objnum+'_'+setup.obj)[0]
        sky_file = strarr(nband+1)
        FOR q=1,nband DO sky_file[q] = (outpath_galfit[idx]+orgpre[idx,q]+objnum+'_'+setup.stamp_pre[q]+'_'+setup.outsky)[0]
; read in single sersic again
        IF file_test(strtrim(out_file+'.fits',2)) THEN sscnt += 1
        update_table, table, i, out_file, obj_file, sky_file, nband, setup
        table[i].org_image_band = orgim[idx[0],1:nband]
        
     ENDFOR
     print, ' '
     print, strtrim(sscnt,2)+' succesful single-sersic fits read in'
     print, ' '
     
     save, table, filename = setup.outdir+'table_before_bd_'+strmid(setup.galfit_out_path,0,strlen(setup.galfit_out_path)-1)+'.sav'
jump_over_this_2:
     
     IF keyword_set(jump2) THEN BEGIN
        print, 'reading already exisiting table instead of all results'
        restore, setup.outdir+'table_before_bd_'+strmid(setup.galfit_out_path,0,strlen(setup.galfit_out_path)-1)+'.sav'
     ENDIF

     print, 'Got all single-sersic fit results now and setting up table for B/D fits'
;=========================================================================
; finished reading in all single sersic results
;=========================================================================
;;====create BD sourcelist================================================
     add_tag, table, 'do_list_bd', 0, table_new
     table = table_new
     delvarx, table_new

; get sourcelist of interesting (primary) sources and correlate to catalogue
     IF (setup.bd_srclist EQ '' OR setup.bd_srclistrad LE 0) THEN BEGIN
        table.do_list_bd = 1
     ENDIF ELSE BEGIN
; only do this when the sav file does not exist or is older than the
; sextractor table!
        IF NOT file_test(strtrim(setup.bd_srclist,2)) THEN BEGIN
           print, "your target list in F04 does not exist. Please provide a valid file name of use 'none' or an empty string to indicate that this feature should not be used"
           stop
        ENDIF
        bd_srclist_name = strtrim(strmid(setup.bd_srclist, strpos(setup.bd_srclist,'/',/reverse_search)+1),2)
        sav_file_test = file_info(strtrim(setup.outdir+'primary_list_'+bd_srclist_name+'.sav',2))
        sex_file_test = file_info(strtrim(setup.outdir+setup.sexcomb,2))
        
        IF sav_file_test.exists EQ 0 OR (sav_file_test.exists EQ 1 AND sav_file_test.mtime LT sex_file_test.mtime) THEN BEGIN
           print, 'correlating SExtractor catalogue to source list for B/D. Might take some time'
           readcol, setup.bd_srclist, do_ra, do_dec, format='F,F', comment='#', /silent
           
           srccor, table.alpha_j2000/15., table.delta_j2000, do_ra/15., do_dec, $
                   setup.srclistrad, tab_i, do_i, OPTION=0, /SPHERICAL, /SILENT
           
; print indices in to file to be read in next time (much faster)
; This has to be done here and not when cutting the postage stamps,
; because the order of objects is different, so indices would be wrong
           save, tab_i, filename=setup.outdir+'primary_list_'+bd_srclist_name+'.sav'
        ENDIF ELSE BEGIN
; if sav file exists and is newer than sextractor table, simply read
; in the indices from there!
           print, 'source correlation has already been done, simply reading result!'
           restore, setup.outdir+'primary_list_'+bd_srclist_name+'.sav'
        ENDELSE
        
; set all wantd objects to 1
        table[tab_i].do_list_bd = 1
        delvarx, tab_i
     ENDELSE
     
     IF keyword_set(logfile) THEN $
        update_log, logfile, systime()+': Beginning Bulge_Disk_decomposition ...'
; set standard values for B/D parameters
; fill with standard values
     print, 'setting up table with default values'
     table.mag_galfit_d = 999.
     table.mag_galfit_band_d = fltarr(nband)+999
     table.re_galfit_d = -1.
     table.re_galfit_band_d = fltarr(nband)-1.
     table.n_galfit_d = -1.
     table.n_galfit_band_d = fltarr(nband)-1.
     table.q_galfit_d = -1.
     table.q_galfit_band_d = fltarr(nband)-1.
     table.mag_galfit_b = 999.
     table.mag_galfit_band_b = fltarr(nband)+999.
     table.re_galfit_b = -1.
     table.re_galfit_band_b = fltarr(nband)-1.
     table.n_galfit_b = -1.
     table.n_galfit_band_b = fltarr(nband)-1.
     table.q_galfit_b = -1.
     table.q_galfit_band_b = fltarr(nband)-1.
          
;******************************************************************************
;****************************************************************************** 
;calculate sky for the brightest objects
;current object (will be used and overwritten by the new, optimized, queue)
     
;; adapt setup
; setup.bd_hpc = 1
; setup.cheb_d = [0,0,8,0,-1,0,0]
; setup.cheb_b = [0,0,8,0,0,0,0]
; setup.bd_label = 'bd3'
; outpath_galfit_bd = strtrim(outpath[*,0]+strmid(setup.galfit_out_path,0,strlen(setup.galfit_out_path)-1)+'_'+setup.bd_label,2)
; outpath_galfit_bd = set_trailing_slash(outpath_galfit_bd)
; FOR i=0ul, n_elements(outpath_galfit_bd)-1 DO IF NOT file_test(outpath_galfit_bd[i]) THEN spawn, 'mkdirhier '+outpath_galfit_bd[i]
     
     IF NOT setup.bd_hpc THEN BEGIN
        cur = 0l
        delvarx, todo, bridge_arr, bridge_use, bridge_obj, bridge_pos, blocked
;create object array for child processes 
        bridge_arr = objarr(setup.max_proc)
;allow main to see which process is free
        bridge_use = bytarr(setup.max_proc)
;save the object number (needed to update the table)
        bridge_obj = lonarr(setup.max_proc)-1
;save the positions of the objects in the bridges
        bridge_pos = dblarr(2, setup.max_proc)+!values.F_NAN
        
;initialise every bridge (specify output property to allow debugging)
        IF setup.max_proc gt 1 THEN BEGIN
           print, 'setting up bridge processes'
           IF keyword_set(bridgejournal) THEN BEGIN
              FOR i=0, setup.max_proc-1 DO bridge_arr[i] = obj_new('IDL_IDLBridge', output=journal_folder+'/bridge_journal_fitting_bd_'+strtrim(i,2))
           ENDIF ELSE BEGIN
              FOR i=0, setup.max_proc-1 DO bridge_arr[i] = obj_new('IDL_IDLBridge')
           ENDELSE
           FOR i=0, setup.max_proc-1 DO BEGIN
              bridge_arr[i]->execute, 'astrolib'
              bridge_arr[i]->execute, '.r '+gala_pro
              bridge_arr[i]->execute, '.r gala_bd_bridge'
              bridge_arr[i]->execute, '.r derive_primary_chi2'
        ENDFOR  
           print, ' all bridges set up'
        ENDIF
        IF keyword_set(plot) THEN BEGIN
           loadct,39,/silent
           plot, table.alpha_j2000, table.delta_j2000, psym=3, ystyle=1, xstyle=1
        ENDIF
        
; set up batch mode! (has to be done for single-sersic and B/D
; independently, otherwise '/jump2' will not work
        print, 'adding one column to the table'
        table = remove_tags(table,'do_batch')
        add_tag, table, 'do_batch', 0, table_new
        table = table_new
        delvarx, table_new
        table[*].do_batch = 0
        
;loop over all batch frames and set do_batch =1
        IF file_test(strtrim(setup.batch,2)) AND strlen(setup.batch) GT 0 THEN BEGIN
           print, 'setting up batch mode for B/D fits'
           readcol, setup.batch, batch, format = 'A', comment = '#', /silent
           
           IF n_elements(batch) GT 0 THEN BEGIN
              FOR f=0ul, n_elements(batch)-1 DO BEGIN
                 dum = where(strtrim(table.frame[0],2) EQ strtrim(batch[f],2), ct)
                 IF ct EQ 0 THEN CONTINUE
                 table[dum].do_batch = 1
              ENDFOR
           ENDIF
           
        ENDIF ELSE table[*].do_batch = 1
        
;loop over all objects
        loop = 0l
        print, 'starting B/D fits at '+systime()
        
        REPEAT BEGIN
           IF loop MOD 100000 EQ 0 AND keyword_set(logfile) THEN BEGIN
              update_log, logfile, systime()+': last in cue... '+strtrim(cur, 2)
              FOR i=0, setup.max_proc-1 DO $
                 update_log, logfile, systime()+': Bridge status... '+ $
                             strtrim(bridge_arr[i]->status(), 2)
           ENDIF
           loop++
           
loopstart_bd:
; only successful single sersic object??
; todo eq flag_galfit eq 2 and flag_galfit_bd eq 0?????
; new source catalogue?
           
           todo=where(table.flag_galfit_bd EQ 0 AND table.do_list_bd EQ 1 AND table.do_batch EQ 1 AND $
                      table.mag_galfit_band[0] GT 0 AND table.mag_galfit_band[0] LT setup.bd_maglim, ctr)
           if ctr eq 0 then begin
              IF setup.max_proc gt 1 THEN BEGIN
                 FOR i=0, setup.max_proc-1 DO bridge_use[i] = bridge_arr[i]->status()
                 goto, loopend_bd
              ENDIF
           ENDIF
           ct = 0l
           
loopstart2_bd:
;get status of bridge elements
           IF setup.max_proc gt 1 THEN BEGIN
              FOR i=0, setup.max_proc-1 DO bridge_use[i] = bridge_arr[i]->status()
           ENDIF
;check for free bridges
           free = where(bridge_use EQ 0, ct)
           
           IF ct GT 0 AND todo[0] NE -1 THEN BEGIN
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
              
;at least one bridge is free --> start newobject
;the available bridge is free[0]
              
;treat finished objects first
              IF bridge_obj[free[0]] GE 0 THEN BEGIN
;read in feedback data
                 idx = where(strtrim(table[bridge_obj[free[0]]].frame[0],2) EQ strtrim(orgim[*,0],2))
                 objnum = round_digit(table[bridge_obj[free[0]]].number, 0, /str)
                 obj_file = (outpath_galfit_bd[idx]+orgpre[idx]+objnum+'_'+setup.bd_label+'_'+setup.obj)[0]
                 out_file = (outpath_galfit_bd[idx]+orgpre[idx]+objnum+'_'+setup.bd_label+'_'+setup.galfit_out)[0]
                 sky_file = strarr(nband+1)
;!!!                for q=1,nband do sky_file[q] = (outpath_galfit[idx]+orgpre[idx,q]+objnum+'_'+setup.stamp_pre[q]+'_bd_'+setup.outsky)[0]
                 for q=1,nband do sky_file[q] = (outpath_galfit[idx]+orgpre[idx,q]+objnum+'_'+setup.stamp_pre[q]+'_'+setup.outsky)[0]

;check if file was done successfully or bombed (done in update_table)
;else table is automatically filled with standard values     
                 update_table, table, bridge_obj[free[0]], out_file, obj_file, sky_file, nband, setup, /bd
                 
;clear object
                 bridge_obj[free[0]] = -1
;clear position of finished object
                 bridge_pos[*, free[0]]= [!values.F_NAN, !values.F_NAN]
              ENDIF
; only run the loop when the fit is actually done, everything else
; does not make sense
; check if current position is far enough from bridge positions
              filled = where(finite(bridge_pos[0, *]) EQ 1 AND $
                             bridge_use GT 0, ct)
              ob=0l
              blocked=-1
              IF ct GT 0 THEN BEGIN
                 REPEAT BEGIN
; get distance to all active objects
                    gcirc, 1, table[todo[ob]].alpha_j2000/15d, table[todo[ob]].delta_j2000, $
                           bridge_pos[0, filled]/15d, bridge_pos[1, filled], dist
; get distance to all blocked objects
                    IF n_elements(blocked) GT 1 THEN $
                       gcirc, 1, table[todo[ob]].alpha_j2000/15d, table[todo[ob]].delta_j2000, $
                              table[blocked[1:n_elements(blocked)-1]].alpha_j2000/15d, $
                              table[blocked[1:n_elements(blocked)-1]].delta_j2000, dist_block
                    IF n_elements(blocked) EQ 1 THEN dist_block = 2*setup.min_dist
; can be simplified when the plots are taken out!
                    IF min(dist) LT setup.min_dist OR min(dist_block) lt setup.min_dist_block THEN BEGIN
                       blocked = [[blocked],todo[ob]]
                    ENDIF

                    ob++
                    IF ob EQ n_elements(todo) AND $ ; no "-1" because I already added 1 to ob
                       (min(dist) LT setup.min_dist OR min(dist_block) LT setup.min_dist_block) THEN BEGIN
                       wait, 1
                       ob=0l
;                       print, 'starting loop over'
                       GOTO, loopstart2_bd
                    ENDIF
                    
                 ENDREP UNTIL (min(dist) GE setup.min_dist AND min(dist_block) GE setup.min_dist_block) OR ob GE n_elements(todo)-1
                 IF min(dist) LT setup.min_dist OR min(dist_block) LT setup.min_dist_block THEN CONTINUE
              ENDIF
              ob=ob-1>0 ; because I already added  1 to ob above in the repeat loop. I want the CURRENT object, not the next
              cur=todo[ob]

; perform some kind of STAR classification so B/D is only done for galaxies
; currently, SExtractor is used, but maybe others are more useful
;              if table[cur].class_star gt 0.8 then begin
;                  print, strtrim(cur,2)+' SEEMS TO BE A STAR! trying next object'
;                  table[cur].flag_galfit_bd = -1
;                  goto, loopstart_bd
;              ENDIF
              
; check whether this object has already been done, if so, read in
; result and restart
              ct = 0l
              idx = where(strtrim(table[cur].frame[0],2) EQ strtrim(orgim[*,0],2), ct)
              IF ct GT 0 THEN BEGIN
                 objnum = round_digit(table[cur].number, 0, /str)
                 obj_file = (outpath_galfit_bd[idx]+orgpre[idx]+objnum+'_'+setup.bd_label+'_'+setup.obj)[0]
                 out_file = (outpath_galfit_bd[idx]+orgpre[idx]+objnum+'_'+setup.bd_label+'_'+setup.galfit_out)[0]
                 sky_file = strarr(nband+1)
;!!!                for q=1,nband do sky_file[q] = (outpath_galfit[idx]+orgpre[idx,q]+objnum+'_'+setup.stamp_pre[q]+'_bd_'+setup.outsky)[0]
                 FOR q=1,nband DO sky_file[q] = (outpath_galfit[idx]+orgpre[idx,q]+objnum+'_'+setup.stamp_pre[q]+'_'+setup.outsky)[0]
;check if file was done successfully or bombed and update table                  
;                    IF file_test(out_file+'.fits') THEN BEGIN ; to be
;                    used for reruns after HPC. Delete obj files, run
;                    again. Also disable galfit in gala_bd_bridge
                 IF file_test(strtrim(obj_file,2)) THEN BEGIN
;                    print, obj_file+' found.'
                    print, 'Updating table now! ('+strtrim(cur, 2)+'/'+strtrim(nbr, 1)+')'                      
                    
                    update_table, table, cur, out_file, obj_file, sky_file, nband, setup, /bd
                    IF n_elements(todo) NE 1 THEN GOTO, loopstart_bd
                    IF n_elements(todo) EQ 1 THEN GOTO, loopend_bd
                 ENDIF
              ENDIF
              
;store position of new object
              bridge_obj[free[0]] = cur
              bridge_pos[*, free[0]] = [table[cur].alpha_j2000, table[cur].delta_j2000]
              table[cur].flag_galfit_bd = 1
              IF n_elements(where(table.flag_galfit_bd)) MOD 10 EQ 0 THEN $
                 print, systime()+': starting B/D on object No. '+strtrim(n_elements(where(table.flag_galfit_bd NE 0)),2)+' of ' $
                        +strtrim(n_elements(where(table.do_list_bd EQ 1 AND table.do_batch EQ 1 AND table.mag_galfit_band[0] GT 0 $
                                                  AND table.mag_galfit_band[0] LT setup.bd_maglim)),2)+ $
                        ' (of '+strtrim(n_elements(table),2)+' objects detected)   '
              IF keyword_set(plot) THEN BEGIN
                 plot, table.alpha_J2000,table.delta_J2000, psym=3, ystyle=1, xstyle=1
                 IF n_elements(blocked) GT 1 THEN BEGIN
                    plots, table[blocked[1:n_elements(blocked)-1]].alpha_J2000,table[blocked[1:n_elements(blocked)-1]].delta_J2000, psym=4, col=200, symsize=2
                    FOR r=1,n_elements(blocked)-1 DO tvellipse, setup.min_dist_block/3600., setup.min_dist_block/3600., table[blocked[r]].alpha_J2000,table[blocked[r]].delta_J2000,col=200,/data
                 ENDIF                
                 done=where(table.flag_galfit_bd GE 1, count)
                 IF count GE 1 THEN BEGIN
                    plots, table[done].alpha_J2000,table[done].delta_J2000, psym=1, col=135, thick=2, symsize=0.5
                 ENDIF
                 plots, bridge_pos[0,*], bridge_pos[1,*], psym=1, col=235
                 FOR q=0,n_elements(bridge_pos[0,*])-1 DO tvellipse, setup.min_dist/3600., setup.min_dist/3600., bridge_pos[0,q], bridge_pos[1,q], col=235,/data,thick=2
              ENDIF
              
; SOME OF THESE FILE NAMES NOT CURRENTLY USED
;find the matching filenames
              idx = where(strtrim(table[cur].frame[0],2) EQ strtrim(orgim[*,0],2))
;define the file names for the:
;postage stamp parameters
              stamp_param_file = (orgpath_file_no_band[idx,0]+setup.stampfile)[0]
              objnum = round_digit(table[cur].number, 0, /str)
;galfit masks
              mask_file = strarr(nband+1)
              FOR q=1,nband DO mask_file[q] = (outpath_galfit[idx]+outpre[idx,q]+objnum+'_'+setup.bd_label+'_'+setup.stamp_pre[q]+'_'+setup.mask)[0]
              mask_file_primary = (outpath_galfit[idx]+outpre[idx,1]+objnum+'_'+setup.mask+'_primary')[0]
;galfit obj file
              obj_file = (outpath_galfit[idx]+orgpre[idx]+objnum+'_'+setup.obj)[0]
              obj_file_bd = (outpath_galfit_bd[idx]+orgpre[idx]+objnum+'_'+setup.bd_label+'_'+setup.obj)[0]
;galfit constraint file
              constr_file = (outpath_galfit[idx]+orgpre[idx]+objnum+'_'+setup.constr)[0]
              constr_file_bd = (outpath_galfit_bd[idx]+orgpre[idx]+objnum+'_'+setup.bd_label+'_'+setup.constr)[0]
;galfit input file
              im_file = strarr(nband+1)
              FOR q=1,nband DO im_file[q] = (orgpath_pre[idx,q]+objnum+'_'+setup.stamp_pre[q])[0]
;galfit sigma maps
              sigma_file = strarr(nband+1)
              FOR q=1,nband DO sigma_file[q] = (orgpath_pre[idx,q]+objnum+'_'+setup.stamp_pre[q]+'_sigma')[0]
              wh_no_sigma = where(setup.sigflags EQ 0,cntns)
              IF cntns GT 0 THEN sigma_file[wh_no_sigma] = 'none'
              
;galfit output path
              out_file = (outpath_galfit[idx]+orgpre[idx]+objnum+'_'+setup.galfit_out)[0]
              out_file_bd = (outpath_galfit_bd[idx]+orgpre[idx]+objnum+'_'+setup.bd_label+'_'+setup.galfit_out)[0]
;sky summary file
              sky_file = strarr(nband+1)
              FOR q=1,nband DO sky_file[q] = (outpath_galfit[idx]+outpre[idx,q]+objnum+'_'+setup.stamp_pre[q]+'_'+setup.outsky)[0]
              
; choose closest PSF according to RA & DEC and subtract filename from structure 'psf_struct'
; read in chosen_psf into 'psf', filename in chosen_psf_file   
              choose_psf, table[cur].alpha_j2000, table[cur].delta_j2000, $
                          psf_struct, table[cur].frame, chosen_psf_file, nband
              
; change seed for random in getsky_loop
              seed=table[cur].number
; create sav file for gala_bridge to read in
              
; writing out the fittab into the sav file is HUGE for big surveys!
; USE 'NEIGHBOURS' TO CUT DOWN TABLE SIZE OF FITTAB!!
              
;select part of table with frames neighbouring the current frame;
; THIS PART IS PRETTY SLOW AND DISABLED FOR NOW as the table is not
; used by the B/D script
;           int_obj = where(table.frame[0] EQ table[cur].frame[0])
;            
;            FOR i=0ul, n_elements(neighbours[*, idx])-1 DO BEGIN
;               tabi = where(table.frame[0] EQ neighbours[i, idx[0]], ct)
;               IF ct GT 0 THEN int_obj = [int_obj, tabi]
;            ENDFOR
;              save_table = table[int_obj]
;            
;; find new values of [cur] and 
;; [idx] will stay the same because it's not the object, but the tile it is on!
;            save_cur = where(save_table.frame[0] eq table[cur].frame[0] and save_table.number eq table[cur].number)
;            save_cur = save_cur[0]
;            
;           save, save_cur, orgwht, idx, orgpath, orgpre, setup, chosen_psf_file,$
;                 sky_file, stamp_param_file, mask_file, im_file, obj_file, $
;                 constr_file, out_file, save_table, nband, orgpath_pre, outpath_file, $
;                 outpath_file_no_band, orgpath_file_no_band, outpath_galfit, $
;                 orgpath_band, orgpath_file, seed,$
;                 filename=out_file+'.sav'
;;;;;;;; END OF COMMENTED SECTION
              galfit_path = outpath_galfit_bd[idx]
              save, out_file, out_file_bd, obj_file, obj_file_bd, constr_file, constr_file_bd, setup, galfit_path, $
                    mask_file_primary, filename=out_file_bd+'.sav'
              
              IF setup.max_proc GT 1 THEN BEGIN
                 IF keyword_set(logfile) THEN $
                    update_log, logfile, systime()+': Starting new bridge... ('+out_file+' on '+strtrim(free[0],2)+')'
; print, 'starting new object at '+systime(0)
;                bridge_arr[free[0]]->execute, $
;                  'gala_bridge, "'+out_file+'.sav", /bd_fit', /nowait
;++++++++++++++++++++++++++ MARCOS SCRIPT
                 if file_test(strtrim(out_file+'.fits',2)) THEN $
                    bridge_arr[free[0]]->execute, $
                    'gala_bd_bridge, "'+out_file_bd+'.sav"',/nowait
                 
;PRO gala_bd_bridge, obj_fitstab_file, label, no_fit=no_fit
;;   num = '21_17.346'
;;   obj_fitstab_file = '/home/barden/Desktop/multi/BD_objects/t'+num+'_gf.fits'
                 
;--------------------------- MARCOS SCRIPT
                 
              ENDIF ELSE BEGIN
                 IF keyword_set(logfile) THEN $
                    update_log, logfile, systime()+': Starting next object... ('+out_file+')'
                 cd, orgpath[idx,0]
;                gala_bridge, out_file+'.bd.sav', /bd_fit', /nowait
;                file_delete, orgpath[idx,0]+'galfit.[0123456789]*', /quiet, $
;                  /allow_nonexistent, /noexpand_path
                 
;++++++++++++++++++++++++++ MARCOS SCRIPT
;                bd_fit, out_file+'.fits',setup.bd_label, setup.galexe
                 IF file_test(strtrim(out_file+'.fits',2)) THEN $
                    gala_bd_bridge, out_file_bd+'.sav'
;--------------------------- MARCOS SCRIPT
                 
              ENDELSE
              wait, 1
;switch to next object
           ENDIF ELSE BEGIN
;all bridges are busy --> wait 
              wait, 1
           ENDELSE
           
loopend_bd:
;stop when all done and no bridge in use any more
        ENDREP UNTIL todo[0] EQ -1 AND total(bridge_use) EQ 0
        
;kill bridge
        print, 'finished all B/D fits, now killing bridge: '+systime(0)
        IF n_elements(bridge_arr) GT 0 THEN obj_destroy, bridge_arr
        
        print, 'bridge killed, now reading in last batch: '+systime(0)
;have to read in the last batch of objects
        remain = where(bridge_obj ge 0, ct)
        IF ct GT 0 THEN BEGIN
           FOR i=0, ct-1 DO BEGIN
              idx = where(strtrim(table[bridge_obj[remain[i]]].frame[0],2) EQ strtrim(orgim[*,0],2))
              objnum = round_digit(table[bridge_obj[remain[i]]].number, 0, /str)
              obj_file = (outpath_galfit_bd[idx]+orgpre[idx]+objnum+'_'+setup.bd_label+'_'+setup.obj)[0]
              out_file = (outpath_galfit_bd[idx]+orgpre[idx]+objnum+'_'+setup.bd_label+'_'+setup.galfit_out)[0]
              sky_file = strarr(nband+1)
              FOR q=1,nband DO sky_file[q] = (outpath_galfit[idx]+orgpre[idx,q]+objnum+'_'+setup.stamp_pre[q]+'_'+setup.outsky)[0]
              
              IF keyword_set(plot) THEN BEGIN
                 plots, table[bridge_obj[remain[i]]].alpha_J2000,table[bridge_obj[remain[i]]].delta_J2000, psym=1, col=135, thick=2, symsize=2
                 tvellipse, setup.min_dist/3600., setup.min_dist/3600., $
                            table[bridge_obj[remain[i]]].alpha_J2000,table[bridge_obj[remain[i]]].delta_J2000, col=0,/data
              ENDIF
;                bridge_obj[remain[i]] = -1
;                bridge_pos[*, remain[i]]= [!values.F_NAN, !values.F_NAN]            
              
;check if file was done successfully or bombed
; if succesfully, fill fitting parameters into fittab
              
              update_table, table, bridge_obj[remain[i]], out_file, obj_file, sky_file, nband, setup, /bd
;print, 'out file exists -- fittab updated'
;else output file does not exist --> bombed
              
;overwrite indices
              bridge_obj[remain[i]] = -1
              bridge_pos[*, remain[i]]= [!values.F_NAN, !values.F_NAN]            
           ENDFOR
        ENDIF
     ENDIF
     
; If HPC mode, all the above , especially the 'loop' is not needed only files are created
; without knowledge of previous B/D fits.
; Also updating tables all the time is not needed as it does not have any influence on the further procedure
; no bridge is used, as the setup script is VERY fast
     IF setup.bd_hpc THEN BEGIN
        
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;        
        cur = 0l
; only successful single sersic object??
; todo eq flag_galfit eq 2 and flag_galfit_bd eq 0?????
        todo=where(table.flag_galfit_bd eq 0 AND table.do_list_bd EQ 1 and $
                   table.mag_galfit_band[0] gt 0 and table.mag_galfit_band[0] lt setup.bd_maglim, cntloop)
        
        FOR loopk= 0l, cntloop-1 DO BEGIN 
           cur = todo[loopk]
           
;find the matching filenames
           idx = where(strtrim(table[cur].frame[0],2) EQ strtrim(orgim[*,0],2))
;define the file names for the:
;postage stamp parameters
           stamp_param_file = (orgpath_file_no_band[idx,0]+setup.stampfile)[0]
           
           objnum = round_digit(table[cur].number, 0, /str)
           
;galfit obj file
           obj_file = (outpath_galfit[idx]+orgpre[idx]+objnum+'_'+setup.obj)[0]
           obj_file_bd = (outpath_galfit_bd[idx]+orgpre[idx]+objnum+'_'+setup.bd_label+'_'+setup.obj)[0]
;galfit constraint file
           constr_file = (outpath_galfit[idx]+orgpre[idx]+objnum+'_'+setup.constr)[0]
           constr_file_bd = (outpath_galfit_bd[idx]+orgpre[idx]+objnum+'_'+setup.bd_label+'_'+setup.constr)[0]
;galfit output path
           out_file = (outpath_galfit[idx]+orgpre[idx]+objnum+'_'+setup.galfit_out)[0]
           out_file_bd = (outpath_galfit_bd[idx]+orgpre[idx]+objnum+'_'+setup.bd_label+'_'+setup.galfit_out)[0]                
           
; check whether this object has already been done, if so, skip
           IF file_test(strtrim(obj_file_bd,2)) THEN print, obj_file_bd+' skipped, exists already'
           IF file_test(strtrim(obj_file_bd,2)) THEN CONTINUE
           
           IF loopk MOD 100 EQ 0 THEN $
              print, systime()+': starting object No. '+strtrim(loopk,2)+' of ' $
                     +strtrim(cntloop,2)+' (of '+strtrim(n_elements(table),2)+' objects detected)   '
           
; create sav file for bd_fit to read in
           galfit_path = outpath_galfit_bd[idx]
           save, out_file, out_file_bd, obj_file, obj_file_bd, constr_file, constr_file_bd, setup, galfit_path, $
                 filename=out_file_bd+'.sav'
           
           IF file_test(strtrim(out_file+'.fits',2)) THEN $
              gala_bd_bridge, out_file_bd+'.sav'
           
        ENDFOR
        
;all feedme files exist, prepare PC files
        gala_bd_bridge_hpc, table, setup
        
     ENDIF
     print, 'done fitting with batch file '+setup.batch
  ENDIF
  
;==============================================================================
;read in sextractor table, combine with galfit results, write out combined fits table
  IF setup.docombine or setup.docombinebd THEN BEGIN
     print, 'reading sexcat for output table'
     tab = read_sex_table(setup.outdir+setup.sexcomb, $
                          outpath_file[0,0]+setup.outparam, $
                          add_col = ['TILE', '" "'])
     ntab = n_elements(tab)
     
     out = read_sex_param(outpath_file[0,0]+setup.outparam, ntab, $
                          add_column = [[addcol],['TILE', '" "']])
     
     struct_assign, tab, out
     
; create ID for all objects
     add_tag, out, 'gala_ID', ' ', out2
     out = out2
     delvarx,out2
     
;find the image files for the sources
     orgim = setup.images
     orgwht = setup.weights
     orgpath = set_trailing_slash(setup.outpath)
     orgpath_band = set_trailing_slash(setup.outpath_band)
     orgpre = setup.outpre
     nband = setup.nband
     
     orgpath_pre = orgpath_band+orgpre
     orgpath_file = orgpath
     for q=0, nband do orgpath_file[*,q]=orgpath_pre[*,q]+strtrim(setup.stamp_pre[q],2)+'.'
     orgpath_file_no_band = orgpath
     for q=0, nband do orgpath_file_no_band[*,q]=orgpath[*,q]+orgpre[*,q]
     if setup.docombinebd then begin
        outpath_galfit_bd = strtrim(outpath[*,0]+strmid(setup.galfit_out_path,0,strlen(setup.galfit_out_path)-1)+'_'+setup.bd_label,2)
        outpath_galfit_bd = set_trailing_slash(outpath_galfit_bd)
     ENDIF
     
     print,' '
     read = 0L

     FOR i=0ul, ntab-1 DO BEGIN
        objnum = round_digit(tab[i].number, 0, /str)
        idx = where(strtrim(tab[i].tile,2) EQ strtrim(orgim[*,0],2))
        out_file = (outpath_galfit[idx]+orgpre[idx]+objnum+'_'+setup.galfit_out)[0] 
        
        if file_test(strtrim(out_file+'.fits',2)) then read += 1
        
; write galapagos ID
        out[i].gala_id = orgpre[idx]+objnum
        
        print, 'reading results for object No.'+strtrim(i+1,2)+' of '+strtrim(ntab,2)+' ('+strtrim(read,2)+' already read)' 
;        statusline, 'reading result '+strtrim(i+1,2)+' of '+strtrim(ntab,2)+'      '
        
        obj_file = (outpath_galfit[idx]+orgpre[idx]+objnum+'_'+setup.obj)[0]
        sky_file = strarr(nband+1)
        FOR q=1,nband DO sky_file[q] = (outpath_galfit[idx]+orgpre[idx,q]+objnum+'_'+setup.stamp_pre[q]+'_'+setup.outsky)[0]
        
; read in single sersic again
        update_table, out, i, out_file, obj_file, sky_file, nband, setup, /final
        
; read in B/D again
        IF setup.docombinebd THEN BEGIN
           out_file_bd = (outpath_galfit_bd[idx]+orgpre[idx]+objnum+'_'+setup.bd_label+'_'+setup.galfit_out)[0]
           obj_file_bd = (outpath_galfit_bd[idx]+orgpre[idx]+objnum+'_'+setup.bd_label+'_'+setup.obj)[0]
           update_table, out, i, out_file_bd, obj_file_bd, sky_file, nband, setup, /bd, /final
           out[i].org_image_band = orgim[idx[0],1:nband]
        ENDIF
        
     ENDFOR
     print, ' '
     
; cleaning bad detections from catalog
     print, 'catalogue has '+strtrim(n_elements(out.tile),2)+' objects'
     IF file_test(strtrim(setup.bad,2)) THEN BEGIN
        readcol, setup.bad, tile, x, y, format = 'A,F,F', comment = '#', /silent
        tiles = uniq(tile, sort(tile))
        flag = intarr(n_elements(out))
        print, ' '+strtrim(n_elements(x),2)+' positions to be removed'
        FOR i=0ul, n_elements(tiles)-1 DO BEGIN
           print, 'cleaning bad objects from output catalog, now from tile '+tile[tiles[i]]
           tileidx = where(tile EQ tile[tiles[i]], ct)
           print, '  '+strtrim(ct,2)+' positions to be removed in this tile (not all have to have objects)'
; If something in the bad detection list
           IF ct GT 0 THEN BEGIN
              catidx = where(strtrim(out.tile,2) EQ strtrim(tile[tiles[i]],2), ct1)
; if some objects on that same tile
              IF ct1 GT 0 THEN BEGIN
                 srccor, x[tileidx], y[tileidx], out[catidx].x_image, $
                         out[catidx].y_image, setup.exclude_rad, xy, oi, $
                         option = 1, /silent
                 IF oi[0] GE 0 THEN BEGIN
                    print, '  '+strtrim(n_elements(oi),2)+' objects deleted'
                    flag[catidx[oi]] = 1
                 ENDIF
              ENDIF
           ENDIF
        ENDFOR
        good = where(flag EQ 0, ct)
        IF ct EQ 0 THEN message, 'No objects in output catalogue left' $
        ELSE out = out[good]
     ENDIF 

; delete duplicate values from table
     out=remove_tags(out,['x_galfit', 'xerr_galfit','y_galfit', 'yerr_galfit','mag_galfit', $
                          'magerr_galfit','re_galfit', 'reerr_galfit','n_galfit', 'nerr_galfit', $
                          'q_galfit', 'qerr_galfit','pa_galfit', 'paerr_galfit', $
                          'x_galfit_b', 'xerr_galfit_b','y_galfit_b', 'yerr_galfit_b','mag_galfit_b', $
                          'magerr_galfit_b','re_galfit_b', 'reerr_galfit_b','n_galfit_b', 'nerr_galfit_b', $
                          'q_galfit_b', 'qerr_galfit_b','pa_galfit_b', 'paerr_galfit_b', $
                          'x_galfit_d', 'xerr_galfit_d','y_galfit_d', 'yerr_galfit_d','mag_galfit_d', $
                          'magerr_galfit_d','re_galfit_d', 'reerr_galfit_d','n_galfit_d', 'nerr_galfit_d', $
                          'q_galfit_d', 'qerr_galfit_d','pa_galfit_d', 'paerr_galfit_d', $
                          'sky_galfit', 'sky_galfit_bd', 'psf_galfit', 'psf_galfit_bd', 'org_image'])
     
; write galapagos and galfit version into catalogue
     add_tag, out, 'galapagos_version', galapagos_version, out2
     out = out2
     
; reorder all columns into useful order
; how can I keep ALL Sextractor columns? Use outparam file!
     readcol, setup.sexout, params, format='A',/silent
; params (standard)
;         'NUMBER',$
;         'X_IMAGE','Y_IMAGE','CXX_IMAGE','CYY_IMAGE','CXY_IMAGE',$
;         'THETA_IMAGE','THETA_WORLD','ELLIPTICITY','KRON_RADIUS','A_IMAGE',$
;         'B_IMAGE','ALPHA_J2000','DELTA_J2000','BACKGROUND','FLUX_BEST',$
;         'FLUXERR_BEST','MAG_BEST','MAGERR_BEST','FLUX_RADIUS','ISOAREA_IMAGE',$
;         'FWHM_IMAGE','FLAGS','CLASS_STAR',$
     order = ['GALAPAGOS_VERSION','GALA_ID', $
              params, $
              'TILE','ORG_IMAGE_BAND',$
              'SKY_GALA_BAND','SKY_SIG_BAND','SKY_RAD_BAND','SKY_FLAG_BAND',$
; galfit values
              'GALFIT_VERSION','FILE_GALFIT','INITFILE','CONSTRNT','LOGFILE','PSF_GALFIT_BAND',$
              'FLAG_GALFIT','FITSECT','CONVBOX','NGOOD_GALFIT_BAND','NMASK_GALFIT_BAND',$
              'NITER_GALFIT','NEIGH_GALFIT','CHISQ_GALFIT','CHISQ_GALFIT_PRIME','NFREE_GALFIT',$
              'NFIX_GALFIT','NDOF_GALFIT','NDOF_GALFIT_PRIME','CHI2NU_GALFIT','CHI2NU_GALFIT_PRIME',$
              'FIRSTCON_GALFIT','LASTCON_GALFIT',$
              'CPUTIME_SETUP_GALFIT','CPUTIME_FIT_GALFIT','CPUTIME_TOTAL_GALFIT',$
              'SKY_GALFIT_BAND','SKY_GALFIT_CHEB',$
; fit values
              'X_GALFIT_DEG','X_GALFIT_BAND','XERR_GALFIT_BAND','X_GALFIT_CHEB','XERR_GALFIT_CHEB',$
              'Y_GALFIT_DEG','Y_GALFIT_BAND','YERR_GALFIT_BAND','Y_GALFIT_CHEB','YERR_GALFIT_CHEB',$
              'MAG_GALFIT_DEG','MAG_GALFIT_BAND','MAGERR_GALFIT_BAND','MAG_GALFIT_CHEB','MAGERR_GALFIT_CHEB',$
              'RE_GALFIT_DEG','RE_GALFIT_BAND','REERR_GALFIT_BAND','RE_GALFIT_CHEB','REERR_GALFIT_CHEB' ,$
              'N_GALFIT_DEG','N_GALFIT_BAND' ,'NERR_GALFIT_BAND' ,'N_GALFIT_CHEB','NERR_GALFIT_CHEB',$
              'Q_GALFIT_DEG','Q_GALFIT_BAND','QERR_GALFIT_BAND' ,'Q_GALFIT_CHEB','QERR_GALFIT_CHEB',$
              'PA_GALFIT_DEG','PA_GALFIT_BAND','PAERR_GALFIT_BAND','PA_GALFIT_CHEB','PAERR_GALFIT_CHEB',$
; BD galfit values
              'GALFIT_VERSION_BD','FILE_GALFIT_BD','INITFILE_BD','CONSTRNT_BD','LOGFILE_BD','PSF_GALFIT_BAND_BD',$
              'FLAG_GALFIT_BD',$
              'NITER_GALFIT_BD','NEIGH_GALFIT_BD','CHISQ_GALFIT_BD','CHISQ_GALFIT_BD_PRIME','NFREE_GALFIT_BD',$
              'NFIX_GALFIT_BD','NDOF_GALFIT_BD','NDOF_GALFIT_BD_PRIME','CHI2NU_GALFIT_BD','CHI2NU_GALFIT_BD_PRIME',$
              'FIRSTCON_GALFIT_BD','LASTCON_GALFIT_BD',$
              'CPUTIME_SETUP_GALFIT_BD','CPUTIME_FIT_GALFIT_BD','CPUTIME_TOTAL_GALFIT_BD' ,$
              'SKY_GALFIT_BAND_BD','SKY_GALFIT_CHEB_BD',$
; fit values bulge
              'X_GALFIT_DEG_B','X_GALFIT_BAND_B','XERR_GALFIT_BAND_B' ,'X_GALFIT_CHEB_B','XERR_GALFIT_CHEB_B',$
              'Y_GALFIT_DEG_B','Y_GALFIT_BAND_B','YERR_GALFIT_BAND_B' ,'Y_GALFIT_CHEB_B','YERR_GALFIT_CHEB_B',$
              'MAG_GALFIT_DEG_B','MAG_GALFIT_BAND_B','MAGERR_GALFIT_BAND_B'  ,'MAG_GALFIT_CHEB_B','MAGERR_GALFIT_CHEB_B',$
              'RE_GALFIT_DEG_B','RE_GALFIT_BAND_B','REERR_GALFIT_BAND_B','RE_GALFIT_CHEB_B','REERR_GALFIT_CHEB_B',$
              'N_GALFIT_DEG_B','N_GALFIT_BAND_B','NERR_GALFIT_BAND_B','N_GALFIT_CHEB_B','NERR_GALFIT_CHEB_B',$
              'Q_GALFIT_DEG_B','Q_GALFIT_BAND_B' ,'QERR_GALFIT_BAND_B','Q_GALFIT_CHEB_B','QERR_GALFIT_CHEB_B',$
              'PA_GALFIT_DEG_B','PA_GALFIT_BAND_B','PAERR_GALFIT_BAND_B','PA_GALFIT_CHEB_B','PAERR_GALFIT_CHEB_B',$
; fit values disk
              'X_GALFIT_DEG_D','X_GALFIT_BAND_D','XERR_GALFIT_BAND_D','X_GALFIT_CHEB_D','XERR_GALFIT_CHEB_D',$
              'Y_GALFIT_DEG_D','Y_GALFIT_BAND_D','YERR_GALFIT_BAND_D','Y_GALFIT_CHEB_D','YERR_GALFIT_CHEB_D',$
              'MAG_GALFIT_DEG_D','MAG_GALFIT_BAND_D','MAGERR_GALFIT_BAND_D','MAG_GALFIT_CHEB_D','MAGERR_GALFIT_CHEB_D',$
              'RE_GALFIT_DEG_D','RE_GALFIT_BAND_D','REERR_GALFIT_BAND_D','RE_GALFIT_CHEB_D','REERR_GALFIT_CHEB_D',$
              'N_GALFIT_DEG_D','N_GALFIT_BAND_D' ,'NERR_GALFIT_BAND_D','N_GALFIT_CHEB_D','NERR_GALFIT_CHEB_D',$
              'Q_GALFIT_DEG_D','Q_GALFIT_BAND_D','QERR_GALFIT_BAND_D','Q_GALFIT_CHEB_D','QERR_GALFIT_CHEB_D',$
              'PA_GALFIT_DEG_D','PA_GALFIT_BAND_D','PAERR_GALFIT_BAND_D','PA_GALFIT_CHEB_D','PAERR_GALFIT_CHEB_D']
     
     out2 = reorder_tags(out, order)
     out = out2
     delvarx, out2
; write out catalogue
     print, 'catalogue has '+strtrim(n_elements(out.tile),2)+' objects left'
     mwrfits, out, setup.outdir+setup.cat, /silent, /create
  ENDIF
  d = check_math()
  
  print, 'done with batch file '+setup.batch
  print, 'Start: '+start
  print, 'End  : '+systime(0)
END
