
;Galaxy Analysis over Large Areas: Parameter Assessment by GALFITting
;Objects from SExtractor
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
   WHILE file_test(file) DO file += strtrim(round(randomu(systime(1))*10), 2)
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
         'number':        value[i] = '0'
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
;<-- additional column of type string with label 'FILE'

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

PRO run_sextractor, sexexe, sexparam, zeropoint, image, weight, $
                    cold, coldcat, coldseg, $
                    hot, hotcat, hotseg, enlarge, $
                    outcat, outseg, outparam, check, chktype, exclude, rad, $
                    outonly = outonly
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

;create a temporary parameter file with the minimum set of sextractor
;parameters
   IF file_test(hot) THEN multi = 1 ELSE multi = 0
   IF file_test(cold) THEN multi += 2
   IF multi EQ 0 THEN $
    message, 'Neither cold nor hot SExtractor setup-file found'
   IF multi EQ 1 THEN BEGIN
      cold = hot
      coldcat = hotcat
      coldseg = hotseg
      multi = 2
   ENDIF

   path = strmid(outcat, 0, strpos(outcat, '/', /reverse_search)+1)
   label = required_entries()
   openw, 1, outparam
   FOR i=0ul, n_elements(label)-1 DO printf, 1, strupcase(label[i])
   openr, 2, sexparam
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
   exptime = double(sxpar(header, 'EXPTIME'))
   zp_eff = strtrim(zeropoint+2.5*alog10(exptime), 2)

   print, '---using exptime: '+strtrim(exptime, 2)+'s (zp='+zp_eff+') for: '+ $
          image

;this will produce a checkimage with all the ellipses
   IF chktype NE 'none' THEN BEGIN
      print, 'starting cold sex check image on image '+image+' ''
      spawn, sexexe+' '+image+' -c '+cold+ $
        ' -CATALOG_NAME '+coldcat+' -CATALOG_TYPE ASCII' + $
        ' -PARAMETERS_NAME '+outparam+ $
        ' -WEIGHT_IMAGE '+weight+ $
        ' -WEIGHT_TYPE MAP_WEIGHT -MAG_ZEROPOINT '+zp_eff+ $
        ' -CHECKIMAGE_TYPE '+chktype+' -CHECKIMAGE_NAME '+ $
        file_dirname(check)+'/'+file_basename(check, '.fits')+'.cold.fits'
      IF multi EQ 3 THEN BEGIN
          print, 'starting hot sex check image'
          spawn, sexexe+' '+image+' -c '+hot+ $
            ' -CATALOG_NAME '+hotcat+' -CATALOG_TYPE ASCII' + $
            ' -PARAMETERS_NAME '+outparam+ $
            ' -WEIGHT_IMAGE '+weight+ $
            ' -WEIGHT_TYPE MAP_WEIGHT -MAG_ZEROPOINT '+zp_eff+ $
            ' -CHECKIMAGE_TYPE '+chktype+' -CHECKIMAGE_NAME '+ $
            file_dirname(check)+'/'+file_basename(check, '.fits')+ $
            '.hot.fits'
      ENDIF
  ENDIF
  
;now start sextractor to create hotcat and coldcat
  print, 'starting cold sex'
  spawn, sexexe+' '+image+' -c '+cold+ $
    ' -CATALOG_NAME '+coldcat+' -CATALOG_TYPE ASCII' + $
    ' -PARAMETERS_NAME '+outparam+ $
    ' -WEIGHT_IMAGE '+weight+ $
    ' -WEIGHT_TYPE MAP_WEIGHT -MAG_ZEROPOINT '+zp_eff+ $
    ' -CHECKIMAGE_TYPE segmentation -CHECKIMAGE_NAME '+coldseg
  IF multi EQ 3 THEN BEGIN
      print, 'starting hot sex'
      spawn, sexexe+' '+image+' -c '+hot+ $
        ' -CATALOG_NAME '+hotcat+' -CATALOG_TYPE ASCII' + $
        ' -PARAMETERS_NAME '+outparam+ $
        ' -WEIGHT_IMAGE '+weight+ $
        ' -WEIGHT_TYPE MAP_WEIGHT -MAG_ZEROPOINT '+zp_eff+ $
        ' -CHECKIMAGE_TYPE segmentation -CHECKIMAGE_NAME '+hotseg
  ENDIF
  
;read in hotcat and coldcat
   cold_table = read_sex_table(coldcat, outparam)
   idx = where(cold_table.kron_radius EQ 0, ct)
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
   IF multi EQ 3 THEN BEGIN
      cold_cxx = cold_table.cxx_image / cold_table.kron_radius^2.
      cold_cyy = cold_table.cyy_image / cold_table.kron_radius^2.
      cold_cxy = cold_table.cxy_image / cold_table.kron_radius^2.
      ncold = n_elements(cold_table)

      hot_cxx = hot_table.cxx_image / hot_table.kron_radius^2.
      hot_cyy = hot_table.cyy_image / hot_table.kron_radius^2.
      hot_cxy = hot_table.cxy_image / hot_table.kron_radius^2.
      nhot = n_elements(hot_table)

      fits_read, coldseg, segim, seghd
      fits_read, hotseg, segim_hot, seghd_hot

      FOR i=0ul, ncold-1 DO BEGIN
         idx = where(cold_cxx[i]*(hot_table.x_image- $
                                  cold_table[i].x_image)^2.+ $
                     cold_cyy[i]*(hot_table.y_image- $
                                  cold_table[i].y_image)^2.+ $
                     cold_cxy[i]*(hot_table.x_image-cold_table[i].x_image)* $
                     (hot_table.y_image-cold_table[i].y_image) GT enlarge^2., $
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

      nhot = n_elements(hot_table)
      off = max(cold_table.number)+1
      FOR i=0ul, nhot-1 DO BEGIN
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
      writefits, outseg, segim, seghd
   ENDIF ELSE file_copy, coldseg, outseg, /overwrite

   IF multi EQ 3 THEN table_all = [cold_table, hot_table] $
   ELSE table_all = cold_table

   IF exclude[0, 0] GE 0 THEN BEGIN
      x = reform(exclude[0, *])
      y = reform(exclude[1, *])

      srccor, x, y, table_all.x_image, table_all.y_image, rad, l, e, $
              option = 1, /silent
      e = invert_index(table_all.number, e)
      IF e[0] EQ -1 THEN message, 'No elements left in catalogue!'
      table_all = table_all[e]

      ntot = n_elements(table_all)
      fits_read, outseg, segim_org, seghd
      segim = segim_org*0

      FOR i=0ul, ntot-1 DO BEGIN
         idx = where(segim_org EQ table_all[i].number, ct)
         IF ct GT 0 THEN segim[idx] = i+1
         table_all[i].number = i+1
      ENDFOR

      writefits, outseg, segim, seghd
   ENDIF

   write_sex_table, table_all, outcat

   IF keyword_set(outonly) THEN $
    file_delete, hotcat, coldcat, hotseg, coldseg, /quiet, /allow_nonexistent, /noexpand_path
END

;==============================================================================
FUNCTION calc_dist_to_edge, file, catalogue
;works only on single files and catalogues
;currently efficiency is optimised for 7000*7000 pix images

;get image size to calculate central position
   fits_read, file, im, hd, exten_no=0
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

PRO create_stamp_file, image, sexcat, sexparam, outparam, sizefac
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
      xa = 0
      IF xlo LT 0 THEN BEGIN
         xa = abs(xlo) & xlo = 0
      ENDIF
      xhi = round(cat[i].x_image)+round(xfac)
      IF xhi GT nx-1 THEN xhi = nx-1

      ylo = round(cat[i].y_image)-round(yfac)
      ya = 0
      IF ylo LT 0 THEN BEGIN
         ya = abs(ylo) & ylo = 0
      ENDIF
      yhi = round(cat[i].y_image)+round(yfac)
      IF yhi GT ny-1 THEN yhi = ny-1
      
      printf, 1, cat[i].number, cat[i].x_image, cat[i].y_image, $
              xlo, xhi, ylo, yhi, format = '(I,2(F),4(I))'
   ENDFOR
   close, 1

END

PRO cut_stamps, image, param, outpath, pre, post
;cut postage stamps given an IMAGE (string, including path) and a
;PARAM file (string, including path, created by
;create_stamp_file). Output postage stamps are written into the
;OUTPATH. PRE is a string that gets prepended to the individual
;postage stamp file names (e.g. 'v'+'123.fits' --> 'v123.fits')

   fits_read, image, im, hd
   nx = sxpar(hd, 'NAXIS1')
   ny = sxpar(hd, 'NAXIS2')

;identify number of lines in the param table
;lines beginning with '#' are treated as comments
   nobj = n_lines(param)

   cat = mrd_struct(['id', 'x', 'y', 'xlo', 'xhi', 'ylo', 'yhi'], $
                    ['0L', '0.d0', '0.d0', '0L', '0L', '0L', '0L'], $
                    nobj, /no_execute)

   fill_struct, cat, param

;  ndigits = fix(alog10(max(cat.id)))+1
   FOR i=0ul, nobj-1 DO BEGIN
      hextract, im, hd, out, outhd, $
                cat[i].xlo, cat[i].xhi, cat[i].ylo, cat[i].yhi, /silent
      num = strtrim(cat[i].id, 2)
;    WHILE strlen(num) LT ndigits DO num = '0'+num
      writefits, outpath+pre+num+post+'.fits', out, outhd
   ENDFOR
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

   fits_read, whtfile, wht, hd
   fits_read, segfile, seg, hd

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
      arr = fix(arr) & arr = arr*0 & arr[idx] = 1
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

   writefits, mapfile+'.fits', map, hd

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

   IF ct GT 1 THEN BEGIN        ;more than 1 array has more than 1 element
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
      sec_d = calc_dist_to_edge(images[j], sec)
      n_sec = n_elements(sec.number)
      sec_file = strarr(n_sec)+images[j]
      sec_id = intarr(n_main)+j

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

         idx = invert_index(main.number, nei)
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

   IF ct GT 1 THEN BEGIN        ;more than 1 array has more than 1 element
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
                     nums, frames, fit_table
;power: convert flux_radius to proper half-light radius of a spheroid (1.4)
;t: sextractor table (ASSUMES THAT ALL IMAGE POSITIONS ARE ON THE SAME WCS
;FRAME)
;c: idx of current object id
;cut: magnitude cut (4.5)
;nums: OUTPUT, numbers of the most contributing objects
;frames: OUTPUT, frames of the most contributing objects
  n = t.number*0.+4.
  q = t.b_image/t.a_image
  re = t.flux_radius^power
  mag = t.mag_best
  theta_image = t.theta_world-t[c].theta_world+t[c].theta_image
  IF n_elements(fit_table) GT 0 THEN BEGIN
      
; or try using 'match'
      ident1 = strtrim(fit_table.org_image,2)+':'+strtrim(fit_table.number,2)
      ident2 = strtrim(t.frame,2)+':'+strtrim(t.number,2)
      match, ident1, ident2, id_idx1, id_idx2
      wh_re_gt0 = where(fit_table[id_idx1].re_galfit GE 0,cntregt0)
      if cntregt0 gt 0 then begin
          n[id_idx2[wh_re_gt0]] = fit_table[id_idx1[wh_re_gt0]].n_galfit
          q[id_idx2[wh_re_gt0]] = fit_table[id_idx1[wh_re_gt0]].q_galfit
          re[id_idx2[wh_re_gt0]] = fit_table[id_idx1[wh_re_gt0]].re_galfit
          mag[id_idx2[wh_re_gt0]] = fit_table[id_idx1[wh_re_gt0]].mag_galfit
          theta_image[id_idx2[wh_re_gt0]] = theta_image[id_idx2[wh_re_gt0]]-t[c].theta_image+ $
            90+fit_table[id_idx1[wh_re_gt0]].pa_galfit
      endif
      wh_theta_gt0 = where(theta_image GT 180, cnt_gt)
      if cnt_gt gt 0 then theta_image[wh_theta_gt0] -= 180
      wh_theta_lt0 = where(theta_image LT 180, cnt_lt)
      if cnt_lt gt 0 then theta_image[wh_theta_lt0] += 180
      
;; Marcos original!
;      FOR i=0ul, n_elements(n)-1 DO BEGIN
;          idx = where(fit_table.org_image EQ t[i].frame AND $
;                      fit_table.number EQ t[i].number, ct) 
;          IF ct GT 1 THEN message, 'unexpected error in contrib_sky'
;          IF ct GT 0 THEN BEGIN
;              IF fit_table[idx].re_galfit GE 0 THEN BEGIN
;                  n[i] = fit_table[idx].n_galfit
;                  q[i] = fit_table[idx].q_galfit
;                  re[i] = fit_table[idx].re_galfit
;                  mag[i] = fit_table[idx].mag_galfit
;                  theta_image[i] = theta_image[i]-t[c].theta_image+ $
;                    90+fit_table[idx].pa_galfit
;                  IF theta_image[i] GT 180 THEN theta_image[i] -= 180
;                  IF theta_image[i] LT -180 THEN theta_image[i] += 180
;              ENDIF
;          ENDIF
;      ENDFOR

  ENDIF
;+++++++++++++++++++++++++++++++=  
  ftot = 10.^(-0.4*(mag-zeropt))*exptime
  kap = (kappa(n))[0]
  f0 = ftot/(2*!pi*re^2.*exp(kap)*n*kap^(-2.*n)*gamma(2.*n)*q)
  
;distance from the current object c
   d = sqrt((t[c].x_image-t.x_image)^2.+(t[c].y_image-t.y_image)^2.)
;position angle of the current source in the reference system of all
;other galaxies
   pa_all = rel_pos_ang(t[c].x_image, t[c].y_image, t.x_image, t.y_image, $
                        theta_image/!radeg)
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

   o = sort(c_all)
   no_fit = where(con[o] LT d[o], ct)
   IF ct EQ 0 THEN BEGIN
      nums = -1
      frames = ''
   ENDIF ELSE BEGIN
;      print, 'cur', 'num', 'x', 'y', 'dist', 'con', 'mag', 'magc', $
;             'c_all', 'c_ctr', format = '(2(A5),4(A7),2(A8),2(A6))'
      nno = n_elements(no_fit)-1
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
      
      grad = where(c_all[o[no_fit]] LT cut, ct)
      IF ct GT 0 THEN nums = t[o[no_fit[grad]]].number ELSE nums = -1
      IF ct GT 0 THEN frames = t[o[no_fit[grad]]].frame ELSE frames = ''
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

PRO getsky_loop, current_obj, table, rad, im0, hd, map, exptime, zero_pt, $
                 scale, offset, power, cut, files, psf, dstep, wstep, gap, $
                 nslope, sky_file, out_file, out_cat, out_param, out_stamps, $
                 global_sky, global_sigsky, conv_box, nums, frames, galexe, $
                 fit_table, b
;current_obj: idx of the current object in table
;table: sextractor table (frame of current object and surrounding
;neighbouring frames)
;rad: a*kron radius for each object in table
;im0: image pixel array
;map: skymap pixel array
;exptime, zero_pt, scale, offset, power, cut: parameters for contrib_targets
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
;nums, frames: object numbers and frames of potential contributing sources
;curb: current band index (deblending only decided in reference band)
;compute the sky for a single source

;computation flag:
;0 - done
;1 - image too small for accurate sky calculation (source is too large)
;2 - secondary contributing source (not subtracted) requires radius
;    larger than image
;4 - sky not converged, still decreasing with radius at image boundary
;    (image too small)
;8 - not enough measurements, value from SExtractor taken
;16 - value from contributing source taken
   sky_flag = 0

;reset the image
   im = im0

;get the size of the image
   sz_im = (size(im0))[1:2]
;make sure that the image is large enough to compute the sky
;compute max radius possible in current image
   xarr = (lindgen(sz_im[0], sz_im[1]) MOD sz_im[0])+1
   yarr = (transpose(lindgen(sz_im[1], sz_im[0]) MOD sz_im[1]))+1
   idx = where(map EQ 0, ct)
   IF ct EQ 0 THEN message, 'NO PIXELS TO CALCULATE SKY'
   max_rad = max(sqrt((table[current_obj].x_image-xarr[idx])^2+ $
                      (table[current_obj].y_image-yarr[idx])^2))

;if the maximum possible radius for the image is exceeded -> start
;with 0 and set flag
   IF rad[current_obj] GT max_rad THEN BEGIN
      rad[current_obj] = 0
      sky_flag += 1
   ENDIF

;see if current source has contributors
; this is only done in REFERENCE band, for the other bands, num and
; frames are input by gala_bridge, to which they have been returned in
; the run on the reference band.
;+++++++++++++++++++=
   if b eq 1 then contrib_targets, exptime, zero_pt, scale, offset, power, table, $
     current_obj, cut, nums, frames, fit_table
   
stop
   contrib_sky = 1e30

   IF nums[0] LT 0 THEN BEGIN
;no contributing sources found-------------------------------------------------
;starting radius for sky calculation is already defined
;nothing to be done
   ENDIF ELSE BEGIN
;contributing sources FOUND----------------------------------------------------
; FIX THIS DO BE USING THE SOURCES FOUND IN REFERENCE BAND!
       read_image_files, files, orgim, xxx, outpath, outpath_bandxxx, outpre, $
         nbandxxx,/silent
       delvarx, xxx, outpath_bandxxx, nbandxxx
;       readcol, files, orgim, outpath, outpre, format = 'A,X,A,A', $
;               comment = '#', /silent
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
                       table.frame EQ frames[current_contrib])
; table.frame[1]??
         dist[current_contrib] = $
          sqrt((table[i_con].x_image-table[current_obj].x_image)^2+ $
               (table[i_con].y_image-table[current_obj].y_image)^2)+ $
          table[i_con].a_image*table[i_con].kron_radius*scale

;find the GALFIT output file for the current contributing source
stop
         idx = where(orgim EQ table[i_con].frame)

         objnum = round_digit(table[i_con].number, 0, /str)
         current_contrib_file = outpath[idx]+out_file+objnum+'.fits'

         IF file_test(current_contrib_file) THEN BEGIN
;a GALFIT result exists for the current contributing source--------------------

;read in the GALFIT image fitting results from current_file
            par = read_galfit_sersic(current_contrib_file)
;     0  1  2    3   4  5  6   7
;par=[x, y, mag, re, n, q, pa, sky]

            contrib_sky = [contrib_sky, par[7]]

;subtract the source from the image
;takes 1.5 min
;            print, systime(), ' subtracting contributing source...'

;position is in the original postage stamp frame
            tb = read_sex_table(outpath[idx]+outpre[idx]+out_cat, $
                                outpath[idx]+outpre[idx]+out_param)
            itb = where(tb.number EQ table[i_con].number)

            stamp_file = outpath[idx]+outpre[idx]+out_stamps
            cat = mrd_struct(['id', 'x', 'y', 'xlo', 'xhi', 'ylo', 'yhi'], $
                             ['0L', '0.d0', '0.d0', '0L', '0L', '0L', '0L'], $
                             n_lines(stamp_file), /no_execute)
            fill_struct, cat, stamp_file
            icat = where(cat.id EQ table[i_con].number)

            par[0] = par[0]+cat[icat].xlo-1-tb[itb].x_image+ $
                     table[i_con].x_image
            par[1] = par[1]+cat[icat].ylo-1-tb[itb].y_image+ $
                     table[i_con].y_image

;the total flux of the target
            ftot = 10.^(-0.4*(par[2]-zero_pt))*exptime

            kap = (kappa(par[4]))[0]
            f0 = ftot/(2*!pi*par[3]^2.*exp(kap)*par[4]*kap^(-2.*par[4])* $
                       gamma(2.*par[4])*par[5])

;arrays for radius and angle of current contributing source
            dist_angle, ang_arr, sz_im, par[0], par[1]
            rad_arr = cos(ang_arr)^2.
            ang_arr = sin(temporary(ang_arr))^2./par[5]^2.
            ang_arr = sqrt(rad_arr+temporary(ang_arr))
            dist_ellipse, rad_arr, sz_im, par[0], par[1], 1./par[5], par[6]
            rad_arr = temporary(rad_arr)*ang_arr
            obj = f0*exp(-kap*((rad_arr/par[3])^(1./par[4])-1.))
;        obj = sersic_flux(rad_arr, ang_arr, par[5], f0, par[3], par[4])
            delvarx, rad_arr, ang_arr

;make sure the convolution size is a power of 2
            cs = 2
            cb = conv_box/2
            WHILE cs LT cb DO cs *= 2
            cs -= 1

;convolution box must be fully inside the postage stamp
            x0 = round(par[0]-1) > (cs+1) < (sz_im[0]-cs-2)
            y0 = round(par[1]-1) > (cs+1) < (sz_im[1]-cs-2)

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
;define a square image of sky pixels max 250x250 pix^2 in size
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
               ringsky = mean(skyim)
               ringsigma = 1e30
            ENDIF ELSE BEGIN
;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
               plothist, skyim, x, y, chars = 2, bin = s*3*2/50., /noplot
               f = gaussfit(x, y, a, sigma=sig, nterms = 3)
;oplot, x, f, col = 250
               resistant_mean, skyim, 3, ringsky, ringsigma
;ver, ringsky
;ver, a[1], col = 250
;            print, ringsky, a[1]
               ringsky = a[1]
               ringsigma = sig[1]  ;a[2]/(n_elements(x)^2-1)
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
            min_sky_flag0 = 8
            new_sky = table[current_obj].background
            new_sky_sig = 999
         ENDELSE
         IF new_sky LT min_sky and new_sky_sig lt 1e20 THEN BEGIN
            min_sky_flag = min_sky_flag0
            min_sky = new_sky
            IF ct GT 0 THEN min_sky_rad = mean(sl_rad[idx]) $
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
      new_sky = table[current_obj].background
      new_sky_sig = 999
      fit = [0, 0]
   ENDELSE
   IF ct GT 0 THEN sky_rad = mean(sl_rad[idx]) ELSE sky_rad = radius[(r-1) >0]
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
END

PRO create_mask, table0, wht, seg, paramfile, mask_file, im_file, image, $
                 current, scale, offset, nums, frames, lim_gal, lim_star, $
                 stel_slope, stel_zp, objects, corner
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
            comment = '#', format = 'I,F,F,L,L,L,L', /silent

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

   con_num = 0
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

      IF r1+r2 GT d AND $
       table[i].mag_best LT table[current].mag_best+faintlim THEN BEGIN
;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;loop source has overlap with current --> secondary
         idx = where(arr LE (rad[i])[0] AND (small_mask MOD 2) EQ 0, ct)
         IF ct GT 0 THEN small_mask[idx] += 1
;remember these for the GALFIT start file
         objects = [objects, i]
      ENDIF ELSE BEGIN
;loop source has NO overlap with current --> tertiary
;if loop source is contributing source on current frame, make secondary
         coni = where(table[i].number EQ nums AND table[i].frame EQ frames, $
                      con)
         IF con GT 0 THEN BEGIN
            plus = 1
            con_num = [con_num, nums[coni]]
         ENDIF ELSE plus = 2
         idx = where(arr LE (rad[i])[0] AND small_mask LT 2, ct)
         IF ct GT 0 THEN small_mask[idx] += plus
      ENDELSE

      IF r1+r2 GT d AND table[i].mag_best GE $
       table[current].mag_best+faintlim THEN $
        BEGIN
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
                  table[objects[i]].frame EQ image AND segm GT 0, ct)
      IF ct GT 0 THEN mask1[idx] = 0
   ENDFOR
   FOR i=0ul, n_elements(con_num)-1 DO BEGIN
      idx = where(segm EQ con_num[i] AND segm GT 0, ct)
      IF ct GT 0 THEN mask1[idx] = 0
   ENDFOR
   mask = (mask+mask1) < 1

   writefits, mask_file+'.fits', mask, headfits(im_file+'.fits')

   corner = [pxlo, pylo]
END

PRO prepare_galfit, objects, files, corner, table0, obj_file, im_file, $
                    constr_file, mask_file, psf_file, out_file, sky_file, $
                    conv_box, zero_pt, plate_scl, num_contrib, frame_contrib, $
                    current, out_cat, out_param, out_stamps, conmaxre, $
                    conminm, conmaxm, fit_table, setup_version, $
                    n_constrained = n_constrained

   setup_version = 3

;objects contains an index of secondary sources

;in the case of contributing sources, the TABLE is changed, so keep a
;backup copy
   table = table0

;current is the index of the current object
   hdr = headfits(im_file+'.fits')
   xmax = sxpar(hdr, 'NAXIS1')
   ymax = sxpar(hdr, 'NAXIS2')

;write constraint file for secondaries
   openw, 1, constr_file
   printf, 1, '# Component/    parameter   constraint  Comment'
   printf, 1, '# operation                  values'
   FOR j=2ul, n_elements(objects)+1 DO BEGIN
      IF keyword_set(n_constrained) AND j EQ 2 THEN BEGIN
         lo = strtrim(n_constrained[0], 2)
         hi = strtrim(n_constrained[1], 2)
      ENDIF ELSE BEGIN
         lo = '0.2'
         hi = '8'
      ENDELSE
      printf, 1, j, ' n '+lo+' to '+hi
      printf, 1, j, ' re 0.3 to '+strtrim(conmaxre, 2)
      printf, 1, j, ' q 0.0001  to 1.'
      printf, 1, j, ' mag '+strtrim(conminm, 2)+' '+strtrim(conmaxm, 2)
      printf, 1, j, ' mag 0 to 40'
      printf, 1, j, ' pa -360 to 360'
      printf, 1, j, ' x '+strtrim(-xmax)+' '+strtrim(xmax)
      printf, 1, j, ' y '+strtrim(-ymax)+' '+strtrim(ymax)
   ENDFOR
   close, 1

;write obj file header plus sky
   openw, 1, obj_file
   printf, 1, '# IMAGE PARAMETERS'
   printf, 1, 'A) '+im_file+'.fits'
   printf, 1, 'B) '+out_file+'.fits'
   printf, 1, 'C) none                # Noise image name ' + $
           '(made from data if blank or "none")'
   printf, 1, 'D) '+psf_file+' kernel' + $
           ' # Input PSF image and (optional) diffusion kernel'
   printf, 1, 'E) 1                   ' + $
           '# PSF oversampling factor relative to data'
   printf, 1, 'F) '+mask_file+'.fits'
   printf, 1, 'G) '+constr_file
   printf, 1, 'H) 1 '+strtrim(xmax, 2)+' 1 '+strtrim(ymax, 2)+ $
           '       # Image region to fit (xmin xmax ' + $
           'ymin ymax)'
   printf, 1, 'I) '+round_digit(conv_box, 0, /str)+'   '+ $
           round_digit(conv_box, 0, /str)+ $
           '         # Size of convolution box (x y)'
   printf, 1, 'J) '+round_digit(zero_pt, 4, /str)+ $
           '              # Magnitude photometric zeropoint'
   printf, 1, 'K) '+round_digit(plate_scl, 5, /str)+' '+ $
           round_digit(plate_scl, 5, /str)+'           # Plate scale (dx dy).'
   printf, 1, 'O) regular             # Display type (regular, ' + $
           'curses, both)'
   printf, 1, 'P) 0                   # Create ouput only? (1=yes; ' + $
           '0=optimize)'
   printf, 1, 'S) 0                   # Modify/create objects interactively?'
   printf, 1, ''
   printf, 1, ''
   printf, 1, '# sky'
   printf, 1, ''
   printf, 1, ' 0) sky'
   IF file_test(sky_file) EQ 0 THEN $
    message, 'sky file corresponding to current object was not found'
   openr, 2, sky_file
   readf, 2, sky, dsky, minrad, maxrad, flag
   close, 2
   printf, 1, ' 1) '+round_digit(sky, 3, /string)+'     0       ' + $
           '# sky background       [ADU counts]'
   printf, 1, ' 2) 0.000      0       # dsky/dx (sky gradient in x)'
   printf, 1, ' 3) 0.000      0       # dsky/dy (sky gradient in y)'
   printf, 1, ' Z) 0                  # output image'
   printf, 1, ''
   printf, 1, ''
   close, 1

;find the GALFIT output file for the current contributing source
   num_current = round_digit(table[current].number, 0, /str)
   file_root = strmid(out_file, 0, strlen(out_file)-strlen(num_current))
   file_root = strmid(file_root, strpos(file_root, '/', /reverse_search)+1, $
                      strlen(file_root))

   readcol, files, orgim, outpath, outpre, format = 'A,X,A,A', $
            comment = '#', /silent
   outpath = set_trailing_slash(outpath)

;find the faintest source with proper magnitude measurement (non 99) and
;correct 99 sources
   nn = where(table[objects].mag_best GT 80, n_nn)
   IF n_nn GT 0 THEN BEGIN
      good_mag = where(table[objects].mag_best LT 80, ngood_mag)
      IF ngood_mag EQ 0 THEN table[objects[nn]].mag_best = zero_pt $
      ELSE $
       table[objects[nn]].mag_best = max(table[objects[good_mag]].mag_best)+2
   ENDIF

;loop over all (primary & secondary) sources
   FOR i=0ul, n_elements(objects)-1 DO BEGIN
      idx = where(orgim EQ table[objects[i]].frame, ct)
      secout_file = outpath[idx]+file_root+ $
                    round_digit(table[objects[i]].number, 0, /str)+'.fits'
      IF file_test(secout_file) THEN BEGIN
;sources with existing fit will be included as static source
         par = read_galfit_sersic(secout_file)

;problem: position from GALFIT is relative to original postage
;stamp. Need to compute offset using SExtractor input for that fit
;position is in the original postage stamp frame
         tb = read_sex_table(outpath[idx]+outpre[idx]+out_cat, $
                             outpath[idx]+outpre[idx]+out_param)
         itb = where(tb.number EQ table[objects[i]].number)
         
         stamp_file = outpath[idx]+outpre[idx]+out_stamps
         cat = mrd_struct(['id', 'x', 'y', 'xlo', 'xhi', 'ylo', 'yhi'], $
                          ['0L', '0.d0', '0.d0', '0L', '0L', '0L', '0L'], $
                          n_lines(stamp_file), /no_execute)
         fill_struct, cat, stamp_file
         icat = where(cat.id EQ table[objects[i]].number)
         
         par[0] = par[0]+cat[icat].xlo-1-tb[itb].x_image+ $
                  table[objects[i]].x_image
         par[1] = par[1]+cat[icat].ylo-1-tb[itb].y_image+ $
                  table[objects[i]].y_image
         
         fix = ['0', '0', '0', '0', '0', '0', '0']
      ENDIF ELSE BEGIN
;sources without fit will be included as free fit
         par = [table[objects[i]].x_image, table[objects[i]].y_image, $
                table[objects[i]].mag_best, $
                10.^(-0.79)*table[objects[i]].flux_radius^1.87, $
                2.5, 1-table[objects[i]].ellipticity, $
                table[objects[i]].theta_image-90.]
         fix = ['1', '1', '1', '1', '1', '1', '1']
      ENDELSE

;if the source position is off the frame: fix the position, PA and q
      IF table[objects[i]].x_image-corner[0] LT 1 OR $
       table[objects[i]].x_image-corner[0] GT xmax OR $
       table[objects[i]].y_image-corner[1] LT 1 OR $
       table[objects[i]].y_image-corner[1] GT ymax THEN BEGIN
         fix[0] = '0' & fix[1] = '0' & fix[5] = '0' & fix[6] = '0'
      ENDIF

      IF finite(par[3]) NE 1 THEN par[3] = table[objects[i]].flux_radius > 3
      IF par[3] LT 0 THEN par[3] = table[objects[i]].flux_radius > 3
      par[3] = par[3] < conmaxre > 0.3
      par[4] = par[4] < 8 > 0.2
      par[5] = par[5] > 0.0001 < 1
      par[6] = par[6] > (-360) < 360

;     0  1  2    3   4  5  6   7
;par=[x, y, mag, re, n, q, pa, sky]
      openu, 1, obj_file, /append
      printf, 1, '# Sersic function'
      printf, 1, ''
      printf, 1, ' 0) sersic             # Object type'
      printf, 1, ' 1) '+round_digit(par[0]-corner[0], 2, /string)+ $
              '  '+round_digit(par[1]-corner[1], 2, /string)+ $
              ' '+fix[0]+' '+fix[1]+' # position x, y        [pixel]'
      printf, 1, ' 3) '+round_digit(par[2], 2, /string)+ $
              '       '+fix[2]+'       # total magnitude'
      printf, 1, ' 4) '+round_digit(par[3], 2, /string)+ $
              '       '+fix[3]+'       #     R_e              [Pixels]'
      printf, 1, ' 5) '+round_digit(par[4], 2, /string)+ $
              '       '+fix[4]+'       # Sersic exponent (deVauc=4, expdisk=1)'
      IF setup_version THEN str = ' 9) ' ELSE str = ' 8) '
      printf, 1, str+round_digit(par[5], 4, /string)+ $
              '       '+fix[5]+'       # axis ratio (b/a)'
      IF setup_version THEN str = '10) ' ELSE str = ' 9) '
      printf, 1, str+round_digit(par[6], 2, /string)+ $
              '       '+fix[6]+'       # position angle (PA)  ' + $
              '[Degrees: Up=0, Left=90]'
      printf, 1, ' Z) 0                  # output image (see above)'
      printf, 1, ''
      close, 1
   ENDFOR

;have to include the contributing sources potentially from other
;frames as well.
   n_nums = n_elements(num_contrib)

   IF n_nums EQ 1 AND num_contrib[0] EQ -1 THEN GOTO, finish
   
;find the GALFIT output file for the current contributing source
;  num_current = integer2string(table[current].number, table.number, /array)
   num_current = round_digit(table[current].number, 0, /str)
   file_root = strmid(out_file, 0, strlen(out_file)-strlen(num_current))
   file_root = strmid(file_root, strpos(file_root, '/', /reverse_search)+1, $
                      strlen(file_root))

   readcol, files, orgim, outpath, format = 'A,X,A,X', comment = '#', /silent
   outpath = set_trailing_slash(outpath)

   openr, 1, constr_file
   line = ''
   WHILE NOT eof(1) DO readf, 1, line
   close, 1
   line = strtrim(line, 2)
   line = strmid(line, 0, strpos(line, ' '))
   ctr = fix(line)+1

;loop over all contributing sources
   FOR i=0ul, n_nums-1 DO BEGIN
      i_con = where(table.number EQ num_contrib[i] AND $
                    table.frame EQ frame_contrib[i])

      dum = where(table[objects].number EQ num_contrib[i] AND $
                  table[objects].frame EQ frame_contrib[i], ct)
      IF ct GT 0 THEN CONTINUE

      idx = where(orgim EQ table[i_con].frame)
;    objnum = integer2string(table[i_con].number, table.number, /array)
      objnum = round_digit(table[i_con].number, 0, /str)
      current_contrib_file = outpath[idx]+file_root+objnum+'.fits'

      IF file_test(current_contrib_file) THEN BEGIN
;sources with existing fit will be included as static source
         par = read_galfit_sersic(current_contrib_file)
;problem: position from GALFIT is relative to original postage
;stamp. Need to compute offset using SExtractor input for that fit
         tb = read_sex_table(outpath[idx]+outpre[idx]+out_cat, $
                             outpath[idx]+outpre[idx]+out_param)
         itb = where(tb.number EQ table[i_con].number)
         
         stamp_file = outpath[idx]+outpre[idx]+out_stamps
         cat = mrd_struct(['id', 'x', 'y', 'xlo', 'xhi', 'ylo', 'yhi'], $
                          ['0L', '0.d0', '0.d0', '0L', '0L', '0L', '0L'], $
                          n_lines(stamp_file), /no_execute)
         fill_struct, cat, stamp_file
         icat = where(cat.id EQ table[i_con].number)

         par[0] = par[0]+cat[icat].xlo-1-tb[itb].x_image+table[i_con].x_image
         par[1] = par[1]+cat[icat].ylo-1-tb[itb].y_image+table[i_con].y_image
         
         fix = ['0', '0', '0', '0', '0', '0', '0']
      ENDIF ELSE BEGIN
;the source might be in the fit_table
         i_fit = where(fit_table.number EQ num_contrib[i] AND $
                       fit_table.org_image EQ frame_contrib[i], ct)
         IF ct GT 0 THEN BEGIN
            IF fit_table[i_fit].re_galfit GE 0 THEN BEGIN
               par = [table[i_con].x_image, table[i_con].y_image, $
                      fit_table[i_fit].mag_galfit, $
                      fit_table[i_fit].re_galfit, fit_table[i_fit].n_galfit, $
                      fit_table[i_fit].q_galfit, fit_table[i_fit].pa_galfit]
;if so fixate the fit
               fix = ['0', '0', '0', '0', '0', '0', '0']
            ENDIF ELSE BEGIN
;source is in fit_table but no fit exists -> bombed -> free fit
               par = [table[i_con].x_image, table[i_con].y_image, $
                      table[i_con].mag_best, $
                      10.^(-0.79)*table[i_con].flux_radius^1.87, $
                      2.5, 1-table[i_con].ellipticity, $
                      table[i_con].theta_image-90.]
;the source is off the frame so just fit profile and magnitude
               fix = ['0', '0', '1', '1', '1', '0', '0']
            ENDELSE
         ENDIF ELSE BEGIN
;source is not in fit_table -> free fit
            par = [table[i_con].x_image, table[i_con].y_image, $
                   table[i_con].mag_best, $
                   10.^(-0.79)*table[i_con].flux_radius^1.87, $
                   2.5, 1-table[i_con].ellipticity, $
                   table[i_con].theta_image-90.]
;the source is off the frame so just fit profile and magnitude
            fix = ['0', '0', '1', '1', '1', '0', '0']
         ENDELSE
;else make it a fully free fit (unless the source is in the fit_table:
;then fit only the position, which comes still from SExtractor)
         IF par[0] GT 1 AND par[0] LT xmax-1 AND $
          par[1] GT 1 AND par[1] LT ymax-1 THEN BEGIN
            IF ct GT 0 THEN BEGIN
               IF fit_table[i_fit].re_galfit GE 0 THEN $
                fix = ['1', '1', '0', '0', '0', '0', '0'] $
               ELSE fix = ['1', '1', '1', '1', '1', '1', '1']
            ENDIF ELSE fix = ['1', '1', '1', '1', '1', '1', '1']
         ENDIF
      ENDELSE

      par[3] = par[3] < conmaxre > 0.3
      par[4] = par[4] < 8 > 0.2
      par[5] = par[5] > 0.0001 < 1
      par[6] = par[6] > (-360) < 360
;     0  1  2    3   4  5  6   7
;par=[x, y, mag, re, n, q, pa, sky]
      openu, 1, obj_file, /append
      printf, 1, '# Sersic function'
      printf, 1, ''
      printf, 1, ' 0) sersic             # Object type'
      printf, 1, ' 1) '+round_digit(par[0]-corner[0], 2, /string)+ $
              '  '+round_digit(par[1]-corner[1], 2, /string)+ $
              ' '+fix[0]+' '+fix[1]+' # position x, y        [pixel]'
      printf, 1, ' 3) '+round_digit(par[2], 2, /string)+ $
              '       '+fix[2]+'       # total magnitude'
      printf, 1, ' 4) '+round_digit(par[3], 2, /string)+ $
              '       '+fix[3]+'       #     R_e              [Pixels]'
      printf, 1, ' 5) '+round_digit(par[4], 2, /string)+ $
              '       '+fix[4]+'       # Sersic exponent (deVauc=4, expdisk=1)'
      IF setup_version THEN str = ' 9) ' ELSE str = ' 8) '
      printf, 1, str+round_digit(par[5], 4, /string)+ $
              '       '+fix[5]+'       # axis ratio (b/a)'
      IF setup_version THEN str = '10) ' ELSE str = ' 9) '
      printf, 1, str+round_digit(par[6], 2, /string)+ $
              '       '+fix[6]+'       # position angle (PA)  ' + $
              '[Degrees: Up=0, Left=90]'
      printf, 1, ' Z) 0                  # output image (see above)'
      printf, 1, ''
      close, 1

      dum = where(fix EQ '1', ct)
      IF ct GT 0 THEN BEGIN
;write constraint file
         openu, 1, constr_file, /append
         printf, 1, ctr, ' n '+lo+' to '+hi
         printf, 1, ctr, ' re 0.3 to '+strtrim(conmaxre, 2)
         printf, 1, ctr, ' q 0.0001  to 1.'
         printf, 1, ctr, ' mag '+strtrim(conminm, 2)+' '+strtrim(conmaxm, 2)
         printf, 1, ctr, ' mag 0 to 40'
         printf, 1, ctr, ' pa -360 to 360'
         printf, 1, ctr, ' x '+strtrim(-xmax)+' '+strtrim(xmax)
         printf, 1, ctr, ' y '+strtrim(-ymax)+' '+strtrim(ymax)
         close, 1
      ENDIF
      ctr += 1
   ENDFOR
finish:

END

PRO read_setup, setup_file, setup
;read in the main setup file
;example:


   ON_IOERROR, bad_input

   len_num = 4               ;length of the numbering scheme, e.g. 4 for 'A00)'

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
                         'enlarge', 0.0, $
                         'outcat', '', $
                         'outseg', '', $
                         'outparam', '', $
                         'check', '', $
                         'chktype', '', $
                         'exclude', '', $
                         'exclude_rad', 0.0, $
                         'outonly', 0, $
                         'bad', '', $
                         'sexcomb', '', $
                         'dostamps', 0, $
                         'stampfile', '', $
                         'stamp_pre', strarr(1), $
                         'stampsize', 0.0, $
                         'dosky', 0, $
                         'skymap', '', $
                         'outsky', '', $
                         'skyscl', 0.0, $
                         'neiscl', 0.0, $
                         'skyoff', 0.0, $
                         'dstep', 0, $
                         'wstep', 0, $
                         'gap', 0, $
                         'cut', 0.0, $
                         'nobj_max', 0, $
                         'power', 0.0, $
                         'bright', 0.0, $
                         'nslope', 0, $
                         'stel_slope', 0.0, $
                         'stel_zp', 0.0, $
                         'maglim_gal', 0.0, $
                         'maglim_star', 0.0, $
                         'nneighb', 0, $
                         'max_proc', 0.0, $
                         'min_dist', 0.0, $
                         'galexe', '', $ 
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
                         'docombine', 0, $
                         'cat', '', $
                         'nice', 0, $
                         'version', 0,$
                         'cheb', intarr(7), $
                         'galfit_out_path',' ')

   setup.enlarge = -1
   setup.exclude_rad = -1
   setup.stampsize = -1
   setup.skyscl = -1
   setup.neiscl = -1
   setup.skyoff = -1
   setup.dstep = -1
   setup.wstep = -1
   setup.gap = -1
   setup.stel_slope = 1e20
   setup.stel_zp = 1e20
   setup.maglim_gal = -1
   setup.maglim_star = -1
      
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
         'B15)': setup.exclude = content
         'B16)': setup.exclude_rad = float(content)
         'B17)': setup.outonly = (content EQ 'outonly') ? 1 : 0
         'B18)': setup.bad = content
         'B19)': setup.sexcomb = content

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
         'D12)': setup.bright = float(content)
         'D13)': setup.nslope = fix(content)
         'D14)': setup.stel_slope = float(content)
         'D15)': setup.stel_zp = float(content)
         'D16)': setup.maglim_gal = float(content)
         'D17)': setup.maglim_star = float(content)
         'D18)': setup.nneighb = fix(content)
         'D19)': setup.max_proc = fix(content)
         'D20)': setup.min_dist = float(content)

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
         'E14)': setup.nice = (content EQ 'nice') ? 1 : 0
         'E15)': BEGIN
            setup.version = 1
            IF (pos = stregex(content, '[0123456789]')) GT 0 THEN $
             content = strmid(content, pos, strlen(content)-pos)
            pos = strpos(content, '.')
            content = strrep(content, '.', ' ')
            strput, content, '.', pos
            content = strcompress(content, /remove_all)
            setup.version = (float(content) GE 2.1) ? 1 : 0
         END
         'E16)': BEGIN
             for n=0,5 do begin
                 pos=strpos(content, ',')
                 setup.cheb[n]=strmid(content,0,pos)
                 content=strmid(content,pos+1)
             ENDFOR
             setup.cheb[6]=content
         END
         'E17)': BEGIN
             if content eq '' then setup.galfit_out_path = content
             if content ne '' then setup.galfit_out_path = set_trailing_slash(content)
         END

         'F00)': setup.docombine = (content EQ 'execute') ? 1 : 0
         'F01)': setup.cat = content
      ENDCASE
   ENDWHILE

;default values
   IF setup.enlarge EQ -1 THEN setup.enlarge = 1.1
   IF setup.exclude_rad EQ -1 THEN setup.exclude_rad = 10.
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

   close, 1

   return

bad_input:
   message, 'Invalid Entry in '+setup_file
END

PRO read_image_files, setup, images, weights, outpath, outpath_band, outpre, nband, silent=silent
; reads in the image file and returns the results to galapagos

; count number of columns in file
   lineone = ''
   openr, 1, setup.files
   readf, 1, lineone
   close, 1
   lineone = strtrim(lineone, 2)
   columnsf = strsplit(lineone, ' ', COUNT=ncolf)      

; if number of columns eq 4 then assume 1 band survey and fits files
; in the table
   if ncolf eq 4 then begin
       if not keyword_set(silent) then print, 'assuming one band dataset. Are all files within A00) fits images?'
       readcol, setup.files, images, weights, outpath, outpre, $
         format = 'A,A,A,A', comment = '#', /silent
; doubling the arrays to get [*,0] SExtractor images and [*,1] fitting images
       images=[[images],[images]]
       weights=[[weights],[weights]]
       outpath=[[outpath],[outpath]]
       outpath_band=outpath
       outpre=[[outpre],[outpre]]
; setup.stamp_pre is used and stays as given in C02)
; outpre is read in from the filelist
; exposure time and zeropoint read from setup file
       hlppre=setup.stamp_pre
       setup=remove_tags(setup,'stamp_pre')
       add_tag, setup, 'stamp_pre', [hlppre, hlppre], setup2
       setup=setup2
       delvarx, setup2
       nband=1
; wavelength
; band
   endif 

; if number of columns eq 3 then assume multi band survey and
; filesnames pointing to other file lists that contain all the images.
   if ncolf eq 5 then begin
       if not keyword_set(silent) then print, 'assuming multi-wavelength dataset. Assuming first line to be for SExtractor, rest for fitting!'
       readcol, setup.files, band, wavelength, filelist, zeropoint, exptime, $
         format = 'A,I,A,F,F', comment = '#', /silent      
       nband=fix(n_elements(band)-1)
       readcol, filelist[0], hlpimages, hlpweights, hlpoutpath, hlpoutpre, $
         format = 'A,A,A,A', comment = '#', /silent
       images=strarr(n_elements(hlpimages),nband+1)
       weights=strarr(n_elements(hlpimages),nband+1)
       outpath=strarr(n_elements(hlpimages),nband+1)
       outpath_band=strarr(n_elements(hlpimages),nband+1)
       outpre=strarr(n_elements(hlpimages),nband+1)
       cnt=intarr(nband+1)
       setup=remove_tags(setup,'stamp_pre')
       add_tag, setup, 'stamp_pre', band, setup2
       setup=setup2
       delvarx, setup2
       setup=remove_tags(setup,'zp')
       add_tag, setup, 'zp', zeropoint, setup2
       setup=setup2
       delvarx, setup2
       setup=remove_tags(setup,'expt')
       add_tag, setup, 'expt', exptime, setup2
       setup=setup2
       delvarx, setup2
      for b=0,nband do begin
           readcol, filelist[b], hlpimages, hlpweights, hlpoutpath, hlpoutpre, $
             format = 'A,A,A,A', comment = '#', /silent
           cnt[b]=n_elements(hlpimages)
           if (cnt[b] ne cnt[0]) and not keyword_set(silent) then print, 'input list '+strtrim(band[b])+' contains a wrong number of entries (tiles)'
           if (cnt[b] ne cnt[0]) and not keyword_set(silent) then stop
           images[*,b]=hlpimages
           weights[*,b]=hlpweights
           outpre[*,b]=hlpoutpre
           outpath[*,b]=set_trailing_slash(setup.outdir)+set_trailing_slash(strtrim(hlpoutpath,2))
           outpath_band[*,b]=outpath[*,b]+strtrim(band[b],2)
       endfor 
   endif
   if ncolf ne 5 and ncolf ne 4 then message, 'Invalid Entry in '+setup_file
stop
END

FUNCTION read_sersic_results, obj, psf
   IF file_test(obj) THEN BEGIN
      hd = headfits(obj, exten = 2)
      mag0 = sxpar(hd, '2_MAG')
      mag = float(strmid(mag0, 0, strpos(mag0, '+/-')))
      magerr = float(strmid(mag0, strpos(mag0, '+/-')+3, strlen(mag0)))
      re0 = sxpar(hd, '2_RE')
      re = float(strmid(re0, 0, strpos(re0, '+/-')))
      reerr = float(strmid(re0, strpos(re0, '+/-')+3, strlen(re0)))
      n0 = sxpar(hd, '2_N')
      n = float(strmid(n0, 0, strpos(n0, '+/-')))
      nerr = float(strmid(n0, strpos(n0, '+/-')+3, strlen(n0)))
      q0 = sxpar(hd, '2_AR')
      q = float(strmid(q0, 0, strpos(q0, '+/-')))
      qerr = float(strmid(q0, strpos(q0, '+/-')+3, strlen(q0)))
      pa0 = sxpar(hd, '2_PA')
      pa = float(strmid(pa0, 0, strpos(pa0, '+/-')))
      paerr = float(strmid(pa0, strpos(pa0, '+/-')+3, strlen(pa0)))
      x0 = sxpar(hd, '2_XC')
      x = float(strmid(x0, 0, strpos(x0, '+/-')))
      xerr = float(strmid(x0, strpos(x0, '+/-')+3, strlen(x0)))
      y0 = sxpar(hd, '2_YC')
      y = float(strmid(y0, 0, strpos(y0, '+/-')))
      yerr = float(strmid(y0, strpos(y0, '+/-')+3, strlen(y0)))
      s0 = sxpar(hd, '1_SKY')
      sky = float(strmid(s0, 1, strpos(s0, ']')))
      psf0 = sxpar(hd, 'PSF') 
      psf= strtrim(psf0, 2)
; find number of neighbors
      comp=0
      repeat comp = comp +1 until sxpar(hd, 'COMP_'+strtrim(comp,2)) eq '0'
      neigh_galfit = comp-3
      chisq_galfit = float(strmid(sxpar(hd, 'CHISQ'),2)) 
      ndof_galfit = float(strmid(sxpar(hd, 'NDOF'),2))
      nfree_galfit = float(strmid(sxpar(hd, 'NFREE'),2))
      nfix_galfit = float(strmid(sxpar(hd, 'NFIX'),2))
      chi2nu_galfit = float(strmid(sxpar(hd, 'CHI2NU'),2))
   ENDIF ELSE BEGIN
      mag = -999
      magerr = 99999
      re = -1
      reerr = 99999
      n = -1
      nerr = 99999
      q = -1
      qerr = 99999
      pa = 0
      paerr = 99999
      x = 0
      xerr = 99999
      y = 0
      yerr = 99999
      sky = -999
      neigh_galfit = -99
      chisq_galfit = -99
      ndof_galfit = -99
      nfree_galfit = -99
      nfix_galfit = -99
      chi2nu_galfit = -99
      psf='none'
   ENDELSE
   return, [mag, magerr, re, reerr, n, nerr, q, qerr, pa, paerr, $
            x, xerr, y, yerr, sky, neigh_galfit, chisq_galfit, ndof_galfit, $
            nfree_galfit, nfix_galfit, chi2nu_galfit]
END

;******************************************************************************
;******************************************************************************
PRO update_table, fittab, table, i, out_file
   IF file_test(out_file) THEN BEGIN
      fittab[i].org_image = table[i].frame
      res = read_sersic_results(out_file,psf)
      idx0 = where(finite(res) NE 1, ct)
      IF ct GT 0 THEN res[idx0] = -99999.
      fittab[i].file_galfit = out_file
      fittab[i].x_galfit = res[10]
      fittab[i].xerr_galfit = res[11]
      fittab[i].y_galfit = res[12]
      fittab[i].yerr_galfit = res[13]
      fittab[i].mag_galfit = res[0]
      fittab[i].magerr_galfit = res[1]
      fittab[i].re_galfit = res[2]
      fittab[i].reerr_galfit = res[3]
      fittab[i].n_galfit = res[4]
      fittab[i].nerr_galfit = res[5]
      fittab[i].q_galfit = res[6]
      fittab[i].qerr_galfit = res[7]
      fittab[i].pa_galfit = res[8]
      fittab[i].paerr_galfit = res[9]
      fittab[i].sky_galfit = res[14]
      fittab[i].psf_galfit = psf
      fittab[i].neigh_galfit = res[15]
      fittab[i].chisq_galfit = res[16]
      fittab[i].ndof_galfit = res[17]
      fittab[i].nfree_galfit = res[18]
      fittab[i].nfix_galfit = res[19]
      fittab[i].chi2nu_galfit = res[20]
  ENDIF
END
;******************************************************************************
;******************************************************************************

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

PRO galapagos, setup_file, gala_PRO, logfile=logfile
   IF n_params() LE 1 THEN gala_pro = 'galapagos'
;   gala_pro = '/home/boris/IDL/gala/galapagos.pro'
;   logfile = '/data/gama/galapagos_multi_wlgalapagos.log'
; .run galapagos.pro
;  galapagos,'~/megamorph_dev/astro-megamorph/scripts_boris/megamorph/gala_setup/multi-wl/gala.gama_mwl_1'
;  galapagos,'~/IDL/megamorph/gala_setup/gala.gama_set3.2_test'
;==============================================================================
;main input: location of the setup file
   IF n_params() LT 1 THEN BEGIN
      setup_file = ''
      read, prompt = 'Location of setup file: ', setup_file
   ENDIF
;  setup_file = '/home/boris/megamorph_dev/astro-megamorph/scripts_boris/megamorph/gala_setup/multi-wl/gala.gama_mwl_1'
;==============================================================================
;read in the setup file
  read_setup, setup_file, setup
   IF keyword_set(logfile) THEN $
      start_log, logfile, 'Reading setup file... done!'
;==============================================================================   
;read input files into arrays
   read_image_files, setup, images, weights, outpath, outpath_band, outpre, $
     nband
; old single-band version, will not work anymore, as the above doubnles
; the array even for single band as elemen [*,0] have to be sextractor
; only, no used for fitting!
;   readcol, setup.files, images, weights, outpath, outpre, $
;            format = 'A,A,A,A', comment = '#', /silent

; MULTIBAND
; outpath:      /data/gama/galapagos_multi_wl/tile10_5/
; outpath_galfit: /data/gama/galapagos_multi_wl/tile10_5/galfit
; outpath_band: /data/gama/galapagos_multi_wl/tile10_5/sex/
; outpath_pre:  /data/gama/galapagos_multi_wl/tile10_5/sex/t10_5.
; outpath_file: /data/gama/galapagos_multi_wl/tile10_5/sex/t10_5.sex.
; outpath_file_no_band: /data/gama/galapagos_multi_wl/tile10_5/t10_5.
; 1 BAND
; outpath:      /data/gama/galapagos_multi_wl_test_3.2/tile10_5/
; outpath_galfit: /data/gama/galapagos_multi_wl/tile10_5/
; outpath_band: /data/gama/galapagos_multi_wl_test_3.2/tile10_5/
; outpath_pre:  /data/gama/galapagos_multi_wl_test_3.2/tile10_5/t10_5.
; outpath_file: /data/gama/galapagos_multi_wl_test_3.2/tile10_5/t10_5.v.
   outpath = set_trailing_slash(outpath)
   outpath_galfit = outpath[*,0]+setup.galfit_out_path
   outpath_band = set_trailing_slash(outpath_band)
   outpath_pre = outpath_band+outpre
   outpath_file = outpath
   for q=0,nband do outpath_file[*,q]=outpath_pre[*,q]+strtrim(setup.stamp_pre[q],2)+'.'
   outpath_file_no_band = outpath
   for q=0,nband do outpath_file_no_band[*,q]=outpath[*,q]+outpre[*,q]
;total number of frames
   nframes = n_elements(images[*,0])
;calculate image centres
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
   neighbours = strarr(setup.nneighb, nframes)
   FOR i=0ul, nframes-1 DO BEGIN
      gcirc, 1, ra_cnt[i]/15., dec_cnt[i], ra_cnt/15., dec_cnt, dist
      ord = sort(dist)
      n = setup.nneighb < (n_elements(ord)-1)
      IF n GT 0 THEN neighbours[0:n-1, i] = images[ord[1:n]]
   ENDFOR
;==============================================================================
;check if output path exists
   IF NOT file_test(setup.outdir) THEN $
    spawn, 'mkdirhier '+setup.outdir
   FOR i=0ul, n_elements(outpath_band)-1 DO IF NOT file_test(outpath_band[i]) THEN spawn, 'mkdirhier '+outpath_band[i]
   FOR i=0, n_elements(outpath_galfit)-1 DO IF NOT file_test(outpath_galfit[i]) THEN spawn, 'mkdirhier '+outpath_galfit[i]
IF keyword_set(logfile) THEN $
    update_log, logfile, 'Initialisation... done!'
;===============================================================================
; define the columns that have to be added to the SExtractor catalogue
; old addcol (1 band, single sersic)
;   addcol = [['ORG_IMAGE', '" "'], $
;             ['FILE_GALFIT', '" "'], $
;             ['X_GALFIT', '0.'], ['XERR_GALFIT', '0.'], $
;             ['Y_GALFIT', '0.'], ['YERR_GALFIT', '0.'], $
;             ['MAG_GALFIT', '0.'], ['MAGERR_GALFIT', '0.'], $
;             ['RE_GALFIT', '0.'], ['REERR_GALFIT', '0.'], $
;             ['N_GALFIT', '0.'], ['NERR_GALFIT', '0.'], $
;             ['Q_GALFIT', '0.'], ['QERR_GALFIT', '0.'], $
;             ['PA_GALFIT', '0.'], ['PAERR_GALFIT', '0.'], $
;             ['SKY_GALFIT', '0.'], ['PSF_GALFIT', '" "'], $
;             ['NEIGH_GALFIT', '0'], ['CHISQ_GALFIT','0'], $
;             ['NDOF_GALFIT','0'], ['NFREE_GALFIT','0'], $
;             ['NFIX_GALFIT','0'], ['CHI2NU_GALFIT','0']]

   define_addcol, addcol, nband, setup.stamp_pre, setup.cheb
;==============================================================================
;run SExtractor
   IF setup.dosex THEN BEGIN
      IF file_test(setup.exclude) THEN $
       readcol, setup.exclude, exclude_files, exclude_x, exclude_y, $
                format = 'A,F,F', /silent $
      ELSE exclude_files = ''

      FOR i=0ul, nframes-1 DO BEGIN
         j = where(exclude_files EQ images[i,0], ct)
         IF ct GT 0 THEN $
          exclude = [[transpose(exclude_x[j]), transpose(exclude_y[j])]] $
         ELSE exclude = [[-1, -1]]

        run_sextractor, setup.sexexe, setup.sexout, setup.zp, $
                         images[i,0], weights[i,0], $
                         setup.cold, $
                         outpath_file[i,0]+setup.coldcat, $
                         outpath_file[i,0]+setup.coldseg, $
                         setup.hot, $
                         outpath_file[i,0]+setup.hotcat, $
                         outpath_file[i,0]+setup.hotseg, $
                         setup.enlarge, $
                         outpath_file[i,0]+setup.outcat, $
                         outpath_file[i,0]+setup.outseg, $
                         outpath_file[i,0]+setup.outparam, $
                         outpath_file[i,0]+setup.check, $
                         setup.chktype, exclude, setup.exclude_rad, $
                         outonly = setup.outonly
;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;         sex2ds9reg, outpath_pre[i]+setup.outcat, outpath_pre[i]+ $
;                     setup.outparam, outpath_pre[i]+'reg', 0.5, tag = outpre[i]
;         sex2ds9reg, outpath_pre[i]+setup.outcat, outpath_pre[i]+ $
;                     setup.outparam, outpath_pre[i]+'num.reg', 0.5, $
;                     tag = outpre[i]+'num', /num
;         sex2ds9reg, outpath_pre[i]+setup.coldcat, outpath_pre[i]+ $
;                     setup.outparam, outpath_pre[i]+'cold.reg', 0.5, $
;                     tag = outpre[i]+'cold'
;         sex2ds9reg, outpath_pre[i]+setup.hotcat, outpath_pre[i]+ $
;                     setup.outparam, outpath_pre[i]+'hot.reg', 0.5, $
;                     tag = outpre[i]+'hot'

;         cat = read_sex_table(outpath_pre[i]+setup.outcat, $
;                              outpath_pre[i]+setup.outparam)
;         hot = cat[where(cat.mag_best GE 23)]
;         forprint, 'circle('+strtrim(hot.x_image, 2)+','+ $
;                   strtrim(hot.y_image, 2)+',20) # color=red', $
;                   /nocomment, textout=outpath_pre[i]+'red.reg'
;         cold = cat[where(cat.mag_best LT 23)]
;         forprint, 'circle('+strtrim(cold.x_image, 2)+','+ $
;                   strtrim(cold.y_image, 2)+',20)', $
;                   /nocomment, textout=outpath_pre[i]+'green.reg'
;         openw, 1, outpath_pre[i]+'head.reg'
;         printf, 1, '# Region file format: DS9 version 4.0'
;         printf, 1, 'global color=green font="helvetica 10 normal" ' + $
;                 'select=1 highlite=1 edit=1 move=1 delete=1 include=1 ' + $
;                 'fixed=0 source'
;         printf, 1, 'image'
;         close, 1
;         spawn, 'cat '+outpath_pre[i]+'head.reg '+outpath_pre[i]+ $
;                'green.reg '+ outpath_pre[i]+'red.reg > '+outpath_pre[i]+ $
;                'check.reg'
;         print, 'Number of green sources: ', ncold
;         print, 'Number of  red  sources: ', nhot
;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      ENDFOR

;combine all SExtractor catalogues
      merge_sex_catalogues, outpath_file[*,0]+setup.outcat, $
                            outpath_file[*,0]+setup.outparam, images[*,0], neighbours, $
                            setup.outdir+setup.sexcomb
      sex2ds9reg, setup.outdir+setup.sexcomb, outpath_file[0,0]+setup.outparam, $
                  setup.outdir+'sexcomb.reg', 10, color='red', tag = 'comb'
      IF keyword_set(logfile) THEN $
       update_log, logfile, 'SExtraction... done!'
   ENDIF
;==============================================================================
;create postage stamp description files 
   IF setup.dostamps THEN BEGIN
       FOR i=0ul, nframes-1 DO BEGIN
           create_stamp_file, images[i,0], $
             outpath_file[i,0]+setup.outcat, $
             outpath_file[i,0]+setup.outparam, $
             outpath_file_no_band[i,0]+setup.stampfile, $
             setup.stampsize
           FOR b=1,nband do begin
               cut_stamps, images[i,b], $
                 outpath_file_no_band[i,0]+setup.stampfile, $
                 outpath_band[i,b], $
                 outpre[i,b], '_'+setup.stamp_pre[b]
          ENDFOR
       ENDFOR
;create skymap files 
       FOR i=0ul, nframes-1 DO BEGIN
           FOR b=1,nband do begin
               create_skymap, weights[i,b], $
                 outpath_file[i,0]+setup.outseg, $
                 outpath_file[i,0]+setup.outcat, $
                 outpath_file[i,0]+setup.outparam, $
                 outpath_file_no_band[i,b]+setup.skymap, $
                 setup.skyscl, setup.skyoff
           ENDFOR
       ENDFOR
       IF keyword_set(logfile) THEN $
         update_log, logfile, 'Postage stamps... done!'
   ENDIF 
;==============================================================================
;measure sky and run galfit
   IF setup.dosky THEN BEGIN
       IF keyword_set(logfile) THEN $
        update_log, logfile, 'Beginning sky loop...'
;;==============================================================================
;read in the combined SExtractor table
      sexcat = read_sex_table(setup.outdir+setup.sexcomb, $
                              outpath_file[0,0]+setup.outparam, $
                              add_col = ['frame', '" "'])
;==============================================================================
; check if psf in setup file is an image or a list
       readin_psf_file, setup.psf, sexcat.alpha_j2000, sexcat.delta_j2000, images[*,1:nband], psf_struct, nband
;==============================================================================
;sort the total catalogue by magnitude and select the
;brightest BRIGHT percent
      br = sort(sexcat.mag_best)
      nbr = round(n_elements(sexcat.mag_best)*setup.bright/100.)

      table = sexcat[br]

; make table.frame multi-wavelength-ready to be passed onto gala_bridge
      tableim = strarr(nband+1,n_elements(table.frame))
      for i = 0, n_elements(images[*,0])-1 do begin
          whtableim = where(table.frame eq images[i,0], ct)
          for b=0,nband do begin
              if ct gt 0 then tableim[b,whtableim] = images[i,b]
          ENDFOR
      ENDFOR 
      table=remove_tags(table,'frame')
      add_tag, table, 'frame', strarr(nband+1), table2
      table=table2
      delvarx, table2
      table.frame = tableim

      fittab = read_sex_param(outpath_file[0,0]+setup.outparam, nbr, $
                              add_column = addcol)
      struct_assign, table, fittab
      fittab.mag_galfit = 999
      fittab.re_galfit = -1
      fittab.n_galfit = -1
      fittab.q_galfit = -1

      mwrfits, table, setup.outdir+setup.sexcomb+'.ttmp', /create
;find the image files for the sources
      read_image_files, setup, orgim, orgwht, orgpath, orgpath_band, orgpre, $
        nband,/silent
;      readcol, setup.files, orgim, orgwht, orgpath, orgpre, $
;               format = 'A,A,A,A', comment = '#', /silent
      orgpath = set_trailing_slash(orgpath)
      orgpath_band = set_trailing_slash(orgpath_band)
      orgpath_pre = orgpath_band+orgpre
      orgpath_file = orgpath
      for q=0, nband do orgpath_file[*,q]=orgpath_pre[*,q]+strtrim(setup.stamp_pre[q],2)+'.'
      orgpath_file_no_band = orgpath
      for q=0, nband do orgpath_file_no_band[*,q]=orgpath[*,q]+orgpre[*,q]

;calculate sky for the brightest objects
;==============================================================================
;==============================================================================

;******************************************************************************
;******************************************************************************
;current object
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
      FOR i=0, setup.max_proc-1 DO $
       bridge_arr[i] = obj_new('IDL_IDLBridge')
;       bridge_arr[i] = obj_new('IDL_IDLBridge', output=setup.outdir+'bridge'+ $
;                               strtrim(i, 2)+'.ttmp')

;loop over all objects
      loop = 0l
      REPEAT BEGIN
         IF loop MOD 100000 EQ 0 AND keyword_set(logfile) THEN BEGIN
            update_log, logfile, 'last in cue... '+strtrim(cur, 2)
            FOR i=0, setup.max_proc-1 DO $
             update_log, logfile, 'Bridge status... '+ $
             strtrim(bridge_arr[i]->status(), 2)
         ENDIF
         loop++
           
         statusline, '  currently working on No. '+strtrim(cur+1,2)+' of '+strtrim(n_elements(sexcat),2)+'   '
;check if current object exists
         ct = 0

;         IF cur LT nbr THEN idx = where(table[cur].frame EQ orgim[0,0], ct)
         IF cur LT nbr THEN idx = where(table[cur].frame EQ orgim[*,0], ct)
         IF ct GT 0 THEN BEGIN
            objnum = round_digit(table[cur].number, 0, /str)
            obj_file = (outpath_galfit[idx]+orgpre[idx,0]+objnum+'_'+setup.obj)[0]
            out_file = (outpath_galfit[idx]+orgpre[idx,0]+objnum+'_'+setup.galfit_out)[0]
;check if file was done successfully or bombed
            IF file_test(obj_file) THEN BEGIN
;print, obj_file+' found.'
;print, 'Updating table now! ('+strtrim(cur, 2)+'/'+strtrim(nbr, 1)+')'
               update_table, fittab, table, cur, out_file
               cur++
               IF cur LT nbr THEN CONTINUE
            ENDIF
         ENDIF

;get status of bridge elements
         FOR i=0, setup.max_proc-1 DO bridge_use[i] = bridge_arr[i]->status()
         
;check for free bridges
         free = where(bridge_use eq 0, ct)

         IF ct GT 0 AND cur LT nbr THEN BEGIN
;at least one bridge is free --> start new object
;the available bridge is free[0]
            
;treat finished objects first
            IF bridge_obj[free[0]] GE 0 THEN BEGIN
;read in feedback data
               idx = where(table[bridge_obj[free[0]]].frame EQ orgim[*,0])
               objnum = round_digit(table[bridge_obj[free[0]]].number, 0, /str)
               obj_file = (outpath_galfit[idx]+orgpre[idx,0]+objnum+'_'+setup.obj)[0]
               out_file = (outpath_galfit[idx]+orgpre[idx,0]+objnum+'_'+setup.galfit_out)[0]
;               out_file = (outpath_galfit[idx]+orgpre[idx]+setup.galfit_out+objnum)[0]+'.fits'
;               obj_file = (outpath_galfit[idx]+orgpre[idx]+setup.obj+objnum)[0]

;check if file was done successfully or bombed
               update_table, fittab, table, bridge_obj[free[0]], out_file
;print, 'out file exists -- fittab updated'
;else output file does not exist --> bombed
               
;clear object
               bridge_obj[free[0]] = -1
;clear position of finished object
               bridge_pos[*, free[0]]= [!values.F_NAN, !values.F_NAN]
            ENDIF
            
;check if current position is far enough from bridge positions
; CHANGE ORDER OF OBJECTS HERE!
            filled = where(finite(bridge_pos[0, *]) EQ 1 AND $
                           bridge_use GT 0, ct)
            IF ct GT 0 THEN BEGIN
               gcirc, 1, table[cur].alpha_j2000/15d, table[cur].delta_j2000, $
                      bridge_pos[0, filled]/15d, bridge_pos[1, filled], dist
               IF min(dist) LT setup.min_dist THEN CONTINUE
            ENDIF
            
;store position of new object
            bridge_pos[*, free[0]] = [table[cur].alpha_j2000, $
                                      table[cur].delta_j2000]
            
;find the matching filenames
            idx = where(table[cur].frame EQ orgim[*,0])
 ;define the file names for the:
;postage stamp parameters
            stamp_param_file = (orgpath_file_no_band[idx,0]+setup.stampfile)[0]
            objnum = round_digit(table[cur].number, 0, /str)
;galfit masks
            mask_file = strarr(nband+1)
            for q=1,nband do mask_file[q] = (outpath_galfit[idx,0]+outpre[idx,0]+objnum+'_'+setup.stamp_pre[q]+'_'+setup.mask)[0]
;galfit obj file
            obj_file = (outpath_galfit[idx]+orgpre[idx,0]+objnum+'_'+setup.obj)[0]
;galfit constraint file
            constr_file = (outpath_galfit[idx]+orgpre[idx,0]+objnum+'_'+setup.constr)[0]
;galfit input file
            im_file = strarr(nband+1)
            for q=1,nband do im_file[q] = (orgpath_pre[idx,q]+objnum+'_'+setup.stamp_pre[q])[0]
;galfit output path
            out_file = (outpath[idx]+orgpre[idx,0]+objnum+'_'+setup.galfit_out)[0]
;sky summary file
            sky_file = strarr(nband+1)
            for q=1,nband do sky_file[q] = (outpath_galfit[idx,0]+outpre[idx,0]+objnum+'_'+setup.stamp_pre[q]+'_'+setup.outsky)[0]

; choose closest PSF according to RA & DEC and subtract filename from
; structure 'psf_struct'
; read in chosen_psf into 'psf' (???), filename in chosen_psf_file   
           choose_psf, table[cur].alpha_j2000, table[cur].delta_j2000, $
                        psf_struct, table[cur].frame, chosen_psf_file, nband
 
; create sav file for gala_bridge to read in
           save, cur, orgwht, idx, orgpath, orgpre, setup, chosen_psf_file,$
             sky_file, stamp_param_file, mask_file, im_file, obj_file, $
             constr_file, out_file, fittab, nband, $
             orgpath_band, orgpath_pre, orgpath_file, orgpath_file_no_band, $
             filename=out_file+'.sav'
stop           
            IF setup.max_proc GT 1 THEN BEGIN
                IF keyword_set(logfile) THEN $
                 update_log, logfile, 'Starting new bridge... ('+out_file+')'
; print, 'starting new object at '+systime(0)
               bridge_arr[free[0]]->execute, 'astrolib'
;               bridge_arr[free[0]]->execute, 'cd,"/home/gems/gala"';§§§§§§§§§§
               bridge_arr[free[0]]->execute, '.r '+gala_pro
stop
               bridge_arr[free[0]]->execute, $
                'gala_bridge, "'+out_file+'.sav"', /nowait
            ENDIF ELSE BEGIN
                IF keyword_set(logfile) THEN $
                 update_log, logfile, 'Starting next object... ('+out_file+')'
               cd, orgpath[idx,0]
               gala_bridge, out_file+'.sav'
               file_delete, orgpath[idx]+'galfit.[0123456789]*', /quiet, $
                            /allow_nonexistent, /noexpand_path
            ENDELSE
;++++++++++++++++++++++++++++++++++++=            
            bridge_obj[free[0]] = cur
;switch to next object
            cur++
         ENDIF ELSE BEGIN
;all bridges are busy --> wait    
            wait, 1
         ENDELSE
         
;stop when all done and no bridge in use any more
; CHANGE THIS FOR NEW FITTING ORDER AS WELL!!
      ENDREP UNTIL cur GE nbr AND total(bridge_use) EQ 0

;have to read in the last batch of objects
      remain = where(bridge_obj ge 0, ct)
      IF ct GT 0 THEN BEGIN
         FOR i=0, ct-1 DO BEGIN
            idx = where(table[bridge_obj[remain[i]]].frame EQ orgim)
            objnum = round_digit(table[bridge_obj[remain[i]]].number, 0, /str)
;            out_file = (orgpath[idx]+orgpre[idx]+ $
;                        setup.galfit_out+objnum)[0]+'.fits'
;            obj_file = (orgpath[idx]+orgpre[idx]+setup.obj+objnum)[0]
            obj_file = (outpath_galfit[idx]+orgpre[idx,0]+objnum+'_'+setup.obj)[0]
            out_file = (outpath[idx]+orgpre[idx,0]+objnum+'_'+setup.galfit_out)[0]

;check if file was done successfully or bombed
; CHANGE TO NEW READING IN ROUTINE!!!
            update_table, fittab, table, bridge_obj[remain[i]], out_file
;print, 'out file exists -- fittab updated'
;else output file does not exist --> bombed
         ENDFOR
      ENDIF

;stop
;++++++++++++++++++++++++++++++++++++
;==============================================================================
;==============================================================================
;read in batch list
      IF file_test(setup.batch) AND strlen(setup.batch) GT 0 THEN BEGIN
         readcol, setup.batch, batch, format = 'A', comment = '#', /silent
      ENDIF ELSE batch = ''

;loop over all frames
      FOR f=0ul, nframes-1 DO BEGIN

;check if current frame is in batchlist
         IF n_elements(batch) GT 0 THEN BEGIN
            dum = where(batch EQ images[f], ct)
            IF ct EQ 0 THEN CONTINUE
         ENDIF ELSE BREAK

;select table with frames neighbouring the current frame
         table = sexcat[where(sexcat.frame EQ images[f])]
         FOR i=0ul, n_elements(neighbours[*, f])-1 DO BEGIN
            tabi = where(sexcat.frame EQ neighbours[i, f], ct)
            IF ct GT 0 THEN table = [table, sexcat[tabi]]
         ENDFOR

;sort table by magnitude
         br = sort(table.mag_best)
         table = table[br]

;number of objects in table
         n_obj = n_elements(table)

         first = 1

;calculate sky for all objects in current frame
         FOR current_obj=0ul, n_obj-1 DO BEGIN

;continue if current object is on neighbouring frame
            IF table[current_obj].frame NE images[f] THEN CONTINUE

;find the matching filenames
            idx = where(table[current_obj].frame EQ orgim)

;define the file names for the:
;postage stamp parameters
            stamp_param_file = (orgpath[idx]+orgpre[idx]+setup.stampfile)[0]
            objnum = round_digit(table[current_obj].number, 0, /str)
;galfit masks
            mask_file = (orgpath[idx]+orgpre[idx]+setup.mask+objnum)[0]
;galfit obj file
            obj_file = (orgpath[idx]+orgpre[idx]+setup.obj+objnum)[0]
;galfit constraint file
            constr_file = (orgpath[idx]+orgpre[idx]+setup.constr+objnum)[0]
;galfit input file
            im_file = (orgpath[idx]+orgpre[idx]+setup.stamp_pre+objnum)[0]
;galfit output path
            out_file = (orgpath[idx]+orgpre[idx]+setup.galfit_out+objnum)[0]
;sky summary file
            sky_file = (orgpath[idx]+orgpre[idx]+setup.outsky+objnum)[0]

            IF first THEN BEGIN
               first = 0
;read in image and weight (takes 15sec)
               print, systime(), ' reading image...'
               fits_read, table[current_obj].frame, im, hd
               fits_read, orgwht[idx], wht, whd

               print, systime(), ' done'

;image size
               sz_im = (size(im))[1:2]

;read segmentation map (needed for excluding neighbouring sources)
               fits_read, orgpath[idx]+orgpre[idx]+setup.outseg, seg

;read the skymap
               print, systime(), ' reading skymap...'
               fits_read, orgpath[idx]+orgpre[idx]+setup.skymap+'.fits', map
;takes 5sec
               print, systime(), ' done'

;rad is the minimum starting radius for the sky calculation (outside
;the aperture of the object)
               rad = table.a_image*table.kron_radius*setup.skyscl+setup.skyoff

;get first guess for global sky
;takes 35sec
               print, systime(), ' estimating global sky...'
               skypix = where(map EQ 0, ct)
               resistant_mean, im[skypix], 3, sky, sigsky, nrej
               sigsky *= sqrt(ct-1-nrej)
               par = [1, sky, sigsky, sigsky]
               plothist, im[skypix], x, y, xr = [-1, 1]*5*sigsky+sky, /peak, $
                         /noplot, bin=sigsky/10.
               yfit = curvefit(x, y, noweight, par, sigma, $
                               FUNCTION_name = 'curvefit_gauss2side', $
                               /noderivative, status = status, iter = iter)
               global_sky = par[1]
               global_sigsky = par[2]

               print, systime(), ' done'

;make sure all positions are relative to the current frame
;(neighbouring frames may have negative positions)
               adxy, hd, table.alpha_j2000, table.delta_j2000, x, y

               table.x_image = x+1
               table.y_image = y+1
            ENDIF

;does a GALFIT obj file exist (cannot test for _gf.fits -> files might
;bomb -> endless loop)?
            IF file_test(obj_file) THEN CONTINUE

            statusline, '  currently working on No. '+strtrim(current_obj+1,2)+' of '+strtrim(n_obj,2)+'   '
; choose closest PSF according to RA & DEC and subtract filename from
; structure 'psf_struct'
            choose_psf, table[current_obj].alpha_j2000, table[current_obj].delta_j2000, $
                        psf_struct, table[current_obj].frame, chosen_psf_file            
            fits_read, chosen_psf_file, psf

            getsky_loop, current_obj, table, rad, im, hd, map, setup.expt, $
                         setup.zp, setup.neiscl, setup.skyoff, setup.power, $
                         setup.cut, setup.files, psf, setup.dstep, $
                         setup.wstep, setup.gap, setup.nslope, sky_file, $
                         setup.galfit_out, setup.outcat, setup.outparam, $
                         setup.stampfile, global_sky, global_sigsky, $
                         setup.convbox, nums, frames, setup.galexe, fittab

            create_mask, table, wht, seg, stamp_param_file, mask_file, $
                         im_file, table[current_obj].frame, current_obj, $
                         setup.neiscl, setup.skyoff, nums, frames, $
                         setup.maglim_gal, setup.maglim_star, $
                         setup.stel_slope, setup.stel_zp, objects, corner
            
            prepare_galfit, objects, setup.files, corner, table, $
                            obj_file, sim_file, constr_file, mask_file, $
                            chosen_psf_file, out_file, sky_file, setup.convbox, $
                            setup.zp, setup.platescl, nums, frames, $
                            current_obj, setup.outcat, setup.outparam, $
                            setup.stampfile, setup.conmaxre, setup.conminm, $
                            setup.conmaxm, fittab, setup.version ;, n_constrained = n_constrained
            cd, orgpath[idx]
            print, setup.galexe
            IF setup.nice THEN spawn, 'nice '+setup.galexe+' '+obj_file $
            ELSE spawn, setup.galexe+' '+obj_file
            file_delete, orgpath[idx]+'galfit.[0123456789]*', /quiet, $
                         /allow_nonexistent, /noexpand_path
         ENDFOR
      ENDFOR
      IF n_elements(bridge_arr) GT 0 THEN obj_destroy, bridge_arr
   ENDIF

;==============================================================================
;read in sextractor table, combine with galfit results, write out
;combined fits table
   IF setup.docombine THEN BEGIN
      tab = read_sex_table(setup.outdir+setup.sexcomb, $
                           outpath_pre[0]+setup.outparam, $
                           add_col = ['TILE', '" "'])

      ntab = n_elements(tab)
      out = read_sex_param(outpath_pre[0]+setup.outparam, ntab, $
                           add_column = addcol)
      struct_assign, tab, out

;find the image files for the sources
      readcol, setup.files, orgim, orgwht, orgpath, orgpre, $
               format = 'A,A,A,A', comment = '#', /silent
      orgpath = set_trailing_slash(orgpath)
print,' '
      FOR i=0ul, ntab-1 DO BEGIN
;         IF (i MOD 1000) EQ 0 THEN print, i, ntab
         statusline, ' reading result '+strtrim(i+1,2)+' of '+strtrim(ntab,2)
         objnum = round_digit(tab[i].number, 0, /str)

         idx = where(tab[i].tile EQ orgim)
         out_file = (orgpath[idx]+orgpre[idx]+setup.galfit_out+objnum)[0]+ $
                    '.fits'

         out[i].org_image = tab[i].tile

         res = read_sersic_results(out_file,psf)
         idx = where(finite(res) NE 1, ct)
         IF ct GT 0 THEN res[idx] = -99999.

         out[i].file_galfit = out_file
         out[i].x_galfit = res[10]
         out[i].xerr_galfit = res[11]
         out[i].y_galfit = res[12]
         out[i].yerr_galfit = res[13]
         out[i].mag_galfit = res[0]
         out[i].magerr_galfit = res[1]
         out[i].re_galfit = res[2]
         out[i].reerr_galfit = res[3]
         out[i].n_galfit = res[4]
         out[i].nerr_galfit = res[5]
         out[i].q_galfit = res[6]
         out[i].qerr_galfit = res[7]
         out[i].pa_galfit = res[8]
         out[i].paerr_galfit = res[9]
         out[i].sky_galfit = res[14]
         out[i].psf_galfit = psf
         out[i].neigh_galfit = res[15]
         out[i].chisq_galfit = res[16]
         out[i].ndof_galfit = res[17]
         out[i].nfree_galfit = res[18]
         out[i].nfix_galfit = res[19]
         out[i].chi2nu_galfit = res[20]
      ENDFOR
print, ' '
      IF file_test(setup.bad) THEN BEGIN
         readcol, setup.bad, tile, x, y, format = 'A,F,F', comment = '#', $
                  /silent
         tiles = uniq(tile, sort(tile))
         flag = intarr(n_elements(out))
         FOR i=0ul, n_elements(tiles)-1 DO BEGIN
            print, tile[tiles[i]]
            tileidx = where(tile EQ tile[tiles[i]], ct)
            IF ct GT 0 THEN BEGIN
               catidx = where(out.org_image EQ tile[tiles[i]], ct1)
               IF ct1 GT 0 THEN BEGIN
                  srccor, x[tileidx], y[tileidx], out[catidx].x_image, $
                          out[catidx].y_image, setup.exclude_rad, xy, oi, $
                          option = 1, /silent
                  IF oi[0] GE 0 THEN flag[catidx[oi]] = 1
               ENDIF
            ENDIF
         ENDFOR
         good = where(flag EQ 0, ct)
         IF ct EQ 0 THEN message, 'No objects in output catalogue left' $
         ELSE out = out[good]
      ENDIF

      mwrfits, out, setup.outdir+setup.cat, /silent, /create
   ENDIF
   d = check_math()
;   stop
END
;==============================================================================
;==============================================================================
;==============================================================================

