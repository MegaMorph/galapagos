@/home/boris/megamorph_dev/astro-megamorph/galapagos/mrd_struct.pro
@/home/boris/megamorph_dev/astro-megamorph/galapagos/mrdfits.pro
@/home/boris/megamorph_dev/astro-megamorph/galapagos/mrd_hread.pro
@/home/boris/megamorph_dev/astro-megamorph/galapagos/valid_num.pro
@/home/boris/megamorph_dev/astro-megamorph/galapagos/mwrfits.pro
@/home/boris/megamorph_dev/astro-megamorph/galapagos/fxaddpar.pro
@/home/boris/megamorph_dev/astro-megamorph/galapagos/fxposit.pro
@/home/boris/megamorph_dev/astro-megamorph/galapagos/fxmove.pro
;Galaxy Analysis over Large Areas: Parameter Assessment by GALFITting
;Objects from SExtractor
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
   
   print, '---using exptime: '+strtrim(exptime[0], 2)+'s (zp='+zp_eff[0]+') for: '+ $
     image
   
;this will produce a checkimage with all the ellipses
   IF chktype NE 'none' THEN BEGIN
       print, 'starting cold sex check image on image '+image+' '
       spawn, sexexe+' '+image+' -c '+cold+ $
         ' -CATALOG_NAME '+coldcat+' -CATALOG_TYPE ASCII' + $
         ' -PARAMETERS_NAME '+outparam+ $
         ' -WEIGHT_IMAGE '+weight+ $
         ' -WEIGHT_TYPE MAP_WEIGHT -MAG_ZEROPOINT '+zp_eff[0]+ $
         ' -CHECKIMAGE_TYPE '+chktype+' -CHECKIMAGE_NAME '+ $
         file_dirname(check)+'/'+file_basename(check, '.fits')+'.cold.fits'
       IF multi EQ 3 THEN BEGIN
           print, 'starting hot sex check image'
           spawn, sexexe+' '+image+' -c '+hot+ $
             ' -CATALOG_NAME '+hotcat+' -CATALOG_TYPE ASCII' + $
             ' -PARAMETERS_NAME '+outparam+ $
             ' -WEIGHT_IMAGE '+weight+ $
             ' -WEIGHT_TYPE MAP_WEIGHT -MAG_ZEROPOINT '+zp_eff[0]+ $
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
    ' -WEIGHT_TYPE MAP_WEIGHT -MAG_ZEROPOINT '+zp_eff[0]+ $
    ' -CHECKIMAGE_TYPE segmentation -CHECKIMAGE_NAME '+coldseg
  IF multi EQ 3 THEN BEGIN
      print, 'starting hot sex'
      spawn, sexexe+' '+image+' -c '+hot+ $
        ' -CATALOG_NAME '+hotcat+' -CATALOG_TYPE ASCII' + $
        ' -PARAMETERS_NAME '+outparam+ $
        ' -WEIGHT_IMAGE '+weight+ $
        ' -WEIGHT_TYPE MAP_WEIGHT -MAG_ZEROPOINT '+zp_eff[0]+ $
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
;      fits_read, outseg, segim_org, seghd
      segim_org = readfits(outseg, seghd, /silent)
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

;   fits_read, whtfile, wht, hd
;   fits_read, segfile, seg, hd
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
      
; try using 'match'
      ident1 = strtrim(fit_table.org_image,2)+':'+strtrim(fit_table.number,2)
      ident2 = strtrim(t.frame[0],2)+':'+strtrim(t.number,2)
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
      
;; Marcos original, loops over all objects and is very slow if tiles
;; contain many objects!
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
      IF ct GT 0 THEN frames = t[o[no_fit[grad]]].frame[1] ELSE frames = ''
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
                 global_sky, global_sigsky, conv_box, nums, frames, galexe, $
                 fit_table, b, orgpath_pre, outpath_file, outpath_file_no_band, $
                 nband, xarr, yarr, seed
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
;8 - not enough measurements, value from SExtractor taken
;16 - value from contributing source taken
   sky_flag = 0
   
; rename image so im0 isn't changed
   im = im0

;get the size of the image
   sz_im = (size(im))[1:2]
;make sure that the image is large enough to compute the sky
;compute max radius possible in current image
   idx = where(map EQ 0, ct)
   IF ct EQ 0 THEN message, 'NO PIXELS TO CALCULATE SKY'

; The following line has to be done here, as map (and idx) could be different for each band)
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
; frames are input by gala_bridge/galapagos, to which they have been returned in
; the run on the reference band.
   if b eq 1 then $
     contrib_targets, exptime[b], zero_pt[b], scale, offset, power, table, $
     current_obj, cut, nums, frames, fit_table
   
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
                         table.frame[1] EQ frames[current_contrib])
           dist[current_contrib] = $
             sqrt((table[i_con].x_image-table[current_obj].x_image)^2+ $
                  (table[i_con].y_image-table[current_obj].y_image)^2)+ $
             table[i_con].a_image*table[i_con].kron_radius*scale
           
;find the GALFIT output file for the current contributing source
           idx = where(orgim[*,1] EQ table[i_con].frame[1])
           
           objnum = round_digit(table[i_con].number, 0, /str)
           current_contrib_file = outpath[idx]+outpre[idx,1]+objnum+'_'+setup.galfit_out+'.fits'
           
           IF file_test(current_contrib_file) THEN BEGIN
;a GALFIT result exists for the current contributing source--------------------
               
;read in the GALFIT image fitting results from current_file
forward_function read_sersic_results  ; not quite sure why this is needed
forward_function read_sersic_results_old_galfit  ; not quite sure why this is needed
               IF setup.version ge 4 then par = read_sersic_results(current_contrib_file,nband)
               IF setup.version lt 4 then par = read_sersic_results_old_galfit(current_contrib_file)      
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
                   ringsigma = sig[1] ;a[2]/(n_elements(x)^2-1)
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
         coni = where(table[i].number EQ nums AND table[i].frame[1] EQ frames, $
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
                  table[objects[i]].frame[b] EQ image AND segm GT 0, ct)
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

PRO prepare_galfit, setup, objects, files, corner, table0, obj_file, im_file, $
                    constr_file, mask_file, psf_file, out_file, sky_file, $
                    conv_box, zero_pt, plate_scl, num_contrib, frame_contrib, $
                    current, out_cat, out_param, out_stamps, conmaxre, $
                    conminm, conmaxm, fit_table, setup_version, nband, $
                    outpre
;   setup_version = 4
;objects contains an index of secondary sources
;in the case of contributing sources, the TABLE is changed, so keep a
;backup copy
   table = table0
;current is the index of the current object
   hdr = headfits(im_file[1]+'.fits')
   xmax = sxpar(hdr, 'NAXIS1')
   ymax = sxpar(hdr, 'NAXIS2')

;write constraint file for secondaries
   openw, 1, constr_file
   printf, 1, '# Component/    parameter   constraint  Comment'
   printf, 1, '# operation                  values'
   FOR j=2ul, n_elements(objects)+1 DO BEGIN
       lo = strtrim(setup.conminn, 2)
       hi = strtrim(setup.conmaxn, 2)
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
   openw, 1, obj_file, width=1000
; WRITE FILES & STUFF
   printf, 1, '# IMAGE PARAMETERS'
;   printf, 1, 'A) '+im_file+'.fits'
   A_po=''
   FOR b=1,nband DO BEGIN
       A_po=A_po+strtrim(im_file[b],2)+'.fits'
       if b lt nband then A_po=A_po+','
   ENDFOR
   printf, 1, 'A) '+a_po+'   #input file'
   if setup.version ge 4 then begin
       A1_po=''
       FOR b=1,nband DO BEGIN
           A1_po=A1_po+strtrim(setup.stamp_pre[b],2)
           if b lt nband then A1_po=A1_po+','
       ENDFOR
       printf, 1, 'A1) '+A1_po+ $
         '             # Band labels (can be omitted if fitting a single band)'
       A2_po=''
       FOR b=1,nband DO BEGIN
           A2_po=A2_po+strtrim(setup.wavelength[b],2)
           if b lt nband then A2_po=A2_po+','
       ENDFOR
       printf, 1, 'A2) '+A2_po+ $
         '             # Band wavelengths (choice of wavelength units is arbitrary, as long as consistent)'
   ENDIF
   printf, 1, 'B) '+out_file+'.fits    # output file name'
   printf, 1, 'C) none                # Noise image name ' + $
           '(made from data if blank or "none")'
;   printf, 1, 'D) '+psf_file+' kernel' + $
;           ' # Input PSF image and (optional) diffusion kernel'
   D_po=''
   FOR b=1,nband DO BEGIN
       D_po=D_po+strtrim(psf_file[b],2)
       if b lt nband then D_po=D_po+','
   ENDFOR
   printf, 1, 'D) '+D_po+' kernel' + $
           ' # Input PSF image and (optional) diffusion kernel'
   printf, 1, 'E) 1                   ' + $
           ' # PSF oversampling factor relative to data'
;   printf, 1, 'F) '+mask_file+'.fits'
   F_po=''
   FOR b=1,nband DO BEGIN
       F_po=F_po+strtrim(mask_file[b],2)+'.fits'
       if b lt nband then F_po=F_po+','
   ENDFOR
   printf, 1, 'F) '+F_po
   printf, 1, 'G) '+constr_file
   printf, 1, 'H) 1 '+strtrim(xmax, 2)+' 1 '+strtrim(ymax, 2)+ $
           '       # Image region to fit (xmin xmax ' + $
           'ymin ymax)'
   printf, 1, 'I) '+round_digit(conv_box, 0, /str)+'   '+ $
           round_digit(conv_box, 0, /str)+ $
           '         # Size of convolution box (x y)'
;   printf, 1, 'J) '+round_digit(zero_pt, 4, /str)+ $
;           '              # Magnitude photometric zeropoint'
   J_po=''
   FOR b=1,nband DO BEGIN
       J_po=J_po+strtrim(round_digit(setup.zp[b],4,/str),2)
       if b lt nband then J_po=J_po+','
   ENDFOR
   printf, 1, 'J) '+J_po+ $
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
; WRITE SKY
;   printf, 1, ' 1) '+round_digit(sky, 3, /string)+'     0,       ' + $
;           '# sky background       [ADU counts]'
   SKY_po=''
   SKY_po2=''
   FOR b=1, nband DO BEGIN
       IF file_test(sky_file[b]) EQ 0 THEN $
         message, 'sky file corresponding to current object was not found in band '+strtrim(setup.stamp_pre[b],2)
       openr, 2, sky_file[b]
       readf, 2, sky, dsky, minrad, maxrad, flag
       close, 2
       SKY_po=SKY_po+strtrim(round_digit(sky,3,/string),2)
       SKY_po2=SKY_po2+'0'
       if b lt nband then SKY_po=SKY_po+','
       if b lt nband then SKY_po2=SKY_po2+','
   ENDFOR
;print, SKY_po
   band_po=' '
   if setup.version ge 4 and nband gt 1 then band_po='band'
   printf, 1, ' 1) '+SKY_po+'    '+SKY_po2+'  '+band_po+'       # sky background       [ADU counts]'
   SKYG_po=''
   SKYG_po2=''
;   printf, 1, ' 2) 0.000      0       # dsky/dx (sky gradient in x)'
;   printf, 1, ' 3) 0.000      0       # dsky/dy (sky gradient in y)'
   FOR b=1, nband DO BEGIN
       SKYG_po=SKYG_po+'0.000'
       SKYG_po2=SKYG_po2+'0'
       if b lt nband then SKYG_po=SKYG_po+','
       if b lt nband then SKYG_po2=SKYG_po2+','
   ENDFOR
; add BAND here and there!!!
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
   for q=0,nband do outpath_file[*,q]=outpath_pre[*,q]+strtrim(setup.stamp_pre[q],2)+'.'
   outpath_file_no_band = outpath
   for q=0,nband do outpath_file_no_band[*,q]=outpath[*,q]+outpre[*,q]

;find the GALFIT output file for the current contributing source
   num_current = round_digit(table[current].number, 0, /str)

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
       idx = where(orgim[*,0] EQ table[objects[i]].frame[0], ct)
       secout_file = outpath_galfit[idx]+outpre[idx,1]+ $
         round_digit(table[objects[i]].number, 0, /str)+'_'+strtrim(setup.galfit_out,2)+'.fits'

; for some reason needs this is needed to find read_sersic_results as a
; function, not a variable. Worked before and still works fine in the
; command line.
forward_function read_sersic_results
forward_function read_sersic_results_old_galfit
       IF file_test(secout_file) THEN BEGIN
;sources with existing fit will be included as static source
           IF setup.version ge 4 then par = read_sersic_results(secout_file,nband)
           IF setup.version lt 4 then par = read_sersic_results_old_galfit(secout_file)      

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
          
           par.x_galfit = par.x_galfit+cat[icat].xlo-1-tb[itb].x_image+ $
             table[objects[i]].x_image
           par.x_galfit_band = par.x_galfit_band+cat[icat].xlo-1-tb[itb].x_image+ $
             table[objects[i]].x_image
           par.y_galfit = par.y_galfit+cat[icat].ylo-1-tb[itb].y_image+ $
             table[objects[i]].y_image
           par.y_galfit_band = par.y_galfit_band+cat[icat].ylo-1-tb[itb].y_image+ $
             table[objects[i]].y_image
           
           fix = ['0', '0', '0', '0', '0', '0', '0']

      ENDIF ELSE BEGIN
;sources without fit will be included as free fit

; reading in file works, because it returns the correct FORMAT, but
; with useless values in it. As they will be replaced here, that does
; not matter
forward_function read_sersic_results
forward_function read_sersic_results_old_galfit
           IF setup.version ge 4 then par = read_sersic_results(secout_file,nband)
           IF setup.version lt 4 then par = read_sersic_results_old_galfit(secout_file)      
           par.x_galfit = table[objects[i]].x_image
           par.x_galfit_band = fltarr(nband)+table[objects[i]].x_image
           par.x_galfit_cheb = fltarr(nband)
           par.x_galfit_cheb[0] = table[objects[i]].x_image
           par.y_galfit = table[objects[i]].y_image
           par.y_galfit_band = fltarr(nband)+table[objects[i]].y_image
           par.y_galfit_cheb = fltarr(nband)
           par.y_galfit_cheb[0] = table[objects[i]].y_image
; NEED TO CORRECT MAGNITUDE STARTING PARAMS!
           par.mag_galfit = table[objects[i]].mag_best
           par.mag_galfit_band = fltarr(nband)+table[objects[i]].mag_best+setup.mag_offset[1:nband]
           par.mag_galfit_cheb = fltarr(nband)
           par.mag_galfit_cheb[0] = table[objects[i]].mag_best
           par.re_galfit = 10.^(-0.79)*table[objects[i]].flux_radius^1.87
           par.re_galfit_band = fltarr(nband)+10.^(-0.79)*table[objects[i]].flux_radius^1.87
           par.re_galfit_cheb = fltarr(nband)
           par.re_galfit_cheb[0] = 10.^(-0.79)*table[objects[i]].flux_radius^1.87
           par.n_galfit = 2.5
           par.n_galfit_band = fltarr(nband)+2.5
           par.n_galfit_cheb = fltarr(nband)
           par.n_galfit_cheb[0] = 2.5
           par.q_galfit = 1-table[objects[i]].ellipticity
           par.q_galfit_band = fltarr(nband)+1-table[objects[i]].ellipticity
           par.q_galfit_cheb = fltarr(nband)
           par.q_galfit_cheb[0] = 1-table[objects[i]].ellipticity
           par.pa_galfit = table[objects[i]].theta_image-90.
           par.pa_galfit_band = fltarr(nband)+table[objects[i]].theta_image-90.
           par.pa_galfit_cheb = fltarr(nband)
           par.pa_galfit_cheb[0] = table[objects[i]].theta_image-90.
           
           fix = ['1', '1', '1', '1', '1', '1', '1']
       ENDELSE
       
;if the source position is off the frame: fix the position, PA and q
       IF table[objects[i]].x_image-corner[0] LT 1 OR $
         table[objects[i]].x_image-corner[0] GT xmax OR $
         table[objects[i]].y_image-corner[1] LT 1 OR $
         table[objects[i]].y_image-corner[1] GT ymax THEN BEGIN
           fix[0] = '0' & fix[1] = '0' & fix[5] = '0' & fix[6] = '0'
       ENDIF
       
;;;;;;;;;;;; CONSTRAINTS
; check some constraints and set starting values accordingly if neccessary
       IF finite(par.re_galfit) NE 1 THEN begin
           par.re_galfit = table[objects[i]].flux_radius > 3
           par.re_galfit_band = fltarr(nband)+(table[objects[i]].flux_radius > 3)
           par.re_galfit_cheb = fltarr(nband)
           par.re_galfit_cheb[0] = table[objects[i]].flux_radius > 3
       ENDIF
       
       IF par.re_galfit LT 0 THEN BEGIN
           par.re_galfit = table[objects[i]].flux_radius > 3
           par.re_galfit_band = fltarr(nband)+(table[objects[i]].flux_radius > 3)
           par.re_galfit_cheb = fltarr(nband)
           par.re_galfit_cheb[0] = table[objects[i]].flux_radius > 3    
       ENDIF
       par.re_galfit = par.re_galfit < float(conmaxre) > 0.3
; hard contraints work on ALL bands if this is used below.
       par.re_galfit_band = par.re_galfit_band < float(conmaxre) > 0.3
; hard contraints work only on principle band if this is used
       par.re_galfit_cheb[0] = par.re_galfit_cheb[0] < float(conmaxre) > 0.3
       par.n_galfit = par.n_galfit < setup.conmaxn > setup.conminn
       par.n_galfit_band = par.n_galfit_band < setup.conmaxn > setup.conminn
       par.n_galfit_cheb[0] = par.n_galfit_cheb[0] < setup.conmaxn > setup.conminn
;       par.n_galfit = par.n_galfit < 8 > 0.2
;       par.n_galfit_band = par.n_galfit_band < 8 > 0.2
;       par.n_galfit_cheb[0] = par.n_galfit_cheb[0] < 8 > 0.2
       par.q_galfit = par.q_galfit > 0.0001 < 1
       par.q_galfit_band = par.q_galfit_band > 0.0001 < 1
       par.q_galfit_cheb[0] = par.q_galfit_cheb[0] > 0.0001 < 1
       par.pa_galfit = par.pa_galfit > (-360) < 360
       par.pa_galfit_band = par.pa_galfit_band > (-360) < 360
       par.pa_galfit_cheb[0] = par.pa_galfit_cheb[0] > (-360) < 360

;     0  1  2    3   4  5  6   7
;par=[x, y, mag, re, n, q, pa, sky]
       openu, 1, obj_file, /append
       printf, 1, '# Sersic function'
       printf, 1, ''
       printf, 1, ' 0) sersic             # Object type'

; for different GALFIT versions, fit will work, READOUT of parameters
; will NOT work!
       x_po=''
       x_po_fit=''
       y_po=''
       y_po_fit=''
       FOR b=1,nband DO BEGIN
           x_po = x_po+round_digit(par.x_galfit_band[b-1]-corner[0],2,/string)
           if b lt nband then x_po=x_po+','
           y_po = y_po+round_digit(par.y_galfit_band[b-1]-corner[1],2,/string)
           if b lt nband then y_po=y_po+','
       ENDFOR
       IF fix[0] eq 1 then x_po_fit = strtrim(setup.cheb[0]+1,2) else x_po_fit = '0'
       IF fix[1] eq 1 then y_po_fit = strtrim(setup.cheb[1]+1,2) else y_po_fit = '0'
       
       if setup.version ge 4 then begin
           printf, 1, ' 1) '+x_po+'  '+x_po_fit+'  '+band_po+'   # position x     [pixel]'
           printf, 1, ' 2) '+y_po+'  '+y_po_fit+'  '+band_po+'   # position y     [pixel]'
       endif else begin
           printf, 1, ' 1) '+x_po+'  '+y_po+'   '+x_po_fit+' '+y_po_fit+'   # position x, y        [pixel]'
       endelse
       mag_po=''
       mag_po_fit=''
       FOR b=1,nband DO BEGIN
           mag_po = mag_po+round_digit(par.mag_galfit_band[b-1],2,/string)
           if b lt nband then mag_po=mag_po+','
       ENDFOR
       IF fix[2] eq 1 then mag_po_fit = strtrim(setup.cheb[2]+1,2) else mag_po_fit = '0'
       printf, 1, ' 3) '+mag_po+'    '+mag_po_fit+'   '+band_po+'    # total magnitude'

       re_po=''
       re_po_fit=''
       FOR b=1,nband DO BEGIN
           re_po = re_po+round_digit(par.re_galfit_band[b-1],2,/string)
           if b lt nband then re_po=re_po+','
       ENDFOR
       IF fix[3] eq 1 then re_po_fit = strtrim(setup.cheb[3]+1,2) else re_po_fit = '0'
       printf, 1, ' 4) '+re_po+'    '+re_po_fit+'   '+band_po+'       #     R_e              [Pixels]'

       n_po=''
       n_po_fit=''
       FOR b=1,nband DO BEGIN
           n_po = n_po+round_digit(par.n_galfit_band[b-1],2,/string)
           if b lt nband then n_po=n_po+','
       ENDFOR
       IF fix[4] eq 1 then n_po_fit = strtrim(setup.cheb[4]+1,2) else n_po_fit = '0'
       printf, 1, ' 5) '+n_po+'    '+n_po_fit+'   '+band_po+'       # Sersic exponent (deVauc=4, expdisk=1)'

       q_po=''
       q_po_fit=''
       FOR b=1,nband DO BEGIN
           q_po = q_po+round_digit(par.q_galfit_band[b-1],4,/string)
           if b lt nband then q_po=q_po+','
       ENDFOR
       IF fix[5] eq 1 then q_po_fit = strtrim(setup.cheb[5]+1,2) else q_po_fit = '0'
       IF setup.version eq 0 THEN str = ' 8) ' ELSE str = ' 9) '
       printf, 1, str+q_po+'    '+q_po_fit+'   '+band_po+'       # axis ratio (b/a)'


       pa_po=''
       pa_po_fit=''
       FOR b=1,nband DO BEGIN
           pa_po = pa_po+round_digit(par.pa_galfit_band[b-1],2,/string)
           if b lt nband then pa_po=pa_po+','
       ENDFOR
       IF fix[6] eq 1 then pa_po_fit = strtrim(setup.cheb[6]+1,2) else pa_po_fit = '0'
       IF setup_version eq 0 THEN str = '9) ' ELSE str = '10) '
       printf, 1, str+pa_po+'    '+pa_po_fit+'   '+band_po+'       # position angle (PA) [Degrees: Up=0, Left=90]'

       printf, 1, ' Z) 0                  # output image (see above)'
       printf, 1, ''
       close, 1
   ENDFOR
   
;have to include the contributing sources potentially from other
;frames as well.
   n_nums = n_elements(num_contrib)

   IF n_nums EQ 1 AND num_contrib[0] EQ -1 THEN GOTO, finish
   
;find the GALFIT output file for the current contributing source
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
   
;loop over all contributing sources
   FOR i=0ul, n_nums-1 DO BEGIN

       i_con = where(table.number EQ num_contrib[i] AND $
                     table.frame[0] EQ frame_contrib[i])
       
       dum = where(table[objects].number EQ num_contrib[i] AND $
                   table[objects].frame[0] EQ frame_contrib[i], ct)
       IF ct GT 0 THEN CONTINUE
       
       idx = where(orgim[*,0] EQ table[i_con].frame[0])
;    objnum = integer2string(table[i_con].number, table.number, /array)
       objnum = round_digit(table[i_con].number, 0, /str)
       current_contrib_file = outpath_galfit[idx]+outpre[idx,1]+objnum+'_'+strtrim(setup.galfit_out,2)+'.fits'
       
       IF file_test(current_contrib_file) THEN BEGIN
;sources with existing fit will be included as static source
forward_function read_sersic_results
forward_function read_sersic_results_old_galfit
           IF setup.version ge 4 then par = read_sersic_results(current_contrib_file,nband)
           IF setup.version lt 4 then par = read_sersic_results_old_galfit(current_contrib_file)      
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
           
           par.x_galfit = par.x_galfit+cat[icat].xlo-1-tb[itb].x_image+table[i_con].x_image
           par.x_galfit_band = par.x_galfit_band+cat[icat].xlo-1-tb[itb].x_image+ $
             table[i_con].x_image
           par.y_galfit = par.y_galfit+cat[icat].ylo-1-tb[itb].y_image+table[i_con].y_image
           par.y_galfit_band = par.y_galfit_band+cat[icat].ylo-1-tb[itb].y_image+ $
             table[i_con].y_image
           
           fix = ['0', '0', '0', '0', '0', '0', '0']
       ENDIF ELSE BEGIN
;the source might be in the fit_table
           i_fit = where(fit_table.number EQ num_contrib[i] AND $
                         fit_table.org_image EQ frame_contrib[i], ct)
           IF ct GT 0 THEN BEGIN
               IF fit_table[i_fit].re_galfit GE 0 THEN BEGIN
                   
; reading in file works, because it returns the correct FORMAT, but
; with useless values in it. As they will be replaced here, that does
; not matter
forward_function read_sersic_results
forward_function read_sersic_results_old_galfit
                   IF setup.version ge 4 then par = read_sersic_results(secout_file,nband)
                   IF setup.version lt 4 then par = read_sersic_results_old_galfit(secout_file)      
; replace correctly!
                   par.x_galfit = table[i_con].x_image
                   par.x_galfit_band = fltarr(nband)+table[i_con].x_image
                   par.x_galfit_cheb = fltarr(nband)
                   par.x_galfit_cheb[0] = table[i_con].x_image
                   par.y_galfit = table[i_con].y_image
                   par.y_galfit_band = fltarr(nband)+table[i_con].y_image
                   par.y_galfit_cheb = fltarr(nband)
                   par.y_galfit_cheb[0] = table[i_con].y_image
; NEED TO CORRECT MAGNITUDE STARTING PARAMS!
                   par.mag_galfit = fit_table[i_fit].mag_galfit
                   par.mag_galfit_band = fit_table[i_fit].mag_galfit_band
                   par.mag_galfit_cheb = fit_table[i_fit].mag_galfit_cheb
                   par.re_galfit = fit_table[i_fit].re_galfit
                   par.re_galfit_band = fit_table[i_fit].re_galfit_band
                   par.re_galfit_cheb = fit_table[i_fit].re_galfit_cheb
                   par.n_galfit = fit_table[i_fit].n_galfit
                   par.n_galfit_band = fit_table[i_fit].n_galfit_band
                   par.n_galfit_cheb = fit_table[i_fit].n_galfit_cheb
                   par.q_galfit = fit_table[i_fit].q_galfit
                   par.q_galfit_band = fit_table[i_fit].q_galfit_band
                   par.q_galfit_cheb = fit_table[i_fit].q_galfit_cheb
                   par.pa_galfit = fit_table[i_fit].pa_galfit
                   par.pa_galfit_band = fit_table[i_fit].pa_galfit_band
                   par.pa_galfit_cheb = fit_table[i_fit].pa_galfit_cheb
                   
;               par = [table[i_con].x_image, table[i_con].y_image, $
;                      fit_table[i_fit].mag_galfit, $
;                      fit_table[i_fit].re_galfit, fit_table[i_fit].n_galfit, $
;                      fit_table[i_fit].q_galfit, fit_table[i_fit].pa_galfit]
;if so fixate the fit
                   fix = ['0', '0', '0', '0', '0', '0', '0']
               ENDIF ELSE BEGIN
;source is in fit_table but no fit exists -> bombed -> free fit
forward_function read_sersic_results
forward_function read_sersic_results_old_galfit
                   IF setup.version ge 4 then par = read_sersic_results(secout_file,nband)
                   IF setup.version lt 4 then par = read_sersic_results_old_galfit(secout_file)      
                   
                   par.x_galfit = table[i_con].x_image
                   par.x_galfit_band = fltarr(nband)+table[i_con].x_image
                   par.x_galfit_cheb = fltarr(nband)
                   par.x_galfit_cheb[0] = table[i_con].x_image
                   par.y_galfit = table[i_con].y_image
                   par.y_galfit_band = fltarr(nband)+table[i_con].y_image
                   par.y_galfit_cheb = fltarr(nband)
                   par.y_galfit_cheb[0] = table[i_con].y_image
; NEED TO CORRECT MAGNITUDE STARTING PARAMS!
                   par.mag_galfit = table[i_con].mag_best
                   par.mag_galfit_band = fltarr(nband)+table[i_con].mag_best+setup.mag_offset[1:nband]
                   par.mag_galfit_cheb = fltarr(nband)
                   par.mag_galfit_cheb[0] = table[i_con].mag_best
                   par.re_galfit = 10.^(-0.79)*table[i_con].flux_radius^1.87
                   par.re_galfit_band = fltarr(nband)+10.^(-0.79)*table[i_con].flux_radius^1.87
                   par.re_galfit_cheb = fltarr(nband)
                   par.re_galfit_cheb[0] = 10.^(-0.79)*table[i_con].flux_radius^1.87
                   par.n_galfit = 2.5
                   par.n_galfit_band = fltarr(nband)+2.5
                   par.n_galfit_cheb = fltarr(nband)
                   par.n_galfit_cheb[0] = 2.5
                   par.q_galfit = 1-table[i_con].ellipticity
                   par.q_galfit_band = fltarr(nband)+1-table[i_con].ellipticity
                   par.q_galfit_cheb = fltarr(nband)
                   par.q_galfit_cheb[0] = 1-table[i_con].ellipticity
                   par.pa_galfit = table[i_con].theta_image-90.
                   par.pa_galfit_band = fltarr(nband)+table[i_con].theta_image-90.
                   par.pa_galfit_cheb = fltarr(nband)
                   par.pa_galfit_cheb[0] = table[i_con].theta_image-90.
                   
;               par = [table[i_con].x_image, table[i_con].y_image, $
;                      table[i_con].mag_best, $
;                      10.^(-0.79)*table[i_con].flux_radius^1.87, $
;                      2.5, 1-table[i_con].ellipticity, $
;                      table[i_con].theta_image-90.]
;the source is off the frame so just fit profile and magnitude, position fixed
                   fix = ['0', '0', '1', '1', '1', '0', '0']
               ENDELSE
           ENDIF ELSE BEGIN
;source is not in fit_table -> free fit
forward_function read_sersic_results
forward_function read_sersic_results_old_galfit
               IF setup.version ge 4 then par = read_sersic_results(secout_file,nband)
               IF setup.version lt 4 then par = read_sersic_results_old_galfit(secout_file)      
               
               par.x_galfit = table[i_con].x_image
               par.x_galfit_band = fltarr(nband)+table[i_con].x_image
               par.x_galfit_cheb = fltarr(nband)
               par.x_galfit_cheb[0] = table[i_con].x_image
               par.y_galfit = table[i_con].y_image
               par.y_galfit_band = fltarr(nband)+table[i_con].y_image
               par.y_galfit_cheb = fltarr(nband)
               par.y_galfit_cheb[0] = table[i_con].y_image
; NEED TO CORRECT MAGNITUDE STARTING PARAMS!
               par.mag_galfit = table[i_con].mag_best
               par.mag_galfit_band = fltarr(nband)+table[i_con].mag_best+setup.mag_offset[1:nband]
               par.mag_galfit_cheb = fltarr(nband)
               par.mag_galfit_cheb[0] = table[i_con].mag_best
               par.re_galfit = 10.^(-0.79)*table[i_con].flux_radius^1.87
               par.re_galfit_band = fltarr(nband)+10.^(-0.79)*table[i_con].flux_radius^1.87
               par.re_galfit_cheb = fltarr(nband)
               par.re_galfit_cheb[0] = 10.^(-0.79)*table[i_con].flux_radius^1.87
               par.n_galfit = 2.5
               par.n_galfit_band = fltarr(nband)+2.5
               par.n_galfit_cheb = fltarr(nband)
               par.n_galfit_cheb[0] = 2.5
               par.q_galfit = 1-table[i_con].ellipticity
               par.q_galfit_band = fltarr(nband)+1-table[i_con].ellipticity
               par.q_galfit_cheb = fltarr(nband)
               par.q_galfit_cheb[0] = 1-table[i_con].ellipticity
               par.pa_galfit = table[i_con].theta_image-90.
               par.pa_galfit_band = fltarr(nband)+table[i_con].theta_image-90.
               par.pa_galfit_cheb = fltarr(nband)
               par.pa_galfit_cheb[0] = table[i_con].theta_image-90.
               
;               par = [table[i_con].x_image, table[i_con].y_image, $
;                      table[i_con].mag_best, $
;                      10.^(-0.79)*table[i_con].flux_radius^1.87, $
;                      2.5, 1-table[i_con].ellipticity, $
;                      table[i_con].theta_image-90.]
;the source is off the frame so just fit profile and magnitude
               fix = ['0', '0', '1', '1', '1', '0', '0']
           ENDELSE
;else make it a fully free fit (unless the source is in the fit_table:
;then fit only the position, which comes still from SExtractor)
           IF par.x_galfit GT 1 AND par.x_galfit LT xmax-1 AND $
             par.y_galfit GT 1 AND par.y_galfit LT ymax-1 THEN BEGIN
               IF ct GT 0 THEN BEGIN
                   IF fit_table[i_fit].re_galfit GE 0 THEN $
                     fix = ['1', '1', '0', '0', '0', '0', '0'] $
                   ELSE fix = ['1', '1', '1', '1', '1', '1', '1']
               ENDIF ELSE fix = ['1', '1', '1', '1', '1', '1', '1']
           ENDIF
       ENDELSE
       
       par.re_galfit = par.re_galfit < conmaxre > 0.3
; hard contraints work on ALL bands if this is used below. USED!!
       par.re_galfit_band = par.re_galfit_band < conmaxre > 0.3
; hard contraints work only on principle band if this is used
       par.re_galfit_cheb[0] = par.re_galfit_cheb[0] < conmaxre > 0.3
       par.n_galfit = par.n_galfit < setup.conmaxn > setup.conminn
       par.n_galfit_band = par.n_galfit_band < setup.conmaxn > setup.conminn
       par.n_galfit_cheb[0] = par.n_galfit_cheb[0] < setup.conmaxn > setup.conminn
;       par.n_galfit = par.n_galfit < 8 > 0.2
;       par.n_galfit_band = par.n_galfit_band < 8 > 0.2
;       par.n_galfit_cheb[0] = par.n_galfit_cheb[0] < 8 > 0.2
       par.q_galfit = par.q_galfit > 0.0001 < 1
       par.q_galfit_band = par.q_galfit_band > 0.0001 < 1
       par.q_galfit_cheb[0] = par.q_galfit_cheb[0] > 0.0001 < 1
       par.pa_galfit = par.pa_galfit > (-360) < 360
       par.pa_galfit_band = par.pa_galfit_band > (-360) < 360
       par.pa_galfit_cheb[0] = par.pa_galfit_cheb[0] > (-360) < 360

;     0  1  2    3   4  5  6   7
;par=[x, y, mag, re, n, q, pa, sky]
      openu, 1, obj_file, /append
      printf, 1, '# Sersic function'
      printf, 1, ''
      printf, 1, ' 0) sersic             # Object type'
; for different GALFIT versions, fit will work, READOUT of parameters
; will NOT work!
       band_po=' '
       if setup.version ge 4 and nband gt 1 then band_po='band'

       x_po=''
       x_po_fit=''
       y_po=''
       y_po_fit=''
       FOR b=1,nband DO BEGIN
           x_po = x_po+round_digit(par.x_galfit_band[b-1]-corner[0],2,/string)
           if b lt nband then x_po=x_po+','
           y_po = y_po+round_digit(par.y_galfit_band[b-1]-corner[1],2,/string)
           if b lt nband then y_po=y_po+','
       ENDFOR
       IF fix[0] eq 1 then x_po_fit = strtrim(setup.cheb[0]+1,2) else x_po_fit = '0'
       IF fix[1] eq 1 then y_po_fit = strtrim(setup.cheb[1]+1,2) else y_po_fit = '0'
       
       if setup.version ge 4 then begin
           printf, 1, ' 1) '+x_po+'  '+x_po_fit+'  '+band_po+'   # position x     [pixel]'
           printf, 1, ' 2) '+y_po+'  '+y_po_fit+'  '+band_po+'   # position y     [pixel]'
       endif else begin
           printf, 1, ' 1) '+x_po+'  '+y_po+'   '+x_po_fit+' '+y_po_fit+'   # position x, y        [pixel]'
       endelse

       mag_po=''
       mag_po_fit=''
       FOR b=1,nband DO BEGIN
           mag_po = mag_po+round_digit(par.mag_galfit_band[b-1],2,/string)
           if b lt nband then mag_po=mag_po+','
       ENDFOR
       IF fix[2] eq 1 then mag_po_fit = strtrim(setup.cheb[2]+1,2) else mag_po_fit = '0'
       printf, 1, ' 3) '+mag_po+'    '+mag_po_fit+'   '+band_po+'    # total magnitude'

       re_po=''
       re_po_fit=''
       FOR b=1,nband DO BEGIN
           re_po = re_po+round_digit(par.re_galfit_band[b-1],2,/string)
           if b lt nband then re_po=re_po+','
       ENDFOR
       IF fix[3] eq 1 then re_po_fit = strtrim(setup.cheb[3]+1,2) else re_po_fit = '0'
       printf, 1, ' 4) '+re_po+'    '+re_po_fit+'   '+band_po+'       #     R_e              [Pixels]'

       n_po=''
       n_po_fit=''
       FOR b=1,nband DO BEGIN
           n_po = n_po+round_digit(par.n_galfit_band[b-1],2,/string)
           if b lt nband then n_po=n_po+','
       ENDFOR
       IF fix[4] eq 1 then n_po_fit = strtrim(setup.cheb[4]+1,2) else n_po_fit = '0'
       printf, 1, ' 5) '+n_po+'    '+n_po_fit+'   '+band_po+'       # Sersic exponent (deVauc=4, expdisk=1)'

       q_po=''
       q_po_fit=''
       FOR b=1,nband DO BEGIN
           q_po = q_po+round_digit(par.q_galfit_band[b-1],4,/string)
           if b lt nband then q_po=q_po+','
       ENDFOR
       IF fix[5] eq 1 then q_po_fit = strtrim(setup.cheb[5]+1,2) else q_po_fit = '0'
       IF setup.version eq 0 THEN str = ' 8) ' ELSE str = ' 9) '
       printf, 1, str+q_po+'    '+q_po_fit+'   '+band_po+'       # axis ratio (b/a)'


       pa_po=''
       pa_po_fit=''
       FOR b=1,nband DO BEGIN
           pa_po = pa_po+round_digit(par.pa_galfit_band[b-1],2,/string)
           if b lt nband then pa_po=pa_po+','
       ENDFOR
       IF fix[6] eq 1 then pa_po_fit = strtrim(setup.cheb[6]+1,2) else pa_po_fit = '0'
       IF setup_version eq 0 THEN str = '9) ' ELSE str = '10) '
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
   if file_test(setup_file) eq 0 then begin
       print, 'input file does not exist'
       stop
   ENDIF

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
                         'enlarge', -1., $
                         'outcat', '', $
                         'outseg', '', $
                         'outparam', '', $
                         'check', '', $
                         'chktype', '', $
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
                         'bright', 0.0, $
                         'nslope', 0, $
                         'stel_slope', 0.0, $
                         'stel_zp', 0.0, $
                         'maglim_gal', -1, $
                         'maglim_star', -1., $
                         'nneighb', 0, $
                         'max_proc', 0.0, $
                         'min_dist', 0.0, $
                         'min_dist_block', -1., $
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
                         'conminn', 0.2, $
                         'conmaxn', 8.0, $
                         'nice', 0, $
                         'version', 0,$
                         'cheb', intarr(7)-1, $
                         'galfit_out_path',' ', $
                         'dobd', 0, $
                         'bd_maglim', -1., $
                         'docombine', 0, $
                         'cat', '')

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
;   for n=0,n_elements(setup.cheb)-1 do setup.cheb[n] = -1
   setup.galfit_out_path = ''

; check format for backwards compatibility
   limitn = 0
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
       IF strupcase(strmid(line, 0, len_num)) eq 'E19)' then limitn = 1
   ENDWHILE
   close, 1

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
         'D21)': setup.min_dist_block = float(content)

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
         'E14)': BEGIN
             if limitn eq 1 then setup.conminn = float(content) $
               else setup.nice = (content EQ 'nice') ? 1 : 0
         END
         'E15)': BEGIN
             if limitn eq 1 then setup.conmaxn = float(content) $
             else begin
                 setup.version = 1
                 IF (pos = stregex(content, '[0123456789]')) GT 0 THEN $
                   content = strmid(content, pos, strlen(content)-pos)
                 pos = strpos(content, '.')
                 content = strrep(content, '.', ' ')
                 strput, content, '.', pos
                 content = strcompress(content, /remove_all)
                 if float(content) lt 2.1 then setup.version = 0
                 if (float(content) GE 2.1 and float(content) lt 4) then setup.version = 1
                 if float(content) GE 4.0 then setup.version = 4
             endelse
         END
         'E16)': BEGIN
             if limitn eq 1 then setup.nice = (content EQ 'nice') ? 1 : 0 $
             else begin
                 for n=0,5 do begin
                     pos=strpos(content, ',')
                     setup.cheb[n]=strmid(content,0,pos)
                     content=strmid(content,pos+1)
                 ENDFOR
                 setup.cheb[6]=content
             endelse
         END
         'E17)': BEGIN
             if limitn eq 1 then begin
                 setup.version = 1
                 IF (pos = stregex(content, '[0123456789]')) GT 0 THEN $
                   content = strmid(content, pos, strlen(content)-pos)
                 pos = strpos(content, '.')
                 content = strrep(content, '.', ' ')
                 strput, content, '.', pos
                 content = strcompress(content, /remove_all)
                 if float(content) lt 2.1 then setup.version = 0
                 if (float(content) GE 2.1 and float(content) lt 4) then setup.version = 1
                 if float(content) GE 4.0 then setup.version = 4
             endif else begin
                 if content eq '' then setup.galfit_out_path = content
                 if content ne '' then setup.galfit_out_path = set_trailing_slash(content)
             endelse
         END
         'E18)': BEGIN
             for n=0,5 do begin
                 pos=strpos(content, ',')
                 setup.cheb[n]=strmid(content,0,pos)
                 content=strmid(content,pos+1)
             ENDFOR
             setup.cheb[6]=content
         END
         'E19)': BEGIN
                 if content eq '' then setup.galfit_out_path = content
                 if content ne '' then setup.galfit_out_path = set_trailing_slash(content)
             END

         'F00)': BEGIN
             if block_bd eq 1 then setup.dobd = (content EQ 'execute') ? 1 : 0 $
             else setup.docombine = (content EQ 'execute') ? 1 : 0
         END
         'F01)': BEGIN
             if block_bd eq 1 then setup.bd_maglim = content $
             else setup.cat = content
         END

         'G00)': setup.docombine = (content EQ 'execute') ? 1 : 0
         'G01)': setup.cat = content
     ENDCASE
   ENDWHILE
   close, 1
; Ensure backwards compatibility for D21, E18, E19. They don't exist
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
   return

bad_input:
   message, 'Invalid Entry in '+setup_file
END

PRO read_image_files, setup, silent=silent
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
       
; read the image names and data like zeropoints, mag_offsets,...
; stored in setup structure
       readcol, setup.files, images, weights, outpath, outpre, $
         format = 'A,A,A,A', comment = '#', /silent

       nband=1
; create arrays in setup needed to store all the data
       add_tag, setup, 'images', strarr(n_elements(images),nband+1), setup2
       setup=setup2
       add_tag, setup, 'weights', strarr(n_elements(images),nband+1), setup2
       setup=setup2
       add_tag, setup, 'outpath', strarr(n_elements(images),nband+1), setup2
       setup=setup2
       add_tag, setup, 'outpath_band', strarr(n_elements(images),nband+1), setup2
       setup=setup2
       add_tag, setup, 'outpre', strarr(n_elements(images),nband+1), setup2
       setup=setup2
       add_tag, setup, 'nband', nband, setup2
       setup=setup2

; doubling the arrays to get [*,0] SExtractor images and [*,1] fitting images
       setup.images = [[images],[images]]
       setup.weights = [[weights],[weights]]
       setup.outpath = [[outpath],[outpath]]
       setup.outpath_band = setup.outpath
       setup.outpre = [[outpre],[outpre]]

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

; if number of columns eq 3 then assume multi band survey and
; filesnames pointing to other file lists that contain all the images.
   if ncolf eq 6 then begin
       if not keyword_set(silent) then print, 'assuming multi-wavelength dataset. Assuming first line to be for SExtractor, rest for fitting!'
       if setup.version lt 4 then BEGIN
           print, 'you seem to be using mulit-wavelength data, but the GALFIT version you have specified only supports one-band data'
           print, 'This version of galapagos needs GALFIT4 in order to be able to read out the fitting parameters (output has to be in a fits table)'
           stop
       ENDIF
       readcol, setup.files, band, wavelength, mag_offset, filelist, zeropoint, exptime, $
         format = 'A,I,F,A,F,F', comment = '#', /silent      

       nband=fix(n_elements(band)-1)
; read first file to get number of images...
       readcol, filelist[0], hlpimages, hlpweights, hlpoutpath, hlpoutpre, $
         format = 'A,A,A,A', comment = '#', /silent
       cnt=intarr(nband+1)

; create arrays in setup needed to store all the data
       add_tag, setup, 'images', strarr(n_elements(hlpimages),nband+1), setup2
       setup=setup2
       add_tag, setup, 'weights', strarr(n_elements(hlpimages),nband+1), setup2
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
       delvarx, setup2
       for b=0,nband do begin
           readcol, filelist[b], hlpimages, hlpweights, hlpoutpath, hlpoutpre, $
             format = 'A,A,A,A', comment = '#', /silent
           cnt[b]=n_elements(hlpimages)
           if (cnt[b] ne cnt[0]) and not keyword_set(silent) then print, 'input list '+strtrim(band[b])+' contains a wrong number of entries (tiles)'
           if (cnt[b] ne cnt[0]) and not keyword_set(silent) then stop
           setup.images[*,b]=hlpimages
           setup.weights[*,b]=hlpweights
           setup.outpre[*,b]=hlpoutpre
           setup.outpath[*,b]=set_trailing_slash(setup.outdir)+set_trailing_slash(strtrim(hlpoutpath,2))
           setup.outpath_band[*,b]=setup.outpath[*,b]+strtrim(band[b],2)
       endfor 
   endif
   if ncolf ne 6 and ncolf ne 4 then message, 'Invalid Entry in '+setup_file
END

FUNCTION read_sersic_results, obj, nband, bd=bd
   IF file_test(obj[0]) THEN BEGIN
       result = mrdfits(obj[0], 'FINAL_BAND',/silent)
       res_cheb = mrdfits(obj[0], 'FINAL_CHEB',/silent)
       fit_info = mrdfits(obj[0], 'FIT_INFO',/silent)
       band_info = mrdfits(obj[0], 'BAND_INFO',/silent)
;       hd = headfits(obj[0], exten = nband+1)
       comp=1
       repeat comp = comp +1 until tag_exist(result, 'COMP'+strtrim(comp,2)+'_MAG') eq 0
; delete feedback, just in case the format of one is different,
; avoiding crash
       delvarx, feedback
       if not keyword_set(bd) then begin
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
                                    'psf_galfit_band', strtrim(band_info.psf, 2), $
                                    'chisq_galfit', fit_info.chisq, $
                                    'ndof_galfit', fit_info.ndof, $
                                    'nfree_galfit', fit_info.nfree, $
                                    'nfix_galfit', fit_info.nfix, $
                                    'chi2nu_galfit', fit_info.chi2nu, $
                                    'iter', fit_info.niter, $
                                    'neigh_galfit', comp-3, 'flag_galfit', 2)
; TO BE ADDED:
; fitting time
; NEIGH_GALFIT HAS TO BE ADAPTED! WHY??
       ENDIF
       if keyword_set(bd) then begin
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
                                    'y_galfit_b', result[0].COMP3_YC, 'yerr_galfit_b', result[0].COMP3_YC_ERR, $                                    'psf_galfit', strtrim(band_info[0].psf,2), 'sky_galfit', result[0].COMP1_SKY, $
                                    'mag_galfit_band_d', result.COMP2_MAG, 'magerr_galfit_band_d',result.COMP2_MAG_ERR, $
                                    're_galfit_band_d', result.COMP2_RE, 'reerr_galfit_band_d', result.COMP2_RE_ERR, $
                                    'n_galfit_band_d', result.COMP2_N, 'nerr_galfit_band_d' ,result.COMP2_N_ERR, $
                                    'q_galfit_band_d', result.COMP2_AR, 'qerr_galfit_band_d', result.COMP2_AR_ERR, $
                                    'pa_galfit_band_d', result.COMP2_PA, 'paerr_galfit_band_d', result.COMP2_PA_ERR, $
                                    'x_galfit_band_d', result.COMP2_XC, 'xerr_galfit_band_d', result.COMP2_XC_ERR, $
                                    'y_galfit_band_b', result.COMP2_YC, 'yerr_galfit_band_b', result.COMP2_YC_ERR, $
                                    'mag_galfit_band_b', result.COMP3_MAG, 'magerr_galfit_band_b',result.COMP3_MAG_ERR, $
                                    're_galfit_band_b', result.COMP3_RE, 'reerr_galfit_band_b', result.COMP3_RE_ERR, $
                                    'n_galfit_band_b', result.COMP3_N, 'nerr_galfit_band_b' ,result.COMP3_N_ERR, $
                                    'q_galfit_band_b', result.COMP3_AR, 'qerr_galfit_band_b', result.COMP3_AR_ERR, $
                                    'pa_galfit_band_b', result.COMP3_PA, 'paerr_galfit_band_b', result.COMP3_PA_ERR, $
                                    'x_galfit_band_b', result.COMP3_XC, 'xerr_galfit_band_b', result.COMP3_XC_ERR, $
                                    'y_galfit_band_b', result.COMP3_YC, 'yerr_galfit_band_b', result.COMP3_YC_ERR, $
                                    'sky_galfit_band', result.COMP1_SKY, $
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
                                    'sky_galfit_cheb', res_cheb.COMP1_SKY, $
                                    'psf_galfit_band', strtrim(band_info.psf, 2), $
                                    'chisq_galfit_bd', fit_info.chisq, $
                                    'ndof_galfit_bd', fit_info.ndof, $
                                    'nfree_galfit_bd', fit_info.nfree, $
                                    'nfix_galfit_bd', fit_info.nfix, $
                                    'chi2nu_galfit_bd', fit_info.chi2nu, $
                                    'iter_bd', fit_info.niter, $
                                    'neigh_galfit_bd', comp-4, 'flag_galfit_bd', 2)
; TO BE ADDED:
; fitting time
; NEIGH_GALFIT HAS TO BE ADAPTED! WHY??
       ENDIF

   ENDIF ELSE BEGIN
       psf=strarr(nband)
       for n=0,nband-1 do psf[n]='none'
       
       if not keyword_set(bd) then begin
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
                                    'psf_galfit_band', psf, $
                                    'chisq_galfit', -99., $
                                    'ndof_galfit', -99l, $
                                    'nfree_galfit', -99l, $
                                    'nfix_galfit', -99l, $
                                    'chi2nu_galfit', -99., $
                                    'iter', -99, $
                                    'neigh_galfit', -99, 'flag_galfit', 1)
 ; TO BE ADDED:
; fitting time
; NEIGH_GALFIT HAS TO BE ADAPTED!
       ENDIF

       if keyword_set(bd) then begin
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
                                    'psf_galfit', 'none', 'sky_galfit', -999., $
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
                                    'sky_galfit_band', fltarr(nband)-999., $
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
                                    'sky_galfit_cheb', fltarr(nband)-999., $
                                    'psf_galfit_band', psf, $
                                    'chisq_galfit_bd', -99., $
                                    'ndof_galfit_bd', -99l, $
                                    'nfree_galfit_bd', -99l, $
                                    'nfix_galfit_bd', -99l, $
                                    'chi2nu_galfit_bd', -99., $
                                    'iter_bd', -99, $
                                    'neigh_galfit_bd', -99, 'flag_galfit_bd', 1)
 ; TO BE ADDED:
; fitting time
; NEIGH_GALFIT HAS TO BE ADAPTED!
       ENDIF

   ENDELSE
   return, feedback
;[mag, magerr, re, reerr, n, nerr, q, qerr, pa, paerr, $
;            x, xerr, y, yerr, sky, neigh_galfit, chisq_galfit, ndof_galfit, $
;            nfree_galfit, nfix_galfit, chi2nu_galfit]
END

FUNCTION read_sersic_results_old_galfit, obj, bd=bd
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
       flag_galfit = 2
       chisq_galfit = float(strmid(sxpar(hd, 'CHISQ'),2)) 
       ndof_galfit = fix(strmid(sxpar(hd, 'NDOF'),2))
       nfree_galfit = fix(strmid(sxpar(hd, 'NFREE'),2))
       nfix_galfit = fix(strmid(sxpar(hd, 'NFIX'),2))
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
       ndof_galfit = -99
       nfree_galfit = -99
       nfix_galfit = -99
       chi2nu_galfit = -99.
       psf='none'
   ENDELSE
   
   if not keyword_set(bd) then BEGIN
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
                                'neigh_galfit', neigh_galfit, 'flag_galfit', flag_galfit)
   endif

   if keyword_set(bd) then begin
       IF file_test(obj) THEN BEGIN
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
                                'psf_galfit', psf, 'sky_galfit', sky, $
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
                                'sky_galfit_band', sky, $
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
                                'sky_galfit_cheb', sky, $
                                'psf_galfit_band', psf, $
                                'chisq_galfit_bd', chisq_galfit, $
                                'ndof_galfit_bd', ndof_galfit, $
                                'nfree_galfit_bd', nfree_galfit, $
                                'nfix_galfit_bd', nfix_galfit, $
                                'chi2nu_galfit_bd', chi2nu_galfit, $
                                'neigh_galfit_bd', neigh_galfit, 'flag_galfit_bd', flag_galfit)
   ENDIF       
   return, feedback
END

PRO update_table, fittab, table, i, out_file, obj_file, sky_file, nband, setup, final = final, bd=bd
; HAS TO BE CHANGED FOR POSSIBILITY FOR B/D DECOMPOSITION
forward_function read_sersic_results
forward_function read_sersic_results_old_galfit
; this routine takes care of objects with non-existent output files
; (e.g. crashed)
    IF setup.version ge 4 then res = read_sersic_results(out_file, nband, bd=bd)
    IF setup.version lt 4 then res = read_sersic_results_old_galfit(out_file, bd=bd)      
    name_fittab = tag_names(fittab)
    name_res = tag_names(res)
if keyword_set(bd) then stop
    
    for j=0,n_elements(name_res)-1 do begin
        tagidx=where(name_fittab eq name_res[j], ct)
        type=size(res.(j))
; if keyword is INT
        if type[1] eq 2 or type[1] eq 3 then begin
            wh=where(finite(res.(j)) ne 1, ct)
            if ct gt 0 then res[wh].(j)=-99999
        ENDIF
; if keyword is FLOAT
        if type[1] eq 4 then begin
            wh=where(finite(res.(j)) ne 1, ct)
            if ct gt 0 then res[wh].(j)=-99999.
        ENDIF
; if keyword is DOUBLE
        if type[1] eq 5 then begin
            wh=where(finite(res.(j)) ne 1, ct)
            if ct gt 0 then res[wh].(j)=double(-99999.)
        ENDIF
; if keyword is STRING
        if type[1] eq 7 then begin
            wh=where(res.(j) eq ' ', ct)
            if ct gt 0 then res[wh].(j)='null'
        ENDIF
;          if ct gt 0 then print, 'changed'
        fittab[i].(tagidx) = res.(j)          
    ENDFOR

    if not keyword_set(bd) then fittab[i].flag_galfit = res.flag_galfit    
    if keyword_set(bd) then fittab[i].flag_galfit_bd = res.flag_galfit_bd    

    
    if not file_test(out_file) then begin
        if not file_test(obj_file) then begin
; object has not yet been started.
            if not keyword_set(bd) then begin
                table[i].flag_galfit = 0
                fittab[i].flag_galfit = 0
            endif
            if keyword_set(bd) then begin
                table[i].flag_galfit_bd = 0
                fittab[i].flag_galfit_bd = 0
            endif
        endif else begin
; object has been started and crashed (or is currently doing sky determination)
            if not keyword_set(bd) then begin
                table[i].flag_galfit = 1
                fittab[i].flag_galfit = 1
            endif
            if keyword_set(bd) then begin
                table[i].flag_galfit_bd = 1
                fittab[i].flag_galfit_bd = 1
            endif
; read all sky files (for the case that the fit crashed and the output
; file does not exist. Useful to find systematic crashes with sky value)
            if not keyword_set(bd) then begin
                for b=1,nband do begin
; check whether sky file exists first
; overwrite -999. from above with true value
                    if file_test(sky_file[b]) eq 1 then begin
                        openr, 99, sky_file[b]
                        readf, 99, sky, dsky, minrad, maxrad, flag
                        close, 99
                        fittab[i].sky_galfit_band[b-1] = round_digit(sky,3)
                        if b eq 1 then $
                          fittab[i].sky_galfit = round_digit(sky,3)
                    endif
                endfor
            ENDIF
        ENDELSE
    ENDIF ELSE BEGIN

; IN B/D THIS BIT WILL BE DONE IN EACH READIN, BUT RESULT IS EQUIVALENT
        if keyword_set(final) then begin
            fittab[i].org_image = table[i].tile
            fittab[i].org_image_band = table[i].tile
        ENDIF
        if not keyword_set(final) then fittab[i].org_image = table[i].frame[0]
        
        if not keyword_set(bd) then begin
            table[i].flag_galfit = 2
            fittab[i].file_galfit = out_file
        ENDIF
        if keyword_set(bd) then begin
            table[i].flag_galfit_bd = 2
            fittab[i].file_galfit_bd = out_file
        ENDIF
    ENDELSE
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

PRO galapagos, setup_file, gala_PRO, logfile=logfile, plot=plot
   start=systime(0)
   print, 'start: '+start
   IF n_params() LE 1 THEN gala_pro = 'galapagos'
;   gala_pro = '/home/boris/IDL/gala/galapagos.pro'
;   logfile = '/data/gama/galapagos_multi_wl_galapagos.log'
; .run galapagos.pro
;  galapagos,'~/megamorph_dev/astro-megamorph/scripts_boris/megamorph/gala_setup/multi-wl/gala.gama_mwl_1'
;  galapagos,'~/IDL/megamorph/gala_setup/gala.gama_set3.2_test'
;==============================================================================
;main input: location of the setup file
   IF n_params() LT 1 THEN BEGIN
      setup_file = ''
      read, prompt = 'Location of setup file: ', setup_file
   ENDIF
;==============================================================================
;read in the setup file
   read_setup, setup_file, setup
   IF keyword_set(logfile) THEN $
     start_log, logfile, 'Reading setup file... done!'
   if setup.dobd eq 1 then print, 'You are trying to do B/D decomposition? You are crazy!' 

;==============================================================================   
;read input files into arrays
   read_image_files, setup
   nband = setup.nband
   images = setup.images
   weights = setup.weights
   outpath = setup.outpath
   outpath_band = setup.outpath_band
   outpre = setup.outpre
   nband = setup.nband

; now that number of bands is known, correct number of additional cheb
; combonents to nband -1
   setup.cheb=setup.cheb <(nband-1)

;; NAMING CONVENTIONS AND EXAMPLES
; MULTIBAND
; outpath:              /data/gama/galapagos_multi_wl/tile10_5/
; outpath_galfit:       /data/gama/galapagos_multi_wl/tile10_5/galfit
; outpath_band[0]:      /data/gama/galapagos_multi_wl/tile10_5/sex/
; outpath_pre[0]:       /data/gama/galapagos_multi_wl/tile10_5/sex/t10_5.
; outpath_file[0]:      /data/gama/galapagos_multi_wl/tile10_5/sex/t10_5.sex.
; outpath_file_no_band: /data/gama/galapagos_multi_wl/tile10_5/t10_5.
; 1 BAND
; outpath:              /data/gama/galapagos_multi_wl_test_3.2/tile10_5/
; outpath_galfit:       /data/gama/galapagos_multi_wl/tile10_5/
; outpath_band:         /data/gama/galapagos_multi_wl_test_3.2/tile10_5/
; outpath_pre:          /data/gama/galapagos_multi_wl_test_3.2/tile10_5/t10_5.
; outpath_file:         /data/gama/galapagos_multi_wl_test_3.2/tile10_5/t10_5.v.
   outpath = set_trailing_slash(outpath)
   outpath_galfit = strtrim(outpath[*,0]+setup.galfit_out_path,2)
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
   neighbours = strarr(setup.nneighb >1, nframes)
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
   FOR i=0ul, n_elements(outpath_band)-1 DO IF NOT file_test(outpath_band[i]) THEN $
     spawn, 'mkdirhier '+strtrim(outpath_band[i],2)
   FOR i=0ul, n_elements(outpath_galfit)-1 DO IF NOT file_test(outpath_galfit[i]) THEN $
   spawn, 'mkdirhier '+outpath_galfit[i]
   IF keyword_set(logfile) THEN $
     update_log, logfile, 'Initialisation... done!'
;===============================================================================

; define the columns that have to be added to the SExtractor catalogue
; to get output catalogue
; This already defines ALL parameters, even the B/D ones if later
; needed. Nothing is done to those until starting the BD-block!
   define_addcol, addcol, nband, fit_bd=setup.dobd
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
;
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
         setup.outdir+'sexcomb.reg', 10, color='green', tag = 'comb'
       IF keyword_set(logfile) THEN $
         update_log, logfile, 'SExtraction... done!'
   ENDIF
;==============================================================================
;create postage stamp description files 
   IF setup.dostamps THEN BEGIN
       FOR i=0ul, nframes-1 DO BEGIN
           print, 'cutting postages for image '+strtrim(outpath_file_no_band[i,0],2)+' and similar'
           create_stamp_file, images[i,0], $
             outpath_file[i,0]+setup.outcat, $
             outpath_file[i,0]+setup.outparam, $
             outpath_file_no_band[i,0]+setup.stampfile, $
             setup.stampsize
           FOR b=1,nband do begin
               print, 'cutting postage stamps for '+strtrim(setup.stamp_pre[b],2)+'-band'
               cut_stamps, images[i,b], $
                 outpath_file_no_band[i,0]+setup.stampfile, $
                 outpath_band[i,b], $
                 outpre[i,b], '_'+setup.stamp_pre[b]
          ENDFOR
       ENDFOR
;create skymap files 
       FOR i=0ul, nframes-1 DO BEGIN
           print, 'creating skymap for image '+strtrim(outpath_file_no_band[i,0],2)
           FOR b=1,nband do begin
               print, 'creating skymap for '+strtrim(setup.stamp_pre[b],2)+'-band'
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
; read in PSF list
;==============================================================================
   IF setup.dosky or setup.dobd  THEN BEGIN
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
   ENDIF

;==============================================================================
;measure sky and run galfit?
;==============================================================================
   IF setup.dosky THEN BEGIN
       IF keyword_set(logfile) THEN $
         update_log, logfile, 'Beginning sky loop...'
;sort the total catalogue by magnitude and select the brightest BRIGHT percent
       br = sort(sexcat.mag_best)
       nbr = round(n_elements(sexcat.mag_best)*setup.bright/100.)

; this table contains all objects, but only sextracor parameters.
; reordering objects to be in brightness order
       table = sexcat[br]
; make table.frame multi-wavelength-ready to be passed onto gala_bridge
       tableim = strarr(nband+1,n_elements(table.frame))

       for i = 0, n_elements(images[*,0])-1 do begin
           whtableim = where(table.frame eq images[i,0], ct)
           if ct gt 0 then for b=0,nband do tableim[b,whtableim] = images[i,b]
       ENDFOR 

       table=remove_tags(table,'frame')
       add_tag, table, 'frame', strarr(nband+1), table2
       table=table2
       table.frame = tableim
       add_tag, table, 'flag_galfit', 0, table2
       table=table2
       delvarx, table2
       
; this table contains additional columns, but only brightes br% of
; objects
; this line only created an empty structure, no values in yet!
       fittab = read_sex_param(outpath_file[0,0]+setup.outparam, nbr, $
                               add_column = addcol)
stop
; fittab does NOT contain FRAME (which is needed quite often!) Other
; than that, table is a subset of parameters, fittab is a subset of
; objects (only [br])
; set standard values
       struct_assign, table, fittab
       fittab.mag_galfit = 999.
       fittab.mag_galfit_band = fltarr(nband)+999.
       fittab.re_galfit = -99.
       fittab.re_galfit_band = fltarr(nband)-99.
       fittab.n_galfit = -99.
       fittab.n_galfit_band = fltarr(nband)-99.
       fittab.q_galfit = -99.
       fittab.q_galfit_band = fltarr(nband)-99.

stop
       mwrfits, table, setup.outdir+setup.sexcomb+'.ttmp', /create
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
       
;calculate sky for the brightest objects
;******************************************************************************
;******************************************************************************
;current object (will be used and overwritten by the neue, optimized, queue)
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

      if keyword_set(plot) then begin
          loadct,39,/silent
          plot, table.alpha_j2000, table.delta_j2000, psym=3, ystyle=1, xstyle=1
      endif
      
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
          
;check if current object exists
loopstart:
          todo=where(fittab.flag_galfit eq 0)
          if todo[0] eq -1 then begin
              FOR i=0, setup.max_proc-1 DO bridge_use[i] = bridge_arr[i]->status()
              goto, loopend
          ENDIF
          ct = 0
                    
loopstart2:
;get status of bridge elements
          FOR i=0, setup.max_proc-1 DO bridge_use[i] = bridge_arr[i]->status()
          
;check for free bridges
          free = where(bridge_use eq 0, ct)
          
;         IF ct GT 0 AND cur LT nbr THEN BEGIN
          IF ct GT 0 AND todo[0] ne -1 THEN BEGIN
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;at least one bridge is free --> start newobject
;the available bridge is free[0]
              
;treat finished objects first
              IF bridge_obj[free[0]] GE 0 THEN BEGIN
;read in feedback data
                  idx = where(table[bridge_obj[free[0]]].frame[0] EQ orgim[*,0])
                  objnum = round_digit(table[bridge_obj[free[0]]].number, 0, /str)
                  obj_file = (outpath_galfit[idx]+orgpre[idx]+objnum+'_'+setup.obj)[0]
                  out_file = (outpath_galfit[idx]+orgpre[idx]+objnum+'_'+setup.galfit_out)[0]
                  sky_file = strarr(nband+1)
                  for q=1,nband do sky_file[q] = (outpath_galfit[idx]+orgpre[idx,q]+objnum+'_'+setup.stamp_pre[q]+'_'+setup.outsky)[0]
 
;check if file was done successfully or bombed
                  update_table, fittab, table, bridge_obj[free[0]], out_file+'.fits', obj_file, sky_file, nband, setup
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
                      gcirc, 1, table[todo[ob]].alpha_j2000/15d, table[todo[ob]].delta_j2000, $
                        bridge_pos[0, filled]/15d, bridge_pos[1, filled], dist
; get distance to all blocked objects
                      if n_elements(blocked) gt 1 then $
                        gcirc, 1, table[todo[ob]].alpha_j2000/15d, table[todo[ob]].delta_j2000, $
                        table[blocked[1:n_elements(blocked)-1]].alpha_j2000/15d, $
                        table[blocked[1:n_elements(blocked)-1]].delta_j2000, dist_block
                      if n_elements(blocked) eq 1 then dist_block = 2*setup.min_dist
; can be simplified when the plots are taken out!
                      IF min(dist) LT setup.min_dist or min(dist_block) lt setup.min_dist_block THEN BEGIN
                          blocked = [[blocked],todo[ob]]
                      ENDIF
                      
                      ob++
                      if ob eq n_elements(todo) and $
                        (min(dist) lt setup.min_dist or min(dist_block) lt setup.min_dist_block) then begin
                          wait, 1
                          ob=0l
                          print, 'starting over'
                          goto, loopstart2
                      ENDIF
                      
                  ENDREP UNTIL (min(dist) gt setup.min_dist and min(dist_block) gt setup.min_dist_block) or ob ge n_elements(todo)-1
                  IF min(dist) LT setup.min_dist or min(dist_block) lt setup.min_dist_block THEN CONTINUE
              ENDIF
              ob=ob-1>0
              cur=todo[ob]
              
; check whether this object has already been done, if so, read in
; result
              ct = 0 
              IF cur LT nbr THEN idx = where(table[cur].frame[0] EQ orgim[*,0], ct)
              IF ct GT 0 THEN BEGIN
                  objnum = round_digit(table[cur].number, 0, /str)
                  obj_file = (outpath_galfit[idx]+orgpre[idx]+objnum+'_'+setup.obj)[0]
                  out_file = (outpath_galfit[idx]+orgpre[idx]+objnum+'_'+setup.galfit_out)[0]
                  sky_file = strarr(nband+1)
                  for q=1,nband do sky_file[q] = (outpath_galfit[idx]+orgpre[idx,q]+objnum+'_'+setup.stamp_pre[q]+'_'+setup.outsky)[0]
;check if file was done successfully or bombed and update table                  
                  IF file_test(obj_file) THEN BEGIN
;                      print, obj_file+' found.'
                      print, 'Updating table now! ('+strtrim(cur, 2)+'/'+strtrim(nbr, 1)+')'                      
                      update_table, fittab, table, cur, out_file+'.fits', obj_file, sky_file, nband, setup
                      IF n_elements(todo) ne 1 then goto, loopstart
                      IF n_elements(todo) eq 1 then goto, loopend
                  ENDIF
              ENDIF

;store position of new object
              bridge_obj[free[0]] = cur
              bridge_pos[*, free[0]] = [table[cur].alpha_j2000, table[cur].delta_j2000]
              table[cur].flag_galfit = 1
              fittab[cur].flag_galfit = 1
              print, '  currently working on No. '+strtrim(n_elements(where(table.flag_galfit ge 1)),2)+' of '+strtrim(n_elements(sexcat),2)+'   '
;              print, obj_file
              if keyword_set(plot) then begin
                  plot, table.alpha_J2000,table.delta_J2000, psym=3, ystyle=1, xstyle=1
                  if n_elements(blocked) gt 1 then begin
                      plots, table[blocked[1:n_elements(blocked)-1]].alpha_J2000,table[blocked[1:n_elements(blocked)-1]].delta_J2000, psym=4, col=200, symsize=2
                      for r=1,n_elements(blocked)-1 do tvellipse, setup.min_dist_block/3600., setup.min_dist_block/3600., table[blocked[r]].alpha_J2000,table[blocked[r]].delta_J2000,col=200,/data
                  ENDIF                
                  done=where(table.flag_galfit ge 1, count)
                  if count ge 1 then begin
                      plots, table[done].alpha_J2000,table[done].delta_J2000, psym=1, col=135, thick=2, symsize=0.5
                  endif
                  plots, bridge_pos[0,*], bridge_pos[1,*], psym=1, col=235
                  for q=0,n_elements(bridge_pos[0,*])-1 do tvellipse, setup.min_dist/3600., setup.min_dist/3600., bridge_pos[0,q], bridge_pos[1,q], col=235,/data,thick=2
              ENDIF
              
;find the matching filenames
              idx = where(table[cur].frame[0] EQ orgim[*,0])
;define the file names for the:
;postage stamp parameters
              stamp_param_file = (orgpath_file_no_band[idx,0]+setup.stampfile)[0]
              objnum = round_digit(table[cur].number, 0, /str)
;galfit masks
              mask_file = strarr(nband+1)
              for q=1,nband do mask_file[q] = (outpath_galfit[idx]+outpre[idx,q]+objnum+'_'+setup.stamp_pre[q]+'_'+setup.mask)[0]
;galfit obj file
              obj_file = (outpath_galfit[idx]+orgpre[idx]+objnum+'_'+setup.obj)[0]
;galfit constraint file
              constr_file = (outpath_galfit[idx]+orgpre[idx]+objnum+'_'+setup.constr)[0]
;galfit input file
              im_file = strarr(nband+1)
              for q=1,nband do im_file[q] = (orgpath_pre[idx,q]+objnum+'_'+setup.stamp_pre[q])[0]
              
;galfit output path
              out_file = (outpath_galfit[idx]+orgpre[idx]+objnum+'_'+setup.galfit_out)[0]
;sky summary file
              sky_file = strarr(nband+1)
              for q=1,nband do sky_file[q] = (outpath_galfit[idx]+outpre[idx,q]+objnum+'_'+setup.stamp_pre[q]+'_'+setup.outsky)[0]
              
; choose closest PSF according to RA & DEC and subtract filename from structure 'psf_struct'
; read in chosen_psf into 'psf', filename in chosen_psf_file   
              choose_psf, table[cur].alpha_j2000, table[cur].delta_j2000, $
                psf_struct, table[cur].frame, chosen_psf_file, nband
              
; change seed for random in getsky_loop
;            randxxx=randomu(seed,1)
;            delvarx, randxxx 
              seed=table[cur].number
; create sav file for gala_bridge to read in
              save, cur, orgwht, idx, orgpath, orgpre, setup, chosen_psf_file,$
                sky_file, stamp_param_file, mask_file, im_file, obj_file, $
                constr_file, out_file, fittab, nband, orgpath_pre, outpath_file, $
                outpath_file_no_band, orgpath_file_no_band, outpath_galfit, $
                orgpath_band, orgpath_file, seed,$
                filename=out_file+'.sav'
              
              IF setup.max_proc GT 1 THEN BEGIN
                  IF keyword_set(logfile) THEN $
                    update_log, logfile, 'Starting new bridge... ('+out_file+')'
; print, 'starting new object at '+systime(0)
                  bridge_arr[free[0]]->execute, 'astrolib'
;               bridge_arr[free[0]]->execute, 'cd,"/home/gems/gala"';§§§§§§§§§§
                  bridge_arr[free[0]]->execute, '.r '+gala_pro
                  bridge_arr[free[0]]->execute, $
                    'gala_bridge, "'+out_file+'.sav"', /nowait
              ENDIF ELSE BEGIN
                  IF keyword_set(logfile) THEN $
                    update_log, logfile, 'Starting next object... ('+out_file+')'
                  cd, orgpath[idx,0]
                  gala_bridge, out_file+'.sav'
                  file_delete, orgpath[idx,0]+'galfit.[0123456789]*', /quiet, $
                    /allow_nonexistent, /noexpand_path
              ENDELSE
;switch to next object
          ENDIF ELSE BEGIN
;all bridges are busy --> wait 
              wait, 1

; FOR TESTING PURPOSES ONLY!!!
;; kill all process that have been running for more than 3 hours
;              countloop=countloop+1
;              if countloop mod 100 eq 0 then begin
;                  print, 'trying to kill long running galfits at '+systime(0)
;                  print, 'ps -uboris -o pid,comm,bsdtime | grep galfit | awk "{split($3, t, ":"); m = int(t[1]); if (t > 60) print $1}" | xargs kill'
;                                                                               
;              ENDIF

          ENDELSE
          
loopend:
;stop when all done and no bridge in use any more
      ENDREP UNTIL todo[0] eq -1 AND total(bridge_use) EQ 0
      
;have to read in the last batch of objects
      remain = where(bridge_obj ge 0, ct)
      IF ct GT 0 THEN BEGIN
         FOR i=0, ct-1 DO BEGIN
            idx = where(table[bridge_obj[remain[i]]].frame[0] EQ orgim[*,0])
            objnum = round_digit(table[bridge_obj[remain[i]]].number, 0, /str)
            obj_file = (outpath_galfit[idx]+orgpre[idx,1]+objnum+'_'+setup.obj)[0]
            out_file = (outpath_galfit[idx]+orgpre[idx,1]+objnum+'_'+setup.galfit_out)[0]
            sky_file = strarr(nband+1)
            for q=1,nband do sky_file[q] = (outpath_galfit[idx]+orgpre[idx,q]+objnum+'_'+setup.stamp_pre[q]+'_'+setup.outsky)[0]
           
            if keyword_set(plot) then begin
                plots, table[bridge_obj[remain[i]]].alpha_J2000,table[bridge_obj[remain[i]]].delta_J2000, psym=1, col=135, thick=2, symsize=2
                tvellipse, setup.min_dist/3600., setup.min_dist/3600., $
                  table[bridge_obj[remain[i]]].alpha_J2000,table[bridge_obj[remain[i]]].delta_J2000, col=0,/data
            ENDIF
            bridge_obj[remain[i]] = -1
            bridge_pos[*, remain[i]]= [!values.F_NAN, !values.F_NAN]            
            
;check if file was done successfully or bombed
; if succesfully, fill fitting parameters into fittab
            update_table, fittab, table, bridge_obj[remain[i]], out_file, obj_file, sky_file, nband, setup
;print, 'out file exists -- fittab updated'
;else output file does not exist --> bombed
         ENDFOR
      ENDIF

;== START BATCH MODE, SCRAP THIS WHEN QUEUE WORKS FINE WITH DATABASE =================================
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
; make table.frame multi-wavelength-ready to be passed onto gala_bridge
          tableim = strarr(nband+1,n_elements(table.frame))
          for i = 0, n_elements(images[*,0])-1 do begin
              whtableim = where(table.frame eq images[i,0], ct)
              if ct gt 0 then for b=0,nband do tableim[b,whtableim] = images[i,b]
          ENDFOR 
          table=remove_tags(table,'frame')
          add_tag, table, 'frame', strarr(nband+1), table2
          table=table2
          delvarx, table2
          table.frame = tableim
          
;sort table by magnitude
          br = sort(table.mag_best)
          table = table[br]
          
;number of objects in table
          n_obj = n_elements(table)
          
          first = 1
          
;calculate sky for all objects in current frame
          counter=0
          todo = n_elements(where(table.frame[0] eq images[f]))

          FOR current_obj=0ul, n_obj-1 DO BEGIN
              
;continue if current object is on neighbouring frame
              IF table[current_obj].frame[0] NE images[f] THEN CONTINUE
              counter +=1
              print, '  currently working on No. '+strtrim(counter,2)+' of '+strtrim(todo,2)+'  objects on frame  '+ $
                strtrim(images[f],2)+'  (frame '+strtrim(f+1,2)+' of '+strtrim(nframes,2)+')'
;find the matching filenames
              idx = where(table[current_obj].frame[0] EQ orgim[*,0])
                            
;define the file names for the:
;postage stamp parameters
              stamp_param_file = (orgpath_file_no_band[idx,0]+setup.stampfile)[0]
              objnum = round_digit(table[current_obj].number, 0, /str)
;galfit masks
              mask_file = strarr(nband+1)
              for q=1,nband do mask_file[q] = (outpath_galfit[idx]+orgpre[idx,1]+objnum+'_'+setup.stamp_pre[q]+'_'+setup.mask)[0]
;galfit obj file
              obj_file = (outpath_galfit[idx]+orgpre[idx,1]+objnum+'_'+setup.obj)[0]
;galfit constraint file
              constr_file = (outpath_galfit[idx]+orgpre[idx,1]+objnum+'_'+setup.constr)[0]
;galfit input file
              im_file = strarr(nband+1)
              for q=1,nband do im_file[q] = (orgpath_pre[idx,q]+objnum+'_'+setup.stamp_pre[q])[0]
;galfit output path
              out_file = (outpath_galfit[idx]+orgpre[idx,1]+objnum+'_'+setup.galfit_out)[0]
;sky summary file
              sky_file = strarr(nband+1)
              for q=1,nband do sky_file[q] = (outpath_galfit[idx]+orgpre[idx,1]+objnum+'_'+setup.stamp_pre[q]+'_'+setup.outsky)[0]
              
              IF first THEN BEGIN
; only to be done for first object on file
                  first = 0
                  print, systime(), ' reading image and estimating global sky values for each band'
; loop over band again as in gala_bridge
                  global_sky = fltarr(nband+1)
                  global_sigsky = fltarr(nband+1)
                  for b=1,nband do begin
                      
;read in image and weight (takes 15sec)
                      im = readfits(table[current_obj].frame[b], hd,/silent)
;image size
                      sz_im = (size(im))[1:2]
                      
;read the skymap
                      map = readfits(orgpath_file_no_band[idx,b]+setup.skymap+'.fits', xxhd,/silent)
                      
;rad is the minimum starting radius for the sky calculation (outside
;the aperture of the object)
                      rad = table.a_image*table.kron_radius*setup.skyscl+setup.skyoff
                      
;get first guess for global sky
; same changes as in gala_bridge.
                      skypix = where(map EQ 0 and finite(im) eq 1,ct)
; new scheme takes around 2 second,s the next, old system, about 6
; moment() would be 3 times faster, but returns a MEAN value!
;; current version
                      global_sky=median(im[skypix])
                      global_sigsky=stddev(im[skypix])
                      if finite(global_sky) ne 1 or finite(global_sigsky) ne 1 then begin
;; old version
;   resistant_mean, im[skypix], 3, sky, sigsky, nrej
;   sigsky *= sqrt(ct-1-nrej)
                          global_sky=median(im[skypix],/double)
                          global_sigsky=stddev(im[skypix],/double)
                      endif
;; also belonging to old version
;   resistant_mean, im[skypix], 3, sky, sigsky, nrej
;   sigsky *= sqrt(ct-1-nrej)
;   par = [1, sky, sigsky, sigsky]
;; second step is a curvefit to the histogram
;   plothist, im[skypix], x, y, xr = [-1, 1]*5*sigsky+sky, /peak, /noplot
;   IF n_elements(par) LT n_elements(x) THEN $
;    yfit = curvefit(x, y, noweight, par, sigma, $
;                    FUNCTION_name = 'curvefit_gauss2side', $
;                    /noderivative, status = status, iter = iter)
;   global_sky = par[1]
;   global_sigsky = par[2]
                    
                      delvarx, skypix
                      
;make sure all positions are relative to the current frame
;(neighbouring frames may have negative positions)
                      adxy, hd, table.alpha_j2000, table.delta_j2000, x, y
                      
                      table.x_image = x+1
                      table.y_image = y+1
                  ENDFOR
              ENDIF
;does a GALFIT obj file exist (cannot test for _gf.fits -> files might
;bomb -> endless loop)?
              IF file_test(obj_file) THEN CONTINUE
              
              choose_psf, table[current_obj].alpha_j2000, table[current_obj].delta_j2000, $
                psf_struct, table[current_obj].frame, chosen_psf_file, nband

; update flag_galfit (for no reason at all, it's not used in this
; scheme here
              table[current_obj].flag_galfit = 1
; now estimate real skies for the object
              for b=1,nband do begin
; choose closest PSF according to RA & DEC and subtract filename from
; structure 'psf_struct'
                  psf = readfits(chosen_psf_file[b], psfhead,/silent)
                  
;read in image and weight again (takes 15sec)
                  im = readfits(table[current_obj].frame[b], hd,/silent)
                  wht = readfits(orgwht[idx,b], whd,/silent)
                  
;read segmentation map (needed for excluding neighbouring sources)
                  seg = readfits(orgpath_file[idx,0]+setup.outseg, xxhd,/silent)
;read the skymap
                  map = readfits(orgpath_file_no_band[idx,b]+setup.skymap+'.fits', xxhd,/silent)
                  
                  seed=table[current_obj].number
                  getsky_loop, setup, current_obj, table, rad, im, hd, map, setup.expt, $
                    setup.zp, setup.neiscl, setup.skyoff, setup.power, $
                    setup.cut, setup.files, psf, setup.dstep, $
                    setup.wstep, setup.gap, setup.nslope, sky_file[b], $
                    setup.galfit_out, setup.outcat, setup.outparam, $
                    setup.stampfile, global_sky[b], global_sigsky[b], $
                    setup.convbox, nums, frames, setup.galexe, fittab, b, $
                    orgpath_pre, outpath_file, outpath_file_no_band, nband, seed
                  
                  create_mask, table, wht, seg, stamp_param_file, mask_file[b], $
                    im_file[b], table[current_obj].frame[b], current_obj, $
                    setup.neiscl, setup.skyoff, nums, frames, $
                    setup.maglim_gal, setup.maglim_star, $
                    setup.stel_slope, setup.stel_zp, objects, corner, $
                    b
; saves memory generally, and (for some reasons) prevents IDL from
; taking up too much memory. Don't quite get that, doesn't even work
; when at the beginning of this loop.
                  delvarx,psf,im,wht,seg,map
; deblending is defined by primary band!
                  if b eq 1 then begin
                      delvarx, save_objects, save_corner
                      save_objects = objects
                      save_corner = corner
                  ENDIF       
              ENDFOR
              
              prepare_galfit, setup, save_objects, setup.files, save_corner, table, $
                obj_file, im_file, constr_file, mask_file, $
                chosen_psf_file, out_file, sky_file, setup.convbox, $
                setup.zp, setup.platescl, nums, frames, $
                current_obj, setup.outcat, setup.outparam, $
                setup.stampfile, setup.conmaxre, setup.conminm, $
                setup.conmaxm, fittab, setup.version, nband, orgpre
              
              cd, outpath_galfit[idx]
              IF setup.nice THEN spawn, 'nice '+setup.galexe+' '+obj_file $
              ELSE spawn, setup.galexe+' '+obj_file
              file_delete, outpath_galfit[idx]+'galfit.[0123456789]*', /quiet, $
                /allow_nonexistent, /noexpand_path
          ENDFOR
      ENDFOR
      IF n_elements(bridge_arr) GT 0 THEN obj_destroy, bridge_arr
      
      delvarx, fittab, table
      
  ENDIF

;==============================================================================
; start B/D fitting.
; a) optimized queue is not needed, all the single sersic fits already exist and should be read in
; b) sky estimations can be re-used.
; c) decision on neighbours could be re-used, too. Information need to be
; written out into a file, though. Or decision is reconsidered, as
; object parameters have changed and decision might be a different one.
; d) Neighbours will only be deblended as single sersics!
  IF setup.dobd THEN BEGIN
;goto, jump_over_this
goto, jump_over_this2

; first read in all single sersic results (ALL)
; This should NOT be neccessary, when table and fittab contain the
; same objects and/or if a proper database is used!!

      print, 'reading all single sersic results so that B/D has the best possible knowledge' 
; start again from virgin structure, fill with all fitting resuluts,
; only reading in, no additional fitting being done
; called sexcat above
      tab = read_sex_table(setup.outdir+setup.sexcomb, $
                           outpath_file[0,0]+setup.outparam, $
                           add_col = ['TILE', '" "'])

      add_tag, tab, 'flag_galfit', 0, tab2
      tab=tab2
      delvarx, tab2

      ntab = n_elements(tab)
      out = read_sex_param(outpath_file[0,0]+setup.outparam, ntab, $
                           add_column = addcol)

      struct_assign, tab, out

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

      print,' '
      FOR i=0ul, ntab-1 DO BEGIN
         statusline, ' reading result '+strtrim(i+1,2)+' of '+strtrim(ntab,2)
         objnum = round_digit(tab[i].number, 0, /str)

         idx = where(tab[i].tile EQ orgim[*,0])
         out_file = (outpath_galfit[idx]+orgpre[idx,0]+objnum+'_'+ $
                     setup.galfit_out)[0]+'.fits'

         obj_file = (outpath_galfit[idx]+orgpre[idx,1]+objnum+'_'+setup.obj)[0]
         sky_file = strarr(nband+1)
         for q=1,nband do sky_file[q] = (outpath_galfit[idx]+orgpre[idx,q]+objnum+'_'+setup.stamp_pre[q]+'_'+setup.outsky)[0]

         update_table, out, tab, i, out_file, obj_file, sky_file, nband, setup, /final
         out[i].org_image_band = orgim[idx[0],1:nband]

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

;=========================================================================
; finished reading in all single sersic results
;=========================================================================
      IF keyword_set(logfile) THEN $
        update_log, logfile, 'Beginning Bulge_Disk_decomposition ...'
;sort the total catalogue by magnitude and select the brightest BRIGHT
;percent
      br = sort(out.mag_best)
      table = out[br]
delvarx, out

;;;;;;;; currently decided on SExtractor mag. single sersic fitting mag might
; be better
      nbr = n_elements(where(table.mag_best lt setup.bd_maglim))

; make table.frame multi-wavelength-ready to be passed onto
; gala_bridge (NOT PASSED OVER, WRITTEN OUT AND READ IN!)
      tableim = strarr(nband+1,n_elements(table.org_image))
      for i = 0, n_elements(images[*,0])-1 do begin
          whtableim = where(table.org_image eq images[i,0], ct)
          if ct gt 0 then for b=0,nband do tableim[b,whtableim] = images[i,b]
      ENDFOR 

;      table=remove_tags(table,'frame')
      add_tag, table, 'frame', strarr(nband+1), table2
      table=table2
      table.frame = tableim
      delvarx, table2, todo
      
      fittab = read_sex_param(outpath_file[0,0]+setup.outparam, nbr, $
                              add_column = addcol)

; fill with single sersic results
      struct_assign, table, fittab
 ; set standard values for bulge & disk parameters
      fittab.mag_galfit_d = 999.
      fittab.mag_galfit_band_d = fltarr(nband)+999
      fittab.re_galfit_d = -99.
      fittab.re_galfit_band_d = fltarr(nband)-99.
      fittab.n_galfit_d = -99.
      fittab.n_galfit_band_d = fltarr(nband)-99.
      fittab.q_galfit_d = -99.
      fittab.q_galfit_band_d = fltarr(nband)-99.
      fittab.mag_galfit_b = 999.
      fittab.mag_galfit_band_b = fltarr(nband)+999.
      fittab.re_galfit_b = -99.
      fittab.re_galfit_band_b = fltarr(nband)-99.
      fittab.n_galfit_b = -99.
      fittab.n_galfit_band_b = fltarr(nband)-99.
      fittab.q_galfit_b = -99.
      fittab.q_galfit_band_b = fltarr(nband)-99.
      
      mwrfits, table, setup.outdir+setup.sexcomb+'.bd.ttmp', /create
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
      
      save, /all, filename = strtrim(setup.outdir,2)+'before_bd2.sav'
jump_over_this2:
      restore, strtrim(setup.outdir,2)+'before_bd2.sav'

stop
;calculate sky for the brightest objects
;******************************************************************************
;******************************************************************************
;current object (will be used and overwritten by the neue, optimized, queue)
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
      
      if keyword_set(plot) then begin
          loadct,39,/silent
          plot, fittab.alpha_j2000, fittab.delta_j2000, psym=3, ystyle=1, xstyle=1
      endif
      
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
          
;check if current object exists
loopstart_bd:
; only successful single sersic object??
; todo eq flag_galfit eq 2 and flag_galfit_bd eq 0?????

          todo=where(fittab.flag_galfit_bd eq 0)
          if todo[0] eq -1 then begin
              FOR i=0, setup.max_proc-1 DO bridge_use[i] = bridge_arr[i]->status()
              goto, loopend_bd
          ENDIF
          ct = 0
          
loopstart2_bd:
;get status of bridge elements
          FOR i=0, setup.max_proc-1 DO bridge_use[i] = bridge_arr[i]->status()
          
;check for free bridges
          free = where(bridge_use eq 0, ct)
          
;         IF ct GT 0 AND cur LT nbr THEN BEGIN
          IF ct GT 0 AND todo[0] ne -1 THEN BEGIN
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
              
;at least one bridge is free --> start newobject
;the available bridge is free[0]
              
;treat finished objects first
              IF bridge_obj[free[0]] GE 0 THEN BEGIN
;read in feedback data
                  idx = where(table[bridge_obj[free[0]]].frame[0] EQ orgim[*,0])
                  objnum = round_digit(table[bridge_obj[free[0]]].number, 0, /str)
                  obj_file = (outpath_galfit[idx]+orgpre[idx]+objnum+'_bd_'+setup.obj)[0]
                  out_file = (outpath_galfit[idx]+orgpre[idx]+objnum+'_bd_'+setup.galfit_out)[0]
                  sky_file = strarr(nband+1)
                  for q=1,nband do sky_file[q] = (outpath_galfit[idx]+orgpre[idx,q]+objnum+'_'+setup.stamp_pre[q]+'_bd_'+setup.outsky)[0]

;check if file was done successfully or bombed (done in update_table)
stop
                  update_table, fittab, table, bridge_obj[free[0]], out_file+'.fits', obj_file, sky_file, nband, setup, /bd
stop
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
                      gcirc, 1, table[todo[ob]].alpha_j2000/15d, table[todo[ob]].delta_j2000, $
                        bridge_pos[0, filled]/15d, bridge_pos[1, filled], dist
; get distance to all blocked objects
                      if n_elements(blocked) gt 1 then $
                        gcirc, 1, table[todo[ob]].alpha_j2000/15d, table[todo[ob]].delta_j2000, $
                        table[blocked[1:n_elements(blocked)-1]].alpha_j2000/15d, $
                        table[blocked[1:n_elements(blocked)-1]].delta_j2000, dist_block
                      if n_elements(blocked) eq 1 then dist_block = 2*setup.min_dist
; can be simplified when the plots are taken out!
                      IF min(dist) LT setup.min_dist or min(dist_block) lt setup.min_dist_block THEN BEGIN
                          blocked = [[blocked],todo[ob]]
                      ENDIF
                      
                      ob++
                      if ob eq n_elements(todo) and $
                        (min(dist) lt setup.min_dist or min(dist_block) lt setup.min_dist_block) then begin
                          wait, 1
                          ob=0l
                          print, 'starting over'
                          goto, loopstart2_bd
                      ENDIF
                      
                  ENDREP UNTIL (min(dist) gt setup.min_dist and min(dist_block) gt setup.min_dist_block) or ob ge n_elements(todo)-1
                  IF min(dist) LT setup.min_dist or min(dist_block) lt setup.min_dist_block THEN CONTINUE
              ENDIF
              ob=ob-1>0
              cur=todo[ob]
              
; perform some kind of STAR classification so B/D is only done for galaxies
; currently, SExtractor is used, but maybe others are more useful
;              if table[cur].class_star gt 0.8 then begin
;                  print, strtrim(cur,2)+' SEEMS TO BE A STAR! trying next object'
;                  table[cur].flag_galfit_bd = -1
;                  fittab[cur].flag_galfit_bd = -1
;                  goto, loopstart_bd
;              ENDIF

; check whether this object has already been done, if so, read in
; result and restart
              ct = 0 
              IF cur LT nbr THEN idx = where(table[cur].frame[0] EQ orgim[*,0], ct)
              IF ct GT 0 THEN BEGIN
                  objnum = round_digit(table[cur].number, 0, /str)
                  obj_file = (outpath_galfit[idx]+orgpre[idx]+objnum+'_bd_'+setup.obj)[0]
                  out_file = (outpath_galfit[idx]+orgpre[idx]+objnum+'_bd_'+setup.galfit_out)[0]
                  sky_file = strarr(nband+1)
                  for q=1,nband do sky_file[q] = (outpath_galfit[idx]+orgpre[idx,q]+objnum+'_'+setup.stamp_pre[q]+'_bd_'+setup.outsky)[0]
;check if file was done successfully or bombed and update table                  
                  IF file_test(obj_file) THEN BEGIN
                      print, obj_file+' found.'
                      print, 'Updating table now! ('+strtrim(cur, 2)+'/'+strtrim(nbr, 1)+')'                      
stop
                      update_table, fittab, table, cur, out_file+'.fits', obj_file, sky_file, nband, setup, /bd
stop
                      IF n_elements(todo) ne 1 then goto, loopstart_bd
                      IF n_elements(todo) eq 1 then goto, loopend_bd
                  ENDIF
              ENDIF

;store position of new object
              bridge_obj[free[0]] = cur
              bridge_pos[*, free[0]] = [table[cur].alpha_j2000, table[cur].delta_j2000]
              table[cur].flag_galfit_bd = 1
              fittab[cur].flag_galfit_bd = 1
              print, '  currently working on No. '+strtrim(n_elements(where(table.flag_galfit_bd ge 1)),2)+' of '+strtrim(n_elements(table.number),2)+'   '
              if keyword_set(plot) then begin
                  plot, table.alpha_J2000,table.delta_J2000, psym=3, ystyle=1, xstyle=1
                  if n_elements(blocked) gt 1 then begin
                      plots, table[blocked[1:n_elements(blocked)-1]].alpha_J2000,table[blocked[1:n_elements(blocked)-1]].delta_J2000, psym=4, col=200, symsize=2
                      for r=1,n_elements(blocked)-1 do tvellipse, setup.min_dist_block/3600., setup.min_dist_block/3600., table[blocked[r]].alpha_J2000,table[blocked[r]].delta_J2000,col=200,/data
                  ENDIF                
                  done=where(table.flag_galfit ge 1, count)
                  if count ge 1 then begin
                      plots, table[done].alpha_J2000,table[done].delta_J2000, psym=1, col=135, thick=2, symsize=0.5
                  endif
                  plots, bridge_pos[0,*], bridge_pos[1,*], psym=1, col=235
                  for q=0,n_elements(bridge_pos[0,*])-1 do tvellipse, setup.min_dist/3600., setup.min_dist/3600., bridge_pos[0,q], bridge_pos[1,q], col=235,/data,thick=2
              ENDIF
              
;find the matching filenames
              idx = where(table[cur].frame[0] EQ orgim[*,0])
;define the file names for the:
;postage stamp parameters
              stamp_param_file = (orgpath_file_no_band[idx,0]+setup.stampfile)[0]
              objnum = round_digit(table[cur].number, 0, /str)
;galfit masks
              mask_file = strarr(nband+1)
              for q=1,nband do mask_file[q] = (outpath_galfit[idx]+outpre[idx,q]+objnum+'_bd_'+setup.stamp_pre[q]+'_'+setup.mask)[0]
;galfit obj file
              obj_file = (outpath_galfit[idx]+orgpre[idx]+objnum+'_bd_'+setup.obj)[0]
;galfit constraint file
              constr_file = (outpath_galfit[idx]+orgpre[idx]+objnum+'_bd_'+setup.constr)[0]
;galfit input file
              im_file = strarr(nband+1)
              for q=1,nband do im_file[q] = (orgpath_pre[idx,q]+objnum+'_'+setup.stamp_pre[q])[0]
              
;galfit output path
              out_file = (outpath_galfit[idx]+orgpre[idx]+objnum+'_bd_'+setup.galfit_out)[0]
;sky summary file
              sky_file = strarr(nband+1)
              for q=1,nband do sky_file[q] = (outpath_galfit[idx]+outpre[idx,q]+objnum+'_'+setup.stamp_pre[q]+'_bd_'+setup.outsky)[0]
              
; choose closest PSF according to RA & DEC and subtract filename from structure 'psf_struct'
; read in chosen_psf into 'psf', filename in chosen_psf_file   
              choose_psf, table[cur].alpha_j2000, table[cur].delta_j2000, $
                psf_struct, table[cur].frame, chosen_psf_file, nband
              
; change seed for random in getsky_loop
;            randxxx=randomu(seed,1)
;            delvarx, randxxx 
              seed=table[cur].number
; create sav file for gala_bridge to read in
              save, cur, orgwht, idx, orgpath, orgpre, setup, chosen_psf_file,$
                sky_file, stamp_param_file, mask_file, im_file, obj_file, $
                constr_file, out_file, fittab, nband, orgpath_pre, outpath_file, $
                outpath_file_no_band, orgpath_file_no_band, outpath_galfit, $
                orgpath_band, orgpath_file, seed,$
                filename=out_file+'.sav'

              IF setup.max_proc GT 1 THEN BEGIN
                  IF keyword_set(logfile) THEN $
                    update_log, logfile, 'Starting new bridge... ('+out_file+')'
; print, 'starting new object at '+systime(0)
                  bridge_arr[free[0]]->execute, 'astrolib'
;               bridge_arr[free[0]]->execute, 'cd,"/home/gems/gala"';§§§§§§§§§§
                  bridge_arr[free[0]]->execute, '.r '+gala_pro
                  bridge_arr[free[0]]->execute, $
                    'gala_bridge, "'+out_file+'.sav", /fit_bd', /nowait
              ENDIF ELSE BEGIN
                  IF keyword_set(logfile) THEN $
                    update_log, logfile, 'Starting next object... ('+out_file+')'
                  cd, orgpath[idx,0]
                  gala_bridge, out_file+'.bd.sav'
                  file_delete, orgpath[idx,0]+'galfit.[0123456789]*', /quiet, $
                    /allow_nonexistent, /noexpand_path
              ENDELSE
;switch to next object
          ENDIF ELSE BEGIN
;all bridges are busy --> wait 
              wait, 1

          ENDELSE
          
loopend_bd:
;stop when all done and no bridge in use any more
      ENDREP UNTIL todo[0] eq -1 AND total(bridge_use) EQ 0
      
;have to read in the last batch of objects
      remain = where(bridge_obj ge 0, ct)
      IF ct GT 0 THEN BEGIN
         FOR i=0, ct-1 DO BEGIN
            idx = where(table[bridge_obj[remain[i]]].frame[0] EQ orgim[*,0])
            objnum = round_digit(table[bridge_obj[remain[i]]].number, 0, /str)
            obj_file = (outpath_galfit[idx]+orgpre[idx,1]+objnum+'_bd_'+setup.obj)[0]
            out_file = (outpath_galfit[idx]+orgpre[idx,1]+objnum+'_bd_'+setup.galfit_out)[0]
            sky_file = strarr(nband+1)
            for q=1,nband do sky_file[q] = (outpath_galfit[idx]+orgpre[idx,q]+objnum+'_'+setup.stamp_pre[q]+'_bd_'+setup.outsky)[0]
           
            if keyword_set(plot) then begin
                plots, table[bridge_obj[remain[i]]].alpha_J2000,table[bridge_obj[remain[i]]].delta_J2000, psym=1, col=135, thick=2, symsize=2
                tvellipse, setup.min_dist/3600., setup.min_dist/3600., $
                  table[bridge_obj[remain[i]]].alpha_J2000,table[bridge_obj[remain[i]]].delta_J2000, col=0,/data
            ENDIF
            bridge_obj[remain[i]] = -1
            bridge_pos[*, remain[i]]= [!values.F_NAN, !values.F_NAN]            
            
;check if file was done successfully or bombed
; if succesfully, fill fitting parameters into fittab
stop
            update_table, fittab, table, bridge_obj[remain[i]], out_file, obj_file, sky_file, nband, setup, /bd
stop
;print, 'out file exists -- fittab updated'
;else output file does not exist --> bombed
        ENDFOR
    ENDIF
ENDIF

jump_over_this:
;==============================================================================
;read in sextractor table, combine with galfit results, write out
;combined fits table
; has to be adapted to also read B/D!
   IF setup.docombine THEN BEGIN
      tab = read_sex_table(setup.outdir+setup.sexcomb, $
                           outpath_file[0,0]+setup.outparam, $
                           add_col = ['TILE', '" "'])
      add_tag, tab, 'flag_galfit', 0, tab2
      tab=tab2
      delvarx, tab2

      ntab = n_elements(tab)
      out = read_sex_param(outpath_file[0,0]+setup.outparam, ntab, $
                           add_column = addcol)
      struct_assign, tab, out

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

      print,' '
      FOR i=0ul, ntab-1 DO BEGIN
         statusline, ' reading result '+strtrim(i+1,2)+' of '+strtrim(ntab,2)
         objnum = round_digit(tab[i].number, 0, /str)

         idx = where(tab[i].tile EQ orgim[*,0])
         out_file = (outpath_galfit[idx]+orgpre[idx,0]+objnum+'_'+ $
                     setup.galfit_out)[0]+'.fits'

         obj_file = (outpath_galfit[idx]+orgpre[idx,1]+objnum+'_'+setup.obj)[0]
         sky_file = strarr(nband+1)
         for q=1,nband do sky_file[q] = (outpath_galfit[idx]+orgpre[idx,q]+objnum+'_'+setup.stamp_pre[q]+'_'+setup.outsky)[0]

stop
; read in single sersic again
         update_table, out, tab, i, out_file, obj_file, sky_file, nband, setup, /final
stop
; read in B/D again
         update_table, out, tab, i, out_file, obj_file, sky_file, nband, setup, /final, /bd
stop
         out[i].org_image_band = orgim[idx[0],1:nband]

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

print, 'Start: '+start
print, 'End  : '+systime(0)
END

