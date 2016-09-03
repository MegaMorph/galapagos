@galapagos.pro
PRO derive_colour_offsets, setup_file, image_number
; this code helps to derive the numbers to be put into one of the
; setup files which defines the 'typical colours' of objects, so
; Galfit can start good starting parameters for magnitudes.
;
; The file starts from the galapagos_setup files itself, so everything
; should be created before. Just leave those number at '0' if
; you don't know them yet.
; This script then runs sextractor on the FIRST IMAGE in each band in
; dual image mode (detection on detection image, measurement on the
; other bands), correlates the objects and prints out typical colour 
; values and error bars on those.
; Simply put the values derived into the galapagos setup file and
; start galapagos
;
; NOTE: image_number defines the number of the images in the
; setup_files (in line counting) that are used for this test. This has
; to be an area where all (!) filters contain data, e.g. all images
; are filled. The bigger the images, the better the statistics

;read in the setup file
  read_setup, setup_file, setup

;read input files into arrays
  save_folder = setup.outdir+'colour_offsets/'
  IF file_test(save_folder) eq 0 THEN spawn, 'mkdir -p '+save_folder
  read_image_files, setup, save_folder
  images = setup.images
  weights = setup.weights
  nband = setup.nband

; run sextractor over all images
; detection in first band, measurements in each band, including first
  IF setup.sex_rms EQ 0 THEN weight_type = 'MAP_WEIGHT'
  IF setup.sex_rms EQ 1 THEN weight_type = 'MAP_RMS'
  cat_name = strarr(nband+1)
  checkimage_name = strarr(nband+1)
  FOR b=0,nband DO BEGIN
     cat_name[b] = save_folder +'/derive_offset_'+strtrim(image_number,2)+'_band_'+strtrim(b,2)+'_cat.fits'
     checkimage_name[b] = save_folder +'/derive_offset_'+strtrim(image_number,2)+'_band_'+strtrim(b,2)+'_check.fits'
     sexcommand = setup.sexexe+' '+images[image_number,0] + ',' + images[image_number,b]+' -c '+setup.hot+ $
                  ' -CATALOG_NAME '+cat_name[b]+' -CATALOG_TYPE FITS_1.0' + $
                  ' -PARAMETERS_NAME '+setup.sexout+ $
                  ' -WEIGHT_IMAGE '+weights[image_number,0] + ',' + weights[image_number,b]+ $
                  ' -WEIGHT_TYPE '+weight_type+','+weight_type+' -MAG_ZEROPOINT '+strtrim(setup.zp[b],2)+ $
                  ' -CHECKIMAGE_TYPE '+setup.chktype+' -CHECKIMAGE_NAME '+ checkimage_name[b]
     
     IF file_test(cat_name[b]) EQ 0 THEN BEGIN
        PRINT, ' '
        print, 'sextracting '+images[image_number,0] + ',' + images[image_number,b]
        print, ' using setup file '+setup.cold
        print, ' -> catalogue '+ cat_name[b]
        spawn, sexcommand
     ENDIF ELSE print, 'catalogue '+cat_name[b]+' already exists. Skipping'
     
  ENDFOR

  offset = fltarr(nband+1)+0.
  sigma = fltarr(nband+1)+0.
  rej = intarr(nband+1)+0
 
  refcat = mrdfits(cat_name[0],1,/silent)
  
  print, 'you should be able to use these offsets: zero_point, image, mean, sigma, #objects'
  FOR b=1,nband DO BEGIN
     cat = mrdfits(cat_name[b],1,/silent)
     resistant_mean, cat.mag_best-refcat.mag_best, 3., m, s, r
     offset[b] = m
     sigma[b] = s
     rej[b] = r
     sigma[b] *= sqrt(n_elements(cat.mag_best)-1-rej[b])
     print, strtrim(setup.zp[b],2), ' ', images[image_number,b], '  ', offset[b], '  ', sigma[b], '  ', strtrim(n_elements(cat.mag_best)-rej[b],2)

  ENDFOR

stop


END
