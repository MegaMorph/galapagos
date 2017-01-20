@galapagos.pro
PRO derive_colour_offsets, setup_file, image_number, hot=hot, image=image, weight=weight
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
;
; can be started with manually defining the image, then 'image' and
; 'weight' have to be the arrays for all bands

;read in the setup file
  read_setup, setup_file, setup

;read input files into arrays
  save_folder = setup.outdir+'colour_offsets/'
  IF file_test(save_folder) eq 0 THEN spawn, 'mkdir -p '+save_folder
  read_image_files, setup, save_folder
  images = setup.images
  weights = setup.weights
  nband = setup.nband
  sexsetup = setup.cold
  post= 'cold'
  if keyword_set(hot) then sexsetup = setup.hot
  if keyword_set(hot) then post='hot'

  if keyword_set(image) then begin
     images = strarr(1,nband+1)
     weights = strarr(1,nband+1)
     images[0,*] = image
     if not keyword_set(weight) then begin
       print, 'If defining images by hand, both images and weights have to be defined'
       stop
     endif
     weights[0,*] = weight
     image_number = 0
  ENDIF

; run sextractor over all images

; detection in first band, measurements in each band, including first
  cat_name = strarr(nband+1)
  checkimage_name = strarr(nband+1)
  weight_type = strarr(nband+1)
  weight_type[*] ='MAP_WEIGHT' 
  IF setup.sex_rms EQ 1 THEN weight_type[0] = 'MAP_RMS'
  FOR b=0,nband DO BEGIN
     cat_name[b] = save_folder +'/derive_offset_'+strtrim(image_number,2)+'_band_'+strtrim(b,2)+'_cat_'+post+'.fits'
     checkimage_name[b] = save_folder +'/derive_offset_'+strtrim(image_number,2)+'_band_'+strtrim(b,2)+'_check'+post+'.fits'
     sexcommand = setup.sexexe+' '+images[image_number,0] + ',' + images[image_number,b]+' -c '+sexsetup+ $
                  ' -CATALOG_NAME '+cat_name[b]+' -CATALOG_TYPE FITS_1.0' + $
                  ' -PARAMETERS_NAME '+setup.sexout+ $
                  ' -WEIGHT_IMAGE '+weights[image_number,0] + ',' + weights[image_number,b]+ $
                  ' -WEIGHT_TYPE '+weight_type[0]+','+weight_type[b]+' -MAG_ZEROPOINT '+strtrim(setup.zp[b],2)+ $
                  ' -CHECKIMAGE_TYPE '+setup.chktype+' -CHECKIMAGE_NAME '+ checkimage_name[b]

     IF file_test(cat_name[b]) EQ 0 THEN BEGIN
        PRINT, ' '
        print, 'sextracting '+images[image_number,0] + ',' + images[image_number,b]
        print, ' using setup file '+sexsetup
        print, ' -> catalogue '+ cat_name[b]
;print, sexcommand
        spawn, sexcommand
     ENDIF ELSE print, 'catalogue '+cat_name[b]+' already exists. Skipping'
     
  ENDFOR

  offset = fltarr(nband+1)+0.
  med = fltarr(nband+1)+0.
  offset_acc = fltarr(nband+1)+0.
  sigma = fltarr(nband+1)+0.
  rej = intarr(nband+1)+0
 
  refcat = mrdfits(cat_name[0],1,/silent)
  

; SAMPLE SELECTION 1
; define sample by simply checking each band individually for
; magnitudes (data) and only using the ones where all bands show good values
  sample = intarr(n_elements(refcat.mag_best))+1
  print, strtrim(total(sample),2)+' objects in catalogue'
; filter nonsense magnitudes
  wh = where(refcat.mag_best gt 30, count)
  if count gt 0 then sample[wh] = 0
  print, strtrim(total(sample),2)+' objects left after cleaning for magnitudes in reference catalogue'
  FOR b=1,nband DO BEGIN
     cat = mrdfits(cat_name[b],1,/silent)
; filter nonsense magnitudes
     wh = where(cat.mag_best gt 30, count)
     if count gt 0 then sample[wh] = 0
     print, strtrim(total(fix(sample)),2)+' objects left after combining with band '+strtrim(b,2)
  ENDFOR
  sample = where(sample eq 1)


  print, 'you should be able to use these offsets: zero_point, image, median, mean, accuracy, sigma, #objects after sigma clipping'
  print, 'Given the double peak profile, the resistant mean values are probably dangerous, I recommend median instead'
  FOR b=1,nband DO BEGIN
     cat = mrdfits(cat_name[b],1,/silent)
;     wh = where(cat.mag_best lt 50)
     plothist, cat[sample].mag_best-refcat[sample].mag_best,xrange=[-1,4],bin=0.05
;stop
     wait, 2
     resistant_mean, cat[sample].mag_best-refcat[sample].mag_best, 5., m, s, r
     med[b] = median(cat[sample].mag_best-refcat[sample].mag_best)
     offset[b] = m
     offset_acc[b] = s
     sigma[b] = s
     rej[b] = r
     sigma[b] *= sqrt(n_elements(sample)-1-rej[b])
     print, strtrim(setup.zp[b],2), ' ', images[image_number,b], '  ', med[b], '  ', offset[b], '  ', offset_acc[b], '  ', sigma[b], '  ', strtrim(n_elements(sample)-rej[b],2)
  ENDFOR


END
