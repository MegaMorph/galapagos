PRO prepare_galfit_bd, setup, objects, files, corner, table0, obj_file, im_file, $
                    constr_file, mask_file, psf_file, out_file, sky_file, $
                    conv_box, zero_pt, plate_scl, num_contrib, frame_contrib, $
                    current, out_cat, out_param, out_stamps, conmaxre, $
                    conminm, conmaxm, setup_version, nband, $
                    outpre, bd_fit = bd_fit
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
openw, 11, constr_file
if keyword_set(bd_fit) then begin
    printf, 11, '# Component/    parameter   constraint  Comment'
    printf, 11, '# operation                  values'
;    FOR j=2ul, n_elements(objects)+1 DO BEGIN
;        lo = strtrim(setup.conminn, 2)
;        hi = strtrim(setup.conmaxn, 2)
;        printf, 11, j, ' n '+lo+' to '+hi
;        printf, 11, j, ' re 0.3 to '+strtrim(conmaxre, 2)
;        printf, 11, j, ' q 0.0001  to 1.'
;        printf, 11, j, ' mag '+strtrim(conminm, 2)+' '+strtrim(conmaxm, 2)
;        printf, 11, j, ' mag 0 to 40'
;        printf, 11, j, ' pa -360 to 360'
;        printf, 11, j, ' x '+strtrim(-xmax)+' '+strtrim(xmax)
;        printf, 11, j, ' y '+strtrim(-ymax)+' '+strtrim(ymax)
;    ENDFOR
ENDIF

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
    SKY_po=SKY_po+strtrim(round_digit(sky,3,/string,/L64),2)
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
    
; for some reason this is needed to find read_sersic_results as a
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

openr, 11, constr_file
line = ''
WHILE NOT eof(11) DO readf, 11, line
close, 11
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
        i_fit = where(table.number EQ num_contrib[i] AND $
                      table.org_image EQ frame_contrib[i], ct)
        IF ct GT 0 THEN BEGIN
            IF table[i_fit].re_galfit GE 0 THEN BEGIN
                
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
                par.mag_galfit = table[i_fit].mag_galfit
                par.mag_galfit_band = table[i_fit].mag_galfit_band
                par.mag_galfit_cheb = table[i_fit].mag_galfit_cheb
                par.re_galfit = table[i_fit].re_galfit
                par.re_galfit_band = table[i_fit].re_galfit_band
                par.re_galfit_cheb = table[i_fit].re_galfit_cheb
                par.n_galfit = table[i_fit].n_galfit
                par.n_galfit_band = table[i_fit].n_galfit_band
                par.n_galfit_cheb = table[i_fit].n_galfit_cheb
                par.q_galfit = table[i_fit].q_galfit
                par.q_galfit_band = table[i_fit].q_galfit_band
                par.q_galfit_cheb = table[i_fit].q_galfit_cheb
                par.pa_galfit = table[i_fit].pa_galfit
                par.pa_galfit_band = table[i_fit].pa_galfit_band
                par.pa_galfit_cheb = table[i_fit].pa_galfit_cheb
                
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
        openu, 11, constr_file, /append
        printf, 11, ctr, ' n '+lo+' to '+hi
        printf, 11, ctr, ' re 0.3 to '+strtrim(conmaxre, 2)
        printf, 11, ctr, ' q 0.0001  to 1.'
        printf, 11, ctr, ' mag '+strtrim(conminm, 2)+' '+strtrim(conmaxm, 2)
        printf, 11, ctr, ' mag 0 to 40'
        printf, 11, ctr, ' pa -360 to 360'
        printf, 11, ctr, ' x '+strtrim(-xmax)+' '+strtrim(xmax)
        printf, 11, ctr, ' y '+strtrim(-ymax)+' '+strtrim(ymax)
        close, 11
    ENDIF
    ctr += 1
ENDFOR
finish:

; close, constraint file
close, 11
END
